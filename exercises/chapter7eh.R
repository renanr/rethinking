# 7H1

library(rethinking)
data(tulips)

t = tulips

# Dummy variable approach from chapter 5:
# d$clade.OWM <- ifelse( d$clade=="Old World Monkey" , 1 , 0 )
# d$clade.S <- ifelse( d$clade=="Strepsirrhine" , 1 , 0 )
# 
# Index variable approach from chapter 5:
# ( d$clade_id <- coerce_index(d$clade) )
# m5.16_alt <- map(
#                  alist(
#                    kcal.per.g ~ dnorm( mu , sigma ) ,
#                    mu <- a[clade_id] ,
#                    a[clade_id] ~ dnorm( 0.6 , 10 ) ,
#                    sigma ~ dunif( 0 , 10 )
#                  ) ,
#                  data=d )
# precis( m5.16_alt , depth=2 )

# if I adopted the dummy variable approach, I would have different parameters
# being multiplied by 1 or 0 and added. That is equivalent to allowing the intercept
# to change per index, duh!

t$bed_id = coerce_index(t$bed)
t$water = (t$water - mean(t$water))
t$shade = (t$shade - mean(t$shade))

m7h1 = map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a[bed_id] + bw * water + bs * shade + bws * water * shade,
    a[bed_id] ~ dnorm(mean(blooms), sd(blooms)),
    bw ~ dnorm(0, 10),
    bs ~ dnorm(0, 10),
    bws ~ dnorm(0, 10),
    sigma ~ dunif(0, 100)),
  data=t
)

# 7H2

m7h2 = map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bw * water + bs * shade + bws * water * shade,
    a ~ dnorm(mean(blooms), sd(blooms)),
    bw ~ dnorm(0, 10),
    bs ~ dnorm(0, 10),
    bws ~ dnorm(0, 10),
    sigma ~ dunif(0, 100)),
  data=t
)

compare(m7h1, m7h2)
#       WAIC pWAIC dWAIC weight   SE dSE
# m7h2 317.4   3.6     0   0.73 9.34  NA
# m7h1 319.4   6.3     2   0.27 9.85 3.1
# From this comparison, I infer that bed is not a relevant predictor. It does not
# influence the outcomes, but "hurts" by facilitating overfitting.

precis(m7h1, depth=2)
#         Mean StdDev   5.5%  94.5%
# a[1]   99.10  22.94  62.44 135.75
# a[2]  141.72  22.93 105.08 178.37
# a[3]  146.21  22.93 109.56 182.85
# Except for the first one, they're very close. But even the first one has a huge overlap.

# 7H3

library(rethinking)
data(rugged)
r = rugged

r = r[complete.cases(r$rgdppc_2000), ]
r$log_gdp = log(r$rgdppc_2000)

m7h3wi <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bAR*rugged*cont_africa + bA*cont_africa,
    a ~ dnorm( 8 , 100 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bAR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ), data=r)

m7h3wo <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bAR*rugged*cont_africa + bA*cont_africa,
    a ~ dnorm( 8 , 100 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bAR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ), data=r[r$country != 'Seychelles', ])

precis(m7h3wi)
precis(m7h3wo)
# (a)
# > precis(m7h3wi)
#        Mean StdDev  5.5% 94.5%
# a      9.18   0.14  8.97  9.40
# bA    -1.85   0.22 -2.20 -1.50
# bR    -0.18   0.08 -0.31 -0.06
# bAR    0.35   0.13  0.14  0.55
# sigma  0.93   0.05  0.85  1.01
# > precis(m7h3wo)
#        Mean StdDev  5.5% 94.5%
# a      9.19   0.14  8.97  9.40
# bA    -1.78   0.22 -2.13 -1.43
# bR    -0.19   0.08 -0.31 -0.07
# bAR    0.25   0.14  0.04  0.47
# sigma  0.93   0.05  0.85  1.01

# (b)
rugged.seq <- seq(from=-1,to=8,by=0.25)
mu.Africa <- link( m7h3wi , data=data.frame(cont_africa=1,rugged=rugged.seq) )
mu.Africa.mean <- apply( mu.Africa , 2 , mean )
mu.Africa.PI <- apply( mu.Africa , 2 , PI , prob=0.97 )
mu.NotAfrica <- link( m7h3wi , data=data.frame(cont_africa=0,rugged=rugged.seq) )
mu.NotAfrica.mean <- apply( mu.NotAfrica , 2 , mean )
mu.NotAfrica.PI <- apply( mu.NotAfrica , 2 , PI , prob=0.97 )
d.A1 <- r[r$cont_africa==1,]
plot( log(rgdppc_2000) ~ rugged , data=d.A1 ,
      col=rangi2 , ylab="log GDP year 2000" ,
      xlab="Terrain Ruggedness Index" )
mtext( "African nations" , 3 )
lines( rugged.seq , mu.Africa.mean , col=rangi2 )
shade( mu.Africa.PI , rugged.seq , col=col.alpha(rangi2,0.3) )
d.A0 <- r[r$cont_africa==0,]
plot( log(rgdppc_2000) ~ rugged , data=d.A0 ,
      col="black" , ylab="log GDP year 2000" ,
      xlab="Terrain Ruggedness Index" )
mtext( "Non-African nations" , 3 )
lines( rugged.seq , mu.NotAfrica.mean )
shade( mu.NotAfrica.PI , rugged.seq )

rugged.seq <- seq(from=-1,to=8,by=0.25)
mu.Africa <- link( m7h3wo , data=data.frame(cont_africa=1,rugged=rugged.seq) )
mu.Africa.mean <- apply( mu.Africa , 2 , mean )
mu.Africa.PI <- apply( mu.Africa , 2 , PI , prob=0.97 )
mu.NotAfrica <- link( m7h3wo , data=data.frame(cont_africa=0,rugged=rugged.seq) )
mu.NotAfrica.mean <- apply( mu.NotAfrica , 2 , mean )
mu.NotAfrica.PI <- apply( mu.NotAfrica , 2 , PI , prob=0.97 )
d.A1 <- r[r$cont_africa==1,]
plot( log(rgdppc_2000) ~ rugged , data=d.A1 ,
      col=rangi2 , ylab="log GDP year 2000" ,
      xlab="Terrain Ruggedness Index" )
mtext( "African nations" , 3 )
lines( rugged.seq , mu.Africa.mean , col=rangi2 )
shade( mu.Africa.PI , rugged.seq , col=col.alpha(rangi2,0.3) )
d.A0 <- r[r$cont_africa==0,]
plot( log(rgdppc_2000) ~ rugged , data=d.A0 ,
      col="black" , ylab="log GDP year 2000" ,
      xlab="Terrain Ruggedness Index" )
mtext( "Non-African nations" , 3 )
lines( rugged.seq , mu.NotAfrica.mean )
shade( mu.NotAfrica.PI , rugged.seq )

# wow, the parameters apparently didn't change that much, but the plots sure did.
# now it looks like the slope is much closer to zero than before! (but still it isn't negative
# as in Non-Africa)

# (c)
m7h3.1 = map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged,
    a ~ dnorm( 8 , 100 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
    ), data=r[r$country != 'Seychelles', ])

m7h3.2 = map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa,
    a ~ dnorm( 8 , 100 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ), data=r[r$country != 'Seychelles', ])

m7h3.3 = map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bAR*rugged*cont_africa + bA*cont_africa,
    a ~ dnorm( 8 , 100 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bAR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ), data=r[r$country != 'Seychelles', ])

rugged.seq <- seq(from=-1,to=8,by=0.25)
mu.Africa = ensemble(m7h3.1, m7h3.2, m7h3.3, data=data.frame(cont_africa=1,rugged=rugged.seq))$link
mu.Africa.mean <- apply( mu.Africa , 2 , mean )
mu.Africa.PI <- apply( mu.Africa , 2 , PI , prob=0.97 )
mu.NotAfrica = ensemble(m7h3.1, m7h3.2, m7h3.3, data=data.frame(cont_africa=0,rugged=rugged.seq))$link
mu.NotAfrica.mean <- apply( mu.NotAfrica , 2 , mean )
mu.NotAfrica.PI <- apply( mu.NotAfrica , 2 , PI , prob=0.97 )
d.A1 <- r[r$cont_africa==1,]
plot( log(rgdppc_2000) ~ rugged , data=d.A1 ,
      col=rangi2 , ylab="log GDP year 2000" ,
      xlab="Terrain Ruggedness Index" )
mtext( "African nations" , 3 )
lines( rugged.seq , mu.Africa.mean , col=rangi2 )
shade( mu.Africa.PI , rugged.seq , col=col.alpha(rangi2,0.3) )
d.A0 <- r[r$cont_africa==0,]
plot( log(rgdppc_2000) ~ rugged , data=d.A0 ,
      col="black" , ylab="log GDP year 2000" ,
      xlab="Terrain Ruggedness Index" )
mtext( "Non-African nations" , 3 )
lines( rugged.seq , mu.NotAfrica.mean )
shade( mu.NotAfrica.PI , rugged.seq )

# The comparison can be seen in the image 7h3comparison.png.
# As expected, no further changes occurred for non-African nations.
# But the slope in African nations further approached zero! This is because a relatively big
# weight was given to model 2, which does not account for interactions (has the same slope for both cases).

# 7H4

library(rethinking)
data(nettle)
n = nettle
n$lang.per.kcap = n$num.lang / n$k.pop

# poor ecology leads to larger networks with less different languages?

n$log_lpc = log(n$lang.per.kcap)
n$log_area = log(n$area)

# if I think I should WAIC: please, go ahead
# argue for the priors
# how should I plot predictions?
# honestly try to evaluate the main effects of mean.growing.season and sd.growing.season
# as well as their two-way interaction (if not sure which approach, try several)

# (a) Evaluate the hypothesis that language diversity, as measured by log(lang.per.cap), 
# is positively associated with the average length of the growing season, mean.growing.season. Consider 
# log(area) in your regression(s) as a covariate (not an interaction). Interpret your results.

m7h4.1 = map(
  alist(
    log_lpc ~ dnorm(mu, sigma),
    mu <- a + bM * mean.growing.season + bA * log_area,
    a ~ dnorm(-3.5, 10),
    bM ~ dnorm(0, .5), # I will use strongly regularizing priors because of my skepticism
    bA ~ dnorm(0, .5),
    sigma ~ dunif(0, 14)
  ), data = n
)
precis(m7h4.1)

cf_mgs = seq(min(n$mean.growing.season), max(n$mean.growing.season), length.out=100)
linked = link(m7h4.1, data=data.frame(mean.growing.season=cf_mgs, log_area=mean(n$log_area)), n=1e3)
mu = apply(linked, 2, mean)
pi = apply(linked, 2, PI, .97)
plot(n$log_lpc ~ n$mean.growing.season, col=rangi2)
lines(cf_mgs, mu)
shade(pi, cf_mgs)

# there really seems to be a weak positive correlation. It seems weak but it seems there

# (b) Now evaluate the hypothesis that language diversity is negatively associated with the standard deviation 
# of length of growing season, sd.growing.season. This hypothesis follows from uncertainty in harvest favoring 
# social insurance through larger social networks and therefore fewer languages. Again, consider log(area) as 
# a covariate (not an interaction). Interpret your results. 

m7h4.2 = map(
  alist(
    log_lpc ~ dnorm(mu, sigma),
    mu <- a + bS * sd.growing.season + bA * log_area,
    a ~ dnorm(-3.5, 10),
    bS ~ dnorm(0, .5), # I will use strongly regularizing priors because of my skepticism
    bA ~ dnorm(0, .5),
    sigma ~ dunif(0, 14)
  ), data = n
)
precis(m7h4.2)

cf_dgs = seq(min(n$sd.growing.season), max(n$sd.growing.season), length.out=100)
linked = link(m7h4.2, data=data.frame(sd.growing.season=cf_dgs, log_area=mean(n$log_area)), n=1e3)
mu = apply(linked, 2, mean)
pi = apply(linked, 2, PI, .97)
plot(n$log_lpc ~ n$sd.growing.season, col=rangi2)
lines(cf_dgs, mu)
shade(pi, cf_dgs)

# even (somewhat-)strongly regularizing priors left a huge probability for a negative association

# (c) Finally, evaluate the hypothesis that mean.growing.season and sd.growing.season interact to 
# synergistically reduce language diversity. The idea is that, in nations with longer average growing seasons, high 
# variance makes storage and redistribution even more important than it would be otherwise. That way, people can 
# cooperate to preserve and protect windfalls to be used during the droughts. These forces in turn may lead to 
# greater social integration and fewer languages. 

m7h4.3 = map(
  alist(
    log_lpc ~ dnorm(mu, sigma),
    mu <- a + bM * mean.growing.season + bS * sd.growing.season + bA * log_area + bMS * mean.growing.season * sd.growing.season,
    a ~ dnorm(-3.5, 10),
    bS ~ dnorm(0, .5), # I will use strongly regularizing priors because of my skepticism
    bM ~ dnorm(0, .5),
    bMS ~ dnorm(0, .5),
    bA ~ dnorm(0, .5),
    sigma ~ dunif(0, 14)
  ), data = n
)
precis(m7h4.3)

par(mfrow=c(1,3))
cf_dgs = c(min(n$sd.growing.season), mean(n$sd.growing.season), max(n$sd.growing.season))
cf_mgs = seq(min(n$mean.growing.season), max(n$mean.growing.season), length.out=100)
for ( dgs in cf_dgs ) {
  plot(n$log_lpc ~ n$mean.growing.season)
  mu <- link( m7h4.3 , data=data.frame(sd.growing.season=dgs, mean.growing.season=cf_mgs, log_area=mean(n$log_area))) 
  mu.mean <- apply( mu , 2 , mean )
  mu.PI <- apply( mu , 2 , PI , prob=0.97 )
  lines( cf_mgs , mu.mean )
  lines( cf_mgs , mu.PI[1,] , lty=2 )
  lines( cf_mgs , mu.PI[2,] , lty=2 )
}

# amazing how the relation between log-lpc and mgs invert when dgs gets big enough
