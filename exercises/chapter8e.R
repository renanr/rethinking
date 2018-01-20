######################
# Easy
# 8E1
# Number (3): the proposal distribution must be symmetric, i.e. the probability of going from A to B must be the same as from going from B to A.

# 8E2
# It achieves the extra efficiency by calculating the posterior distribution of an individual parameter (using conjugate pairs for prior and likelihood).
# The limitation is being tied to the choice of conjugate pairs. It also doesn't scale efficiently to really complex models.

# 8E3
# HMC requires continuous parameters.

# 8E4
# The effective number of samples, n_eff, as calculated by Stan, accounts for autocorrelation between samples, and so n_eff <= #samples.

# 8E5
# 1.0, more than that can be a sign of non-convergence.

# 8E6
# Well-mixing, stationarity.

######################
# Medium
# 8M1
library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]
dd.trim <- dd[ , c("log_gdp","rugged","cont_africa") ]
str(dd.trim)

m8.1stan <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2)
  ), data=dd.trim )

m8.1stan.M1.1 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dunif(0,2)
  ), data=dd.trim )

m8.1stan.M1.2 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dexp(1)
  ), data=dd.trim )

post1 = extract.samples(m8.1stan)
post2 = extract.samples(m8.1stan.M1.1)
post3 = extract.samples(m8.1stan.M1.2)
# rethinking::plotpost(post1)
# rethinking::plotpost(post2)
# rethinking::plotpost(post3)

for (i in 1:5) {
  dens(data.frame(cauchy=post1[[i]], unif=post2[[i]], exp=post3[[i]]))
}

ggplot() +
  geom_density(aes(post1[[5]], colour='cauchy')) +
  geom_density(aes(post2[[5]], colour='unif')) +
  geom_density(aes(post3[[5]], colour='exp'))

# They do! Especially in other parameters (!)
# For sigma, `exp` is the most uncertain, producing thicker tails, and `unif` produces the narrowest
# posterior among the three of them (except at the peak, where it is `cauchy`).

# 8M2
m8.2cauchy1 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,200)
  ), data=dd.trim )

m8.2cauchy2 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,20)
  ), data=dd.trim )

m8.2cauchy3 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,.02)
  ), data=dd.trim )

post211 = extract.samples(m8.2cauchy1)
post212 = extract.samples(m8.2cauchy2)
post213 = extract.samples(m8.2cauchy3)

ggplot() +
  geom_density(aes(post211[[5]], colour='200')) +
  geom_density(aes(post212[[5]], colour='20')) +
  geom_density(aes(post213[[5]], colour='.02'))

m8.2exp1 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dexp(1)
  ), data=dd.trim )

m8.2exp2 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dexp(.25)
  ), data=dd.trim )

m8.2exp3 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dexp(.0625)
  ), data=dd.trim )

post221 = extract.samples(m8.2exp1)
post222 = extract.samples(m8.2exp2)
post223 = extract.samples(m8.2exp3)

ggplot() +
  geom_density(aes(post221[[5]], colour='1')) +
  geom_density(aes(post222[[5]], colour='.25')) +
  geom_density(aes(post223[[5]], colour='.0625'))

# 8M3: CHANGE THE MODEL, THIS IS RANDOM SHIT
y <- rnorm( 100 , mean=0 , sd=1 )

m8e3.1 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a1 + a2 ,
    a1 ~ dnorm( 0 , 10 ) ,
    a2 ~ dnorm( 0 , 10 ) ,
    sigma ~ dcauchy( 0 , 1 )
  ),
  data=list(y=y) , start=list(a1=0,a2=0,sigma=1) , chains=2 , iter=4000 , warmup=1 )

m8e3.2 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a1 + a2 ,
    a1 ~ dnorm( 0 , 10 ) ,
    a2 ~ dnorm( 0 , 10 ) ,
    sigma ~ dcauchy( 0 , 1 )
  ),
  data=list(y=y) , start=list(a1=0,a2=0,sigma=1) , chains=2 , iter=4000 , warmup=10 )

m8e3.3 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a1 + a2 ,
    a1 ~ dnorm( 0 , 10 ) ,
    a2 ~ dnorm( 0 , 10 ) ,
    sigma ~ dcauchy( 0 , 1 )
  ),
  data=list(y=y) , start=list(a1=0,a2=0,sigma=1) , chains=2 , iter=4000 , warmup=100 )

plot(m8e3.1)
plot(m8e3.2)
plot(m8e3.3)
# 10 warmup samples was already enough. 100 doesn't take much longer and is safer!

######################
# Hard
# 8H1

mp <- map2stan(
  alist(
    a ~ dnorm(0,1),
    b ~ dcauchy(0,1)
  ),
  data=list(y=1),
  start=list(a=0,b=0),
  iter=1e4, warmup=100 , WAIC=FALSE )
plot(mp)
# The cauchy distribution has a high "spread", allowing overwhelmingly high (or low) values to appear
# often.

# 8H2
# Models about divorce rates: m5.1, m5.2 and m5.3 using stan. `compare` with WAIC and explain results.

library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
d$Marriage.s <- (d$Marriage-mean(d$Marriage))/
  sd(d$Marriage)
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage-mean(d$MedianAgeMarriage))/
  sd(d$MedianAgeMarriage)
dtrim = d[, c('MedianAgeMarriage.s', 'Marriage.s', 'Divorce')]

m5.1 <- map2stan(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bA * MedianAgeMarriage.s ,
    a ~ dnorm( 10 , 10 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) , data = dtrim )

m5.2 <- map2stan(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bR * Marriage.s ,
    a ~ dnorm( 10 , 10 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) , data = dtrim )

m5.3 <- map2stan(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s ,
    a ~ dnorm( 10 , 10 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ),
  data = dtrim )

compare(m5.1, m5.2, m5.3)
# Marriage.s is a shitty predictor. It's just fucking noise, drop that shit already.

# 8H3
N <- 100
height <- rnorm(N,10,2)
leg_prop <- runif(N,0.4,0.5)
leg_left <- leg_prop*height + rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height + rnorm( N , 0 , 0.02 )
d <- data.frame(height,leg_left,leg_right)

m5.8s <- map2stan(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) ,
    sigma ~ dcauchy( 0 , 1 )
  ),
  data=d, chains=4, start=list(a=10,bl=0,br=0,sigma=1) )

m5.8s2 <- map2stan(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) & T[0,] ,
    sigma ~ dcauchy( 0 , 1 )
  ),
  data=d, chains=4, start=list(a=10,bl=0,br=0,sigma=1) )

weak = extract.samples(m5.8s)
strong = extract.samples(m5.8s2)

vai = function(i) {ggplot() + geom_density(aes(weak[[i]], colour='weak')) + geom_density(aes(strong[[i]], colour='strong')) + labs(x='x_lab', y='y_lab', colour='col_lab', title='title', subtitle='subtitle') + theme_solarized_2(light = FALSE) + scale_colour_solarized("blue")}

# The prior restricts negative values for `br` and this is also reflected on a restriction on high values of `lb`
# (the 'weak' distribution has a considerable mass for values of `bl` above 2.5, but not the 'strong' distribution)
# As both values are highly correlated, producing an "undetermined" system, one parameter can be chosen "freely" and the
# other correspondingly to get the final value. If we restrict one, this restriction is transmitted since the sum must remain
# the same. I.E.: b1 + b2 = a, if b1 >= 0, b2 <= a.

# 8H4

compare(m5.8s, m5.8s2)
# The unrestricted has more effective parameters, because they are unrestricted (?)

# 8H5

num_weeks <- 1e3
positions <- rep(0,num_weeks)
populations = seq(10, 1) # could be `number_islands` instead of 10, but yeah fuck it
current <- 10
for ( i in 1:num_weeks ) {
  positions[i] <- current
  proposal <- current + sample( c(-1,1) , size=1 )
  if ( proposal < 1 ) proposal <- 10
  if ( proposal > 10 ) proposal <- 1
  # move?
  prob_move <- populations[proposal]/populations[current]
  current <- ifelse( runif(1) < prob_move , proposal , current )
}

# 8H6

num_tosses <- 1e4
results <- rep(0,num_tosses)
densities = c(3, 7) # could be `number_islands` instead of 10, but yeah fuck it
current <- 2
for ( i in 1:num_tosses ) {
  results[i] <- current
  proposal <- current + sample( c(-1,1) , size=1 )
  if ( proposal < 1 ) proposal <- 2
  if ( proposal > 2 ) proposal <- 1
  # move?
  prob_move <- densities[proposal]/densities[current]
  current <- ifelse( runif(1) < prob_move , proposal , current )
}