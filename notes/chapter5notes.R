# load data
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

# standardize predictor
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage-mean(d$MedianAgeMarriage))/
  sd(d$MedianAgeMarriage)
# fit model
m5.1 <- map(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bA * MedianAgeMarriage.s ,
    a ~ dnorm( 10 , 10 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) , data = d )

# compute percentile interval of mean
MAM.seq <- seq( from=-3 , to=3.5 , length.out=30 )
mu <- link( m5.1 , data=data.frame(MedianAgeMarriage.s=MAM.seq) )
mu.PI <- apply( mu , 2 , PI )

# plot it all
plot( Divorce ~ MedianAgeMarriage.s , data=d , col=rangi2 )
abline( m5.1 )
shade( mu.PI , MAM.seq )

# now for marriage rates
d$Marriage.s <- (d$Marriage - mean(d$Marriage))/sd(d$Marriage)
m5.2 <- map(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bR * Marriage.s ,
    a ~ dnorm( 10 , 10 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) , data = d )
MR.seq <- seq( from=-3 , to=3.5 , length.out=30 )
mu <- link( m5.2 , data=data.frame(Marriage.s=MR.seq) )
mu.PI <- apply( mu , 2 , PI )
plot( Divorce ~ Marriage.s , data=d , col=rangi2 )
abline( m5.2 )
shade( mu.PI , MR.seq )

# let's put it together
m5.3 <- map(
            alist(
              Divorce ~ dnorm( mu , sigma ) ,
              mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s ,
              a ~ dnorm( 10 , 10 ) ,
              bR ~ dnorm( 0 , 1 ) ,
              bA ~ dnorm( 0 , 1 ) ,
              sigma ~ dunif( 0 , 10 )
            ) ,
            data = d )
precis( m5.3 )

# PREDICTOR RESIDUAL
m5.4 <- map(
            alist(
              Marriage.s ~ dnorm( mu , sigma ) ,
              mu <- a + b*MedianAgeMarriage.s ,
              a ~ dnorm( 0 , 10 ) ,
              b ~ dnorm( 0 , 1 ) ,
              sigma ~ dunif( 0 , 10 )
            ) ,
            data = d )

# compute expected value at MAP, for each State
mu <- coef(m5.4)['a'] + coef(m5.4)['b']*d$MedianAgeMarriage.s
# compute residual for each State
m.resid <- d$Marriage.s - mu

plot( Marriage.s ~ MedianAgeMarriage.s , d , col=rangi2 )
abline( m5.4 )
# loop over States
for ( i in 1:length(m.resid) ) {
  x <- d$MedianAgeMarriage.s[i] # x location of line segment
  y <- d$Marriage.s[i] # observed endpoint of line segment
  # draw the line segment
  lines( c(x,x) , c(mu[i],y) , lwd=0.5 , col=col.alpha("black",0.7) )
}

# COUNTERFACTUAL
# prepare new counterfactual data
A.avg <- mean( d$MedianAgeMarriage.s )
R.seq <- seq( from=-3 , to=3 , length.out=30 )
pred.data <- data.frame(
  Marriage.s=R.seq,
  MedianAgeMarriage.s=A.avg
)
# compute counterfactual mean divorce (mu)
mu <- link( m5.3 , data=pred.data )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )
# simulate counterfactual divorce outcomes
R.sim <- sim( m5.3 , data=pred.data , n=1e4 )
R.PI <- apply( R.sim , 2 , PI )
# display predictions, hiding raw data with type="n"
plot( Divorce ~ Marriage.s , data=d , type="n" )
mtext( "MedianAgeMarriage.s = 0" )
lines( R.seq , mu.mean )
shade( mu.PI , R.seq )
shade( R.PI , R.seq )

R.avg <- mean( d$Marriage.s )
A.seq <- seq( from=-3 , to=3.5 , length.out=30 )
pred.data2 <- data.frame(
  Marriage.s=R.avg,
  MedianAgeMarriage.s=A.seq
)
mu <- link( m5.3 , data=pred.data2 )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )
A.sim <- sim( m5.3 , data=pred.data2 , n=1e4 )
A.PI <- apply( A.sim , 2 , PI )
plot( Divorce ~ MedianAgeMarriage.s , data=d , type="n" )
mtext( "Marriage.s = 0" )
lines( A.seq , mu.mean )
shade( mu.PI , A.seq )
shade( A.PI , A.seq )

# call link without specifying new data
# so it uses original data
mu <- link( m5.3 )
# summarize samples across cases
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )
# simulate observations
# again no new data, so uses original data
divorce.sim <- sim( m5.3 , n=1e4 )
divorce.PI <- apply( divorce.sim , 2 , PI )

plot( mu.mean ~ d$Divorce , col=rangi2 , ylim=range(mu.PI) ,
      xlab="Observed divorce" , ylab="Predicted divorce" )
abline( a=0 , b=1 , lty=2 )
for ( i in 1:nrow(d) )
  lines( rep(d$Divorce[i],2) , c(mu.PI[1,i],mu.PI[2,i]) ,
         col=rangi2 )
identify( x=d$Divorce , y=mu.mean , labels=d$Loc , cex=0.8 )

# compute residuals
divorce.resid <- d$Divorce - mu.mean
# get ordering by divorce rate
o <- order(divorce.resid)
# make the plot
dotchart( divorce.resid[o] , labels=d$Loc[o] , xlim=c(-6,5) , cex=0.6 )
abline( v=0 , col=col.alpha("black",0.2) )
for ( i in 1:nrow(d) ) {
  j <- o[i] # which State in order
  lines( d$Divorce[j]-c(mu.PI[1,j],mu.PI[2,j]) , rep(i,2) )
  points( d$Divorce[j]-c(divorce.PI[1,j],divorce.PI[2,j]) , rep(i,2),
          pch=3 , cex=0.6 , col="gray" )
}

# Now let's go to milk

library(rethinking)
data(milk)
d <- milk
str(d)

dcc <- d[ complete.cases(d) , ]

m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + bn*neocortex.perc ,
    a ~ dnorm( 0 , 100 ) ,
    bn ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 1 )
  ) ,
  data=dcc )

np.seq <- 0:100
pred.data <- data.frame( neocortex.perc=np.seq )
mu <- link( m5.5 , data=pred.data , n=1e4 )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )
plot( kcal.per.g ~ neocortex.perc , data=dcc , col=rangi2 )
lines( np.seq , mu.mean )
lines( np.seq , mu.PI[1,] , lty=2 )
lines( np.seq , mu.PI[2,] , lty=2 )

dcc$log.mass <- log(dcc$mass)
m5.6 <- map(
            alist(
              kcal.per.g ~ dnorm( mu , sigma ) ,
              mu <- a + bm*log.mass ,
              a ~ dnorm( 0 , 100 ) ,
              bm ~ dnorm( 0 , 1 ) ,
              sigma ~ dunif( 0 , 1 )
            ) ,
            data=dcc )
precis(m5.6)

bm.log.seq <- seq(from=-2.5, to=4.5, length.out = 100)
pred.data <- data.frame( log.mass=bm.log.seq )
mu <- link( m5.6 , data=pred.data , n=1e4 )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )
plot( kcal.per.g ~ log.mass , data=dcc , col=rangi2 )
lines( bm.log.seq , mu.mean )
lines( bm.log.seq , mu.PI[1,] , lty=2 )
lines( bm.log.seq , mu.PI[2,] , lty=2 )

m5.7 <- map(
            alist(
              kcal.per.g ~ dnorm( mu , sigma ) ,
              mu <- a + bn*neocortex.perc + bm*log.mass ,
              a ~ dnorm( 0 , 100 ) ,
              bn ~ dnorm( 0 , 1 ) ,
              bm ~ dnorm( 0 , 1 ) ,
              sigma ~ dunif( 0 , 1 )
            ) ,
            data=dcc )
precis(m5.7)

mean.log.mass <- mean( log(dcc$mass) )
np.seq <- 0:100
pred.data <- data.frame(
  neocortex.perc=np.seq,
  log.mass=mean.log.mass
)
mu <- link( m5.7 , data=pred.data , n=1e4 )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )
plot( kcal.per.g ~ neocortex.perc , data=dcc , type="n" )
lines( np.seq , mu.mean )
lines( np.seq , mu.PI[1,] , lty=2 )
lines( np.seq , mu.PI[2,] , lty=2 )

mean.neocortex.per <- mean( dcc$neocortex.perc )
bm.log.seq <- seq(from=-3, to=5, length.out = 100)
pred.data <- data.frame(
  log.mass=bm.log.seq,
  neocortex.perc=mean.neocortex.per
)
mu <- link( m5.7 , data=pred.data , n=1e4 )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )
plot( kcal.per.g ~ log.mass , data=dcc , type="n" )
lines( bm.log.seq , mu.mean )
lines( bm.log.seq , mu.PI[1,] , lty=2 )
lines( bm.log.seq , mu.PI[2,] , lty=2 )

# Overthinking
N <- 100 # number of cases <- 0.7 # correlation btw x_pos and x_neg
rho <- 0.7 # correlation btw x_pos and x_neg
x_pos <- rnorm( N ) # x_pos as Gaussian
x_neg <- rnorm( N , rho*x_pos , # x_neg correlated with x_pos
                sqrt(1-rho^2) )
y <- rnorm( N , x_pos - x_neg ) # y equally associated with x_pos, x_neg
d <- data.frame(y,x_pos,x_neg) # bind all together in data frame
pairs(d)

# When adding variables hurts

N <- 100 # number of individuals
height <- rnorm(N,10,2) # sim total height of each
leg_prop <- runif(N,0.4,0.5) # leg as proportion of height
leg_left <- leg_prop*height + # sim left leg as proportion + error
  rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height + # sim right leg as proportion + error
  rnorm( N , 0 , 0.02 )
# combine into data frame
d <- data.frame(height,leg_left,leg_right)

m5.8 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=d )
plot(precis(m5.8))

post <- extract.samples(m5.8)
plot( bl ~ br , post , col=col.alpha(rangi2,0.1) , pch=16 )

sum_blbr <- post$bl + post$br
dens( sum_blbr , col=rangi2 , lwd=2 , xlab="sum of bl and br" )

m5.9 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=d )
precis(m5.9)

# going back to milk

library(rethinking)
data(milk)
d <- milk

# kcal.per.g regressed on perc.fat
m5.10 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + bf*perc.fat ,
    a ~ dnorm( 0.6 , 10 ) ,
    bf ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=d )
# kcal.per.g regressed on perc.lactose
m5.11 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + bl*perc.lactose ,
    a ~ dnorm( 0.6 , 10 ) ,
    bl ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=d )
precis( m5.10 , digits=3 )
precis( m5.11 , digits=3 )

m5.12 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + bf*perc.fat + bl*perc.lactose ,
    a ~ dnorm( 0.6 , 10 ) ,
    bf ~ dnorm( 0 , 1 ) ,
    bl ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=d )
precis( m5.12 , digits=3 )

pairs( ~ kcal.per.g + perc.fat + perc.lactose ,
       data=d , col=rangi2 )

cor( d$perc.fat , d$perc.lactose )

# POST-TREATMENT BIAS

# number of plants
N <- 100
# simulate initial heights
h0 <- rnorm(N,10,2)
# assign treatments and simulate fungus and growth
treatment <- rep( 0:1 , each=N/2 )
fungus <- rbinom( N , size=1 , prob=0.5 - treatment*0.4 )
h1 <- h0 + rnorm(N, 5 - 3*fungus)
# compose a clean data frame
d <- data.frame( h0=h0 , h1=h1 , treatment=treatment , fungus=fungus )

m5.13 <- map(
             alist(
               h1 ~ dnorm(mu,sigma),
               mu <- a + bh*h0 + bt*treatment + bf*fungus,
               a ~ dnorm(0,100),
               c(bh,bt,bf) ~ dnorm(0,10),
               sigma ~ dunif(0,10)
             ),
             data=d )
precis(m5.13)

m5.14 <- map(
  alist(
    h1 ~ dnorm(mu,sigma),
    mu <- a + bh*h0 + bt*treatment,
    a ~ dnorm(0,100),
    c(bh,bt) ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ),
  data=d )
precis(m5.14)

# Categorical
data(Howell1)
d <- Howell1
str(d)

m5.15 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bm*male ,
    a ~ dnorm( 178 , 100 ) ,
    bm ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d )
precis(m5.15)

post <- extract.samples(m5.15)
mu.male <- post$a + post$bm
PI(mu.male)

m5.15b <- map(
              alist(
                height ~ dnorm( mu , sigma ) ,
                mu <- af*(1-male) + am*male ,
                af ~ dnorm( 178 , 100 ) ,
                am ~ dnorm( 178 , 100 ) ,
                sigma ~ dunif( 0 , 50 )
              ) ,
              data=d )
precis(m5.15b)

data(milk)
d <- milk
unique(d$clade)
d$clade.NWM <- ifelse( d$clade=="New World Monkey" , 1 , 0 )
d$clade.OWM <- ifelse( d$clade=="Old World Monkey" , 1 , 0 )
d$clade.S <- ifelse( d$clade=="Strepsirrhine" , 1 , 0 )

m5.16 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + b.NWM*clade.NWM + b.OWM*clade.OWM + b.S*clade.S ,
    a ~ dnorm( 0.6 , 10 ) ,
    b.NWM ~ dnorm( 0 , 1 ) ,
    b.OWM ~ dnorm( 0 , 1 ) ,
    b.S ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=d )
precis(m5.16)

# sample posterior
post <- extract.samples(m5.16)
# compute averages for each category
mu.ape <- post$a
mu.NWM <- post$a + post$b.NWM
mu.OWM <- post$a + post$b.OWM
mu.S <- post$a + post$b.S
# summarize using precis
precis( data.frame(mu.ape,mu.NWM,mu.OWM,mu.S) )

diff.NWM.OWM <- mu.NWM - mu.OWM
quantile( diff.NWM.OWM , probs=c(0.025,0.5,0.975) )

# INDEX VARIABLE
d$clade_id <- coerce_index(d$clade)
m5.16_alt <- map(
                 alist(
                   kcal.per.g ~ dnorm( mu , sigma ) ,
                   mu <- a[clade_id] ,
                   a[clade_id] ~ dnorm( 0.6 , 10 ) ,
                   sigma ~ dunif( 0 , 10 )
                 ) ,
                 data=d )
precis( m5.16_alt , depth=2 )