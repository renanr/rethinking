library(rethinking)
data(Howell1)
d <- Howell1

d2 <- d[d$age >= 18 ,]
dens(d2$height)

curve(dnorm(x, 178, 20), from=100, to=250)

curve(dunif(x, 0, 50), from=-10, to=60)

sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

mu.list <- seq( from=140, to=160 , length.out=200 )
sigma.list <- seq( from=4 , to=9 , length.out=200 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum( dnorm(
  d2$height ,
  mean=post$mu[i] ,
  sd=post$sigma[i] ,
  log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )

sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                       prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]
plot( sample.mu , sample.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.1) )

dens( sample.mu )
dens( sample.sigma )

HPDI( sample.mu )
HPDI( sample.sigma )

# OVERTHINKING
d3 <- sample(d2$height, size=20)
mu.list <- seq( from=150, to=170 , length.out=200 )
sigma.list <- seq( from=4 , to=20 , length.out=200 )
post2 <- expand.grid( mu=mu.list , sigma=sigma.list )
post2$LL <- sapply( 1:nrow(post2) , function(i)
  sum( dnorm( d3 , mean=post2$mu[i] , sd=post2$sigma[i] ,
              log=TRUE ) ) )
post2$prod <- post2$LL + dnorm( post2$mu , 178 , 20 , TRUE ) +
  dunif( post2$sigma , 0 , 50 , TRUE )
post2$prob <- exp( post2$prod - max(post2$prod) )
sample2.rows <- sample( 1:nrow(post2) , size=1e4 , replace=TRUE ,
                        prob=post2$prob )
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]
plot( sample2.mu , sample2.sigma , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="mu" , ylab="sigma" , pch=16 )
dens( sample2.sigma , norm.comp=TRUE )

library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)

m4.1 <- map(flist, data=d2)

precis(m4.1)

vcov(m4.1)
diag(vcov(m4.1))
cov2cor(vcov(m4.1))

post <- extract.samples(m4.1, n=1e4)
head(post)

plot(d2$height ~ d2$weight)

fllist <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b * weight,
  a ~ dnorm(178, 100),
  b ~ dnorm(0, 10),
  sigma ~ dunif(0, 50)
)

# fit model
m4.3 <- map(
  fllist,
  data=d2 )

precis(m4.3, corr=TRUE)

# CENTERING
d2$weight.c <- d2$weight - mean(d2$weight)

m4.4 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight.c ,
    a ~ dnorm( 178 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d2 )
precis( m4.4 , corr=TRUE )

plot( height ~ weight , data=d2 )
abline( a=coef(m4.3)["a"] , b=coef(m4.3)["b"] )

post <- extract.samples( m4.3 )
mu_at_50 <- post$a + post$b * 50
dens( mu_at_50 , col=rangi2 , lwd=2 , xlab="mu|weight=50" )
HPDI( mu_at_50 , prob=0.89 )

mu <- link( m4.3 )

# define sequence of weights to compute predictions for
# these values will be on the horizontal axis
weight.seq <- seq( from=25 , to=70 , by=1 )
# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link( m4.3 , data=data.frame(weight=weight.seq) )

# Would be equivalent to, manually:
post <- extract.samples(m4.3)
mu.link <- function(weight) post$a + post$b * weight
weight.seq <- seq(from=25, to=70, by=1)
mu <- sapply(weight.seq, mu.link)
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=.89)

# use type="n" to hide raw data
plot( height ~ weight , d2 , type="n" )
# loop over samples and plot each mu value
for ( i in 1:100 )
  points( weight.seq , mu[i,] , pch=16 , col=col.alpha(rangi2,0.1) )

# summarize the distribution of mu
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 )

# plot raw data
# fading out points to make line and interval more visible
plot( height ~ weight , data=d2 , col=col.alpha(rangi2,0.5) )
# plot the MAP line, aka the mean mu for each weight
lines( weight.seq , mu.mean )
# plot a shaded region for 89% HPDI
shade( mu.HPDI , weight.seq )

sim.height <- sim( m4.3 , data=list(weight=weight.seq), n=1e6 )
height.PI <- apply( sim.height , 2 , PI , prob=0.97 )

# plot raw data
plot( height ~ weight , d2 , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( weight.seq , mu.mean )
# draw HPDI region for line
shade( mu.HPDI , weight.seq )
# draw PI region for simulated heights
shade( height.PI , weight.seq )

# POLYNOMIALS

library(rethinking)
data(Howell1)
d <- Howell1
plot(d$height ~ d$weight)

d$weight.s <- ( d$weight - mean(d$weight) )/sd(d$weight)
plot(d$height ~ d$weight.)