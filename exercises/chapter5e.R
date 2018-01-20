#######
# 5M1 #
#######

N <- 100
rho <- 0.3 # the smaller the correlation factor is, the more it is reduced when both predictors are included in the model
x_academia <- rnorm(N)
x_agua_bebida <- rnorm(N , rho*x_academia, sqrt(1-rho^2))
emagrecimento <- rnorm(N , x_academia)
d <- data.frame(emagrecimento, x_academia, x_agua_bebida)

m1 <- lm(emagrecimento ~ x_academia, data=d)

library(rethinking)
precis(m1)

m2 <- lm(emagrecimento ~ x_agua_bebida, data=d)
precis(m2)

m3 <- lm(emagrecimento ~ x_academia + x_agua_bebida, data=d)
precis(m3)

#######
# 5M2 #
#######

N <- 100
rho <- 0.7 # the smaller the correlation factor is, the more it is reduced when both predictors are included in the model
x_sleepers <- rnorm(N)
x_potions <- rnorm(N , rho*x_sleepers, sqrt(1-rho^2))
grana <- rnorm(N , x_sleepers - x_potions)
d <- data.frame(grana, x_sleepers, x_potions)

m1 <- lm(grana ~ x_sleepers, data=d)

library(rethinking)
precis(m1)

m2 <- lm(grana ~ x_potions, data=d)
precis(m2)

m3 <- lm(grana ~ x_sleepers + x_potions, data=d)
precis(m3)

#######
# 5M4 #
#######

library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

d$MedianAgeMarriage.s <- (d$MedianAgeMarriage-mean(d$MedianAgeMarriage))/
  sd(d$MedianAgeMarriage)

d$Marriage.s <- (d$Marriage - mean(d$Marriage))/sd(d$Marriage)

mormons <- read.csv("~/R Projects/mormonperc.csv")

d <- merge(d, mormons)
d$MormonPercentage.s <- (d$percentage - mean(d$percentage)) / sd(d$percentage)

m5.m4 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR * Marriage.s + bM * MedianAgeMarriage.s + bP * MormonPercentage.s,
    a ~ dnorm( 10 , 10 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bM ~ dnorm( 0 , 1 ) ,
    bP ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ),
  data=d
)

precis(m5.m4)

#######
# 5H1 #
#######

# (1)

data(foxes)
f <- foxes
f$area.s <- (f$area - mean(f$area)) / sd(f$area)
f$groupsize.s <- (f$groupsize - mean(f$groupsize)) / sd(f$groupsize)
m5.h1.1 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b * area.s,
    a ~ dnorm(5, 3),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 4)
  ),
  data=f
)
precis(m5.h1.1)

A.seq <- seq( from=-2.5 , to=2.5 , length.out=100 )
mu <- link( m5.h1.1 , data=data.frame(area.s=A.seq) )
mu.PI <- apply( mu , 2 , PI, prob=.95 )
plot( weight ~ area.s , data=f , col=rangi2 )
abline( m5.h1.1 )
shade( mu.PI , A.seq )

# (2)

m5.h1.2 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b * groupsize.s,
    a ~ dnorm(5, 3),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 4)
  ),
  data=f
)
precis(m5.h1.2)

GS.seq <- seq( from=-2.5 , to=2.5 , length.out=100 )
mu <- link( m5.h1.2 , data=data.frame(groupsize.s=GS.seq) )
mu.PI <- apply( mu , 2 , PI, prob=.95 )
plot( weight ~ groupsize.s , data=f , col=rangi2 )
abline( m5.h1.2 )
shade( mu.PI , GS.seq )
# small and uncertain (small world) negative correlation between group size and weight

#######
# 5H2 #
#######

m5.h2 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bG * groupsize.s + bA * area.s,
    a ~ dnorm(5, 3),
    bG ~ dnorm(0, 1),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 4)
  ),
  data=f
)
precis(m5.h2)

A.seq <- seq( from=-2.5 , to=2.5 , length.out=100 )
GS.mean <- mean(f$groupsize.s)
mu <- link(m5.h2 , data=data.frame(area.s=A.seq, groupsize.s=GS.mean))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply( mu , 2 , PI, prob=.95 )
w.sim <- sim(m5.h2, data=data.frame(area.s=A.seq, groupsize.s=GS.mean), n=1e4)
w.PI <- apply(w.sim, 2, PI, prob=.95)
plot( weight ~ area.s , data=f , col=rangi2 )
lines( A.seq , mu.mean )
shade( mu.PI , A.seq )
shade( w.PI , A.seq )

GS.seq <- seq(from=-2.5, to=2.5, length.out=100)
A.mean <- mean(f$area.s)
mu <- link(m5.h2, data=data.frame(area.s=A.mean, groupsize.s=GS.seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=.95)
w.sim <- sim(m5.h2, data=data.frame(area.s=A.mean, groupsize.s=GS.seq))
w.PI <- apply(w.sim, 2, PI, prob=.95)
plot(weight ~ groupsize.s, data=f, col=rangi2)
lines(GS.seq, mu.mean)
shade(mu.PI, GS.seq)
shade(w.PI, GS.seq)

# The model says both are important. We get this because each predictor is correlated with a
# different sign, and both are positively correlated, as can be seen:

m5e <- lm(groupsize.s ~ area.s, data=f)
precis(m5e) # area.s      0.83   0.05  0.74  0.91

#######
# 5H3 #
#######

# (1)

f$avgfood.s <- (f$avgfood - mean(f$avgfood)) / sd(f$avgfood)

m5.h3.1 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bF * avgfood.s + bG * groupsize.s,
    a ~ dnorm(5, 3),
    bG ~ dnorm(0, 1),
    bF ~ dnorm(0, 1),
    sigma ~ dunif(0, 4)
  ),
  data=f
)
precis(m5.h3.1)

# (2)

m5.h3.2 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bF * avgfood.s + bG * groupsize.s + bA * area.s,
    a ~ dnorm(5, 3),
    bG ~ dnorm(0, 1),
    bF ~ dnorm(0, 1),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 4)
  ),
  data=f
)
precis(m5.h3.2)

m5e2 <- lm(avgfood.s ~ area.s, data=f)
precis(m5e2) # area.s      0.88   0.04  0.81  0.95

F.seq <- seq( from=-2.5 , to=2.5 , length.out=100 )
GS.mean <- mean(f$groupsize.s)
mu <- link(m5.h3.2 , data=data.frame(avgfood.s=F.seq, groupsize.s=GS.mean))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply( mu , 2 , PI, prob=.95 )
w.sim <- sim(m5.h3.2, data=data.frame(avgfood.s=F.seq, groupsize.s=GS.mean), n=1e4)
w.PI <- apply(w.sim, 2, PI, prob=.95)
plot( weight ~ avgfood.s , data=f , col=rangi2 )
lines( F.seq , mu.mean )
shade( mu.PI , F.seq )
shade( w.PI , F.seq )

# the effects are reduced because they represent very similar properties (it can be seen that
# they're strongly correlated, as an argument), probably, the larger the area, the more food
# can be found.

# I find it really hard to decide between these coefficients. I think I'd pick avgfood for two
# reasons: (1) epistemologically, food seems more directly related to weight (unfortunately for
# me) and (2) the beta cofficients are a little further away from zero