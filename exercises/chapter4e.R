my.model.params <- alist(
  y ~ dnorm(mu, sigma),
  mu ~ dnorm(0, 10),
  sigma ~ dunif(0, 10)
)

sample_mu <- rnorm(1e4, 0, 10)
sample_sigma <- runif(1e4, 0, 10)
sim.prior <- rnorm(1e4, sample_mu, sample_sigma)
dens(sim.prior)

library(rethinking)
data(Howell1)
d <- Howell1
d$y <- d$height
my.model <- map(my.model.params, data=d)

library(rethinking)

data("Howell1")
d <- Howell1

# H1

d2 <- d[d$weight > 32,]
plot(d2$weight ~ d2$age)
min(d2$age)

d3 <- d[d$age >= 18, ]
d3$weight.s <- (d3$weight - mean(d3$weight)) / sd(d3$weight)
m4.1 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * weight.s,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data=d3
)

x.weights <- c(46.95, 43.72, 64.78, 32.59, 54.63)
x.weights.s <- (x.weights - mean(d3$weight)) / sd(d3$weight)
sim.heights <- sim(m4.1, data=list(weight.s=x.weights.s), n=1e4)
expected <- apply(sim.heights, 2, mean)
hpdi <- apply(sim.heights, 2, HPDI, prob=.89)

dotchart( expected , labels=x.weights , xlim=c(120,190) , cex=0.6 )
abline( v=0 , col=col.alpha("black",0.2) )
for ( i in 1:length(x.weights) ) {
  j <- i
  lines( c(hpdi[1,j], hpdi[2,j]) , rep(i,2) )
  points( c(hpdi[1,j],hpdi[2,j]) , rep(i,2),
          pch=3 , cex=0.6 , col="gray" )
}

# H2

d4 <- d[d$age < 18, ]
d4$weight.s <- (d4$weight - mean(d4$weight)) / sd(d4$weight)
m4.2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * weight.s,
    a ~ dnorm(100, 20),
    b ~ dnorm(20, 20),
    sigma ~ dunif(0, 50)
  ),
  data=d4
)

precis(m4.2)
str(10 / sd(d4$weight)  * 24.31)
plot(d4$height ~ d4$weight, col=col.alpha(rangi2, 0.4))
abline( a=coef(m4.2)["a"] , b=coef(m4.2)["b"] )

seq.weight <- sort(d4$weight)
seq.weight.s <- (seq.weight - mean(d4$weight)) / sd(d4$weight)
mu <- link(m4.2, data=data.frame(weight.s=seq.weight.s), n=1e4)
mu.mean <- apply(mu, 2, mean)
lines(seq.weight, mu.mean, col="yellow4")
mu.HPDI <- apply(mu, 2, HPDI, prob=.89)
shade( mu.HPDI, seq.weight )

simloks <- sim(m4.2, data=data.frame(weight.s=seq.weight.s), n=1e4)
sim.hpdi <- apply(simloks, 2, HPDI)
shade(sim.hpdi, seq.weight)

# The model overestimates heights for both low and high weights.
# I would pick a different model that allows the slope to adjust to weight values.

library(rethinking)
data("Howell1")
d <- Howell1

d$weight.log <- log(d$weight)

m4.h3 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * weight.log,
    a ~ dnorm(178, 100),
    b ~ dnorm(0, 100),
    sigma ~ dunif(0, 50)
  ),
  data=d
)

precis(m4.h3)

plot(height ~ weight, data=Howell1, col=col.alpha(rangi2, 0.4))

# (1)
mu <- link(m4.h3, n=1e4)
mu.mean <- apply(mu, 2, mean)
points(d$weight, mu.mean)

# (2)
mu.hpdi <- apply(mu, 2, HPDI)
for ( i in 1:length(d$weight) ) {
  lines(c(d$weight[[i]], d$weight[[i]]), c(mu.hpdi[1, i], mu.hpdi[2, i]))
}

# (3)
sim.heights <- sim(m4.h3, n=1e4)
sim.hpdi <- apply(sim.heights, 2, HPDI)
for ( i in 1:length(d$weight) ) {
  lines(c(d$weight[[i]], d$weight[[i]]), c(sim.hpdi[1, i], sim.hpdi[2, i]))
}

# Shit, I couldn't make that look good. Let's try again!
plot(height ~ weight, data=Howell1, col=col.alpha(rangi2, 0.4))

seq.weights <- sort(d$weight)
seq.weights.log <- log(seq.weights)
mu <- link(m4.h3, data=data.frame(weight.log=seq.weights.log), n=1e4)

# 1
mu.mean <- apply(mu, 2, mean)
lines(seq.weights, mu.mean)

# 2
mu.hpdi <- apply(mu, 2, HPDI)
shade(mu.hpdi, seq.weights)

# 3
sim2 <- sim(m4.h3, data=data.frame(weight.log=seq.weights.log), n=1e4)
sim.hpdi <- apply(sim2, 2, HPDI)
shade(sim.hpdi, seq.weights)