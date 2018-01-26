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