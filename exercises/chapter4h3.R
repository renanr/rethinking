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