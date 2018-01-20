p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size=9, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

plot(samples)

library(rethinking)
dens(samples)

sum(posterior[p_grid < .5])
sum(samples < .5) / 1e4

quantile(samples, .8)

quantile(samples, c(.1, .9))

loss <- sapply(p_grid, function(d) sum(posterior*abs(d - p_grid)))
plot(loss)

dummy_w <- rbinom(1e5, size=2, prob=.7)
table(dummy_w) / 1e5

dummy_w <- rbinom(1e5, size=9, prob=.7)
simplehist(dummy_w, xlab="dummy water count")

w <- rbinom(1e4, size=9, prob=samples)
simplehist(w)