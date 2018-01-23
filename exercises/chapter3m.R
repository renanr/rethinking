# Chapter 3 Practice: Medium

# 1
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1, 1000)
likelihood <- dbinom(8, size=15, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

# 2
plot(posterior ~ p_grid, type="l")
nb.samples <- 1e4
samples <- sample(p_grid, prob=posterior, size=nb.samples, replace=TRUE)
HPDI(samples, .9)

# 3
dummy_ws <- rbinom(1e4, size=15, prob=samples)
table(dummy_ws) / 1e4

# 4
new.prob <- rbinom(1e4, size=9, prob=samples)
table(new.prob) / 1e4

# 5
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- ifelse(p_grid < .5, 0, 1)
likelihood <- dbinom(8, size=15, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
plot(p_grid, posterior, type="b", xlab="probability of water", ylab="posterior probability")
mtext("new prior")
nb.samples = 1e4
samples <- sample(p_grid, prob=posterior, size=nb.samples, replace=TRUE)
HPDI(samples, .9)
dummy_ws <- rbinom(1e4, size=15, prob=samples)
table(dummy_ws) / 1e4
new.prob <- dbinom(6, size=9, prob=samples)
dens(new.prob)

dbinom(6, size=9, prob=.7)