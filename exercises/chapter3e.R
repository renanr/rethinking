# Chapter 3 Practice: Easy

p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size=9, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
nb.samples <- 1e4
samples <- sample(p_grid, prob=posterior, size=nb.samples, replace=TRUE)

plot(posterior)
sum(posterior[p_grid > .2])

# 1
a <- sum(samples < .2) / nb.samples

# 2
b <- sum(samples > .8) / nb.samples

# 3
1 - a - b

# 4
quantile(samples, .2)

# 5
quantile(samples, .8)

# 6
HPDI(samples, prob=.66)

# 7
quantile(samples, c(.17, .87))