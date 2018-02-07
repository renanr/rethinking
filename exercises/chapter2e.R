# (1) W, W, W
# (2) W, W, W, L
# (3) L, W, W, L, W, W, W

# Approximating the continuous [0, 1] distribution os possible water/land ratios
nb.points <- 100
p_grid <- seq(from=0, to=1, length.out=nb.points)

# Prior probability
prior <- rep(1, nb.points)

# (1)
# Binomial likelihood, which gives
likelihood <- dbinom(3, size=3, prob=p_grid)

# Posterior = likelihood * prior
unstd.posterior <- likelihood * prior

# Standardizes posterior
posterior <- unstd.posterior / sum(unstd.posterior)

# Plotting
plot(p_grid, posterior, type="b", xlab="probability of water", ylab="posterior probability")
mtext(paste(toString(nb.points), "points", sep=" "))

# (2) "incrementally"
likelihood.2 <- dbinom(0, size=1, prob=p_grid)
unstd.posterior.2 <- posterior * likelihood.2
posterior.2 <- unstd.posterior.2 / sum(unstd.posterior.2)

# Plotting
plot(p_grid, posterior.2, type="b", xlab="probability of water", ylab="posterior probability")
mtext(paste(toString(nb.points), "points", sep=" "))

# (2) "independently"
likelihood.2.2 <- dbinom(3, size=4, prob=p_grid)
unstd.posterior.2.2 <- likelihood.2.2 * prior
posterior.2.2 <- unstd.posterior.2.2 / sum(unstd.posterior.2.2)
plot(p_grid, posterior2.2, type="b", xlab="probability of water", ylab="posterior probability")
mtext(paste(toString(nb.points), "points", sep=" "))

# (3) "independently"
likelihood.3 <- dbinom(5, size=7, prob=p_grid)
unstd.posterior.3 <- likelihood.3 * prior
posterior.3 <- unstd.posterior.3 / sum(unstd.posterior.3)
plot(p_grid, posterior.3, type="b", xlab="probability of water", ylab="posterior probability")
mtext(paste(toString(nb.points), "points", sep=" "))

# (1) W, W, W
# (2) W, W, W, L
# (3) L, W, W, L, W, W, W

# Approximating the continuous [0, 1] distribution os possible water/land ratios
nb.points <- 100
p_grid <- seq(from=0, to=1, length.out=nb.points)

# Prior probability
prior <- ifelse(p_grid < 0.5, 0, 1)

# (1)
# Binomial likelihood, which gives
likelihood <- dbinom(3, size=3, prob=p_grid)

# Posterior = likelihood * prior
unstd.posterior <- likelihood * prior

# Standardizes posterior
posterior <- unstd.posterior / sum(unstd.posterior)

# Plotting
plot(p_grid, posterior, type="b", xlab="probability of water", ylab="posterior probability")
mtext(paste(toString(nb.points), "points", sep=" "))

# (2) "incrementally"
likelihood.2 <- dbinom(0, size=1, prob=p_grid)
unstd.posterior.2 <- posterior * likelihood.2
posterior.2 <- unstd.posterior.2 / sum(unstd.posterior.2)

# Plotting
plot(p_grid, posterior.2, type="b", xlab="probability of water", ylab="posterior probability")
mtext(paste(toString(nb.points), "points", sep=" "))

# (2) "independently"
likelihood.2.2 <- dbinom(3, size=4, prob=p_grid)
unstd.posterior.2.2 <- likelihood.2.2 * prior
posterior.2.2 <- unstd.posterior.2.2 / sum(unstd.posterior.2.2)
plot(p_grid, posterior2.2, type="b", xlab="probability of water", ylab="posterior probability")
mtext(paste(toString(nb.points), "points", sep=" "))

# (3) "independently"
likelihood.3 <- dbinom(5, size=7, prob=p_grid)
unstd.posterior.3 <- likelihood.3 * prior
posterior.3 <- unstd.posterior.3 / sum(unstd.posterior.3)
plot(p_grid, posterior.3, type="b", xlab="probability of water", ylab="posterior probability")
mtext(paste(toString(nb.points), "points", sep=" "))