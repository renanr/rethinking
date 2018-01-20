# GRID APPROXIMATION

# define grid
points.nb <- 100
p_grid <- seq(from=0, to=1, length.out = points.nb)

# define prior
prior <- exp(-5 * abs(p_grid - 0.5))

# compute likelihood at each value in grid
likelihood <- dbinom(6, size=9, prob=p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# display the posterior
plot(p_grid, posterior, type="b", xlab="probability of water", ylab="posterior probability")
mtext(paste(toString(points.nb), "points", sep=" "))

# QUADRATIC APPROXIMATION

library(rethinking)
globe.qa <- map(
  alist(
    w ~ dbinom(9, p), # binomial likelihood
    p ~ dunif(0, 1)   # uniform prior
  ),
  data=list(w=6)
)

# display summary of quadratic approximation
precis(globe.qa)