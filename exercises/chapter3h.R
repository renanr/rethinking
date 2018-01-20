data(homeworkch3)

sum(birth1 + birth2) # 111
length(birth1) + length(birth2) # 200

# 1
nb.points <- 1e4
p_grid <- seq(from=0, to=1, length.out=nb.points)
prior <- rep(1, nb.points)
likelihood <- dbinom(111, size=200, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
plot(p_grid, posterior, type="b", xlab="probability of boy", ylab="posterior")
p_grid[which.max(posterior)] # 0.555

# 2
nb.samples <- 1e4
samples <- sample(p_grid, prob=posterior, size=nb.samples, replace=TRUE)
HPDI(samples, c(.5, .89, .97))
#     |0.97     |0.89      |0.5      0.5|     0.89|     0.97| 
# 0.4783478 0.4980498 0.5296530 0.5765577 0.6109611 0.6314631

# 3
simulated.births <- rbinom(1e4, size=200, prob=samples)
dens(simulated.births)

# 4
simulated.first.births <- rbinom(1e4, size=100, prob=samples)
dens(simulated.first.births)
sum(birth1)

# 5
sum(birth1 == 0) # 49
sum(birth2[birth1 == 0]) # 39
simulated.boys.after.girls <- rbinom(1e4, size=49, prob=samples)
dens(simulated.boys.after.girls)