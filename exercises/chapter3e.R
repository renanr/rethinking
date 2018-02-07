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
quantile(samples, c(.17, .83))
PI(samples, .66)

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

# Chapter 3 practice: hard

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
