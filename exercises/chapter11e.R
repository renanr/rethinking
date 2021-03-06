# CHAPTER 11
# EASY
# 1

# An ordered categorical variable has an implicit increasing relation
# among values, such as the degree to which someone likes a product (if
# it's defined from 1 to 4, 4 means that the person likes it more than
# 3, which in turn means she likes it more than 2, etc).
# An unordered categorical only has distinct values, but they have no
# increasing or decreasing relations, such as colour of a car (1 for green,
# 2 for red, etc).

# 2
# A cumulative logit link. It's different because it forces the variables
# to be ordered.

# 3
# That the rates or probabilities are much lower than they really are.

# 4
# Number of fraudulent transactions in a time period.
# For the under-dispersed, maybe observations of a species when there's a single
# last individual in the area?

# MEDIUM
# 1
d = data.frame(rating = c(1, 2, 3, 4),
               count = c(12, 36, 7, 41))

d$cumprob = cumsum(d$count) / sum(d$count)
d$logcumodds = with(d, log(cumprob / (1 - cumprob)))

# 2
library(ggthemes)

ggplot(d) +
  geom_segment(aes(x=rating, xend=rating, y=0, yend=cumprob), colour='red') +
  geom_segment(aes(x=rating, xend=rating, y=cumprob - count / sum(count), yend=cumprob), colour='blue') +
  geom_line(aes(x=rating, y=cumprob), colour='white') +
  geom_point(aes(x=rating, y=cumprob), colour='white') +
  theme_solarized(light = FALSE) +
  labs(x='Rating', y='Cumulative Proportion')

# 3
# dzip <- function( x , p , lambda , log=TRUE ) {
#   ll <- ifelse(
#     x==0 ,
#     p + (1-p)*exp(-lambda) ,
#     (1-p)*dpois(x,lambda,FALSE)
#   )
#   if ( log==TRUE ) ll <- log(ll)
#   return(ll)
# }
dzib <- function( x , pz , n , p , log=TRUE ) {
  ll <- ifelse(
    x==0 ,
    pz + (1-pz)*((1 - p) ** n) ,
    (1-pz)*dbinom(x, size=n, prob=p, FALSE)
  )
  if ( log==TRUE ) ll <- log(ll)
  return(ll)
}

# HARD
# 1
data("Hurricanes")
d = Hurricanes

m1a = map2stan(
  alist(
    deaths ~ dpois(lambda),
    log(lambda) <- a,
    a ~ dnorm(3, 10)
  ), data=d, iter=1500, warmup=300, chains=2
)

m1b = map2stan(
  alist(
    deaths ~ dpois(lambda),
    log(lambda) <- a + b * femininity,
    a ~ dnorm(3, 10),
    b ~ dnorm(0, 5)
  ), data=d, iter=1500, warmup=300, chains=2
)

s1 = extract.samples(m1b)
ggplot() + geom_density(aes(s1$b))
# Looks like a quadratic approximation would be pretty fine.

compare(m1a, m1b)
precis(m1b)
sim1 = sim(m1b, d, 1e3)
d$proj = apply(sim1, 2, mean)
d$hpdimin = apply(sim1, 2, HPDI, .87)[1, ]
d$hpdimax = apply(sim1, 2, HPDI, .87)[2, ]
ggplot(d) +
  geom_point(aes(x=femininity, y=deaths, colour='Real Deal')) +
  geom_point(aes(x=femininity, y=proj, colour='Retrodiction')) +
  geom_errorbar(aes(x=femininity, ymin=hpdimin, ymax=hpdimax))

# The association is weak, but pretty certain. It does not retrodict well for
# very feminine (femininity > .75) names.

# 2
m2 = map2stan(
  alist(
    deaths ~ dgampois(mu, scale=theta),
    log(mu) <- a + b * femininity,
    a ~ dnorm(3, 10),
    b ~ dnorm(0, 5),
    theta ~ dexp(1)
  ), data=d, iter=1500, warmup=300, chains=2, constraints=list(theta="lower=0")
)

DIC(m1b)
DIC(m2)
precis(m2)

sim2 = sim(m2, d, 1e3)
d$proj2 = apply(sim2, 2, mean)
d$hpdimin2 = apply(sim2, 2, HPDI, .89)[1, ]
d$hpdimax2 = apply(sim2, 2, HPDI, .89)[2, ]
ggplot(d) +
  geom_point(aes(x=femininity, y=deaths, colour='Real Deal')) +
  geom_point(aes(x=femininity, y=proj2, colour='Retrodiction')) +
  geom_errorbar(aes(x=femininity, ymin=hpdimin2, ymax=hpdimax2))

# The new model allows more variation for the rate (lambda) for every pair
# of parameters, since it's sampled from a distribution, and therefore there
# are broader values of parameters that are plausible.

# 3 to 5: I'm sorry but I couldn't work them out just yet.
# Will get them done next time I go through the book!

# 6

data("Fish")
d = Fish

m6 <- map(
  alist(
    fish_caught ~ dzipois( p , lambda ),
    logit(p) <- aP + bC * child + bP * persons + bM * camper,
    log(lambda) <- aL + bL * livebait + log(hours),
    aP ~ dnorm(0,1),
    aL ~ dnorm(0,10),
    c(bC, bP, bM, bL) ~ dnorm(0,1)
  ) ,
  data=d )
precis(m6)

m6b <- map(
  alist(
    fish_caught ~ dzipois( p , lambda ),
    logit(p) <- aP + bP * persons,
    log(lambda) <- aL + bL * livebait + bC * child + bM * camper + log(hours),
    aP ~ dnorm(0,1),
    aL ~ dnorm(0,10),
    c(bC, bP, bM, bL) ~ dnorm(0,1)
  ) ,
  data=d )
precis(m6b)

m6c <- map(
  alist(
    fish_caught ~ dzipois( p , lambda ),
    logit(p) <- aP + bP * persons,
    log(lambda) <- aL + bL * livebait + bC * child + log(hours),
    aP ~ dnorm(0,1),
    aL ~ dnorm(0,10),
    c(bC, bP, bM, bL) ~ dnorm(0,1)
  ) ,
  data=d )
precis(m6b)

compare(m6, m6b, m6c)
