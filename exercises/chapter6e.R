# Definition of INFORMATION: decrease in uncertainty caused by learning an outcome.
# Definition of INFORMATION ENTROPY: quantification of uncertainty. H(p) = -E * log(p_i) = - sum(p_i * log(p_i))

# 6E1
# 1. We want information entropy to be continuous. We don't want small changes in probabilities to cause big changes in entropy.
# 2. We want it to increase with the number of possible events. The more events, the more uncertainty.
# 3. We want information entropy to sum up over different outcomes.

# 6E2
# p_heads = .7
p.e2 = c(.7, .3)
entropy.e2 = -sum(p.e2 * log(p.e2)) # - E * log(p)

# 6E3
p.e3 = c(.2, .25, .25, .3)
entropy.e3 = -sum(p.e3 * log(p.e3))

# 6E4
p.e4 = c(1 / 3, 1 / 3, 1 / 3)
entropy.e4 = -sum(p.e4 * log(p.e4))

# 6M1
# Deviance is an estimator to Divergence (D(p, q) = sum(p * (log(p) - log(q))) = sum(p * log(p / q))): Dev(q) = -2 * sum(log(q))
# Cross Entropy: H(p, q) = - sum(p * log(q)) => D(p, q) = H(p, q) - H(p)

# AIC: Akaike Information Criterion = D_train + 2 * P = -2 * sum(log(p_i)) + 2 * P, in which p_i is the likelihood of (train)
#      event i and P is the number of parameters
# DIC: Deviance Information Criterion = D_bar + p_D, p_D (effective number of parameters of the model) = D_bar - D_hat,
#      in which D_bar = average deviance (calculated over many samples from the posterior) and D_hat = the deviance
#      calculated at the posterior mean.
# WAIC: Widely Available Information Criterion = -2 * (lppd - P_WAIC), in which lppd: log-pointwise-predictive-density =
#       sum(log(Pr(y_i))), (Pr(y_i) is the average likelihood of observation i) P_WAIC: effective number of parameters =
#       sum(V(y_i)), V(y_i) is the variance in log-likelihood for observation i
# All of them assume (sample size) N >> (number of parameters) K. AIC and DIC also assume that the posterior distribution
# is multivariate Gaussian. AIC also assumes that priors are flat (or overwhelmed by the likelihood); therefore, WAIC is
# the more general (less restrictive).

# 6M2
# Model selection is the exercise of using different criteria (such as the information ones) to select one model to be used
# among a group of candidates. Model averaging, on the other hand, is using a set of weights to sample proportionally from
# the group of models. Under model selection, the specificities of each model are lost. Under model averaging, less is
# lost.

# 6M3
# Because deviance scales with sample size. Also, some points may have particular effect on deviance.

# 6M4
# They decrease. Concentrated priors prevent the modeling of noise, thus are used to avoid overfitting, i.e. superfluous
# parameters will be penalized.

# 6M5
# Because they render the model "less excitable" by data, preventing it to learn noise as signal

# 6M6
# Because they render the model "too unexcitable", so it doesn't learn even signal as signal

# HARD
library(rethinking)
data(Howell1)
d <- Howell1
d$age <- (d$age - mean(d$age))/sd(d$age)
set.seed( 1000 )
i <- sample(1:nrow(d),size=nrow(d)/2)
d1 <- d[ i , ]
d2 <- d[ -i , ]

# 6H1

avg_height = mean(d1$height)
m6.1 = rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * age,
    a ~ dnorm(avg_height, 100),
    b1 ~ dnorm(0, 50),
    sigma ~ dunif(0, 200)
  ),
  data=d1
)
m6.2 = rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * age + b2 * I(age ** 2),
    a ~ dnorm(avg_height, 100),
    b1 ~ dnorm(0, 50),
    b2 ~ dnorm(0, 50),
    sigma ~ dunif(0, 200)
  ),
  data=d1
)
m6.3 = rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * age + b2 * I(age ** 2) + b3 * I(age ** 3),
    a ~ dnorm(avg_height, 100),
    b1 ~ dnorm(0, 50),
    b2 ~ dnorm(0, 50),
    b3 ~ dnorm(0, 50),
    sigma ~ dunif(0, 200)
  ),
  data=d1
)
m6.4 = rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * age + b2 * I(age ** 2) + b3 * I(age ** 3) + b4 * I(age ** 4),
    a ~ dnorm(avg_height, 100),
    b1 ~ dnorm(0, 50),
    b2 ~ dnorm(0, 50),
    b3 ~ dnorm(0, 50),
    b4 ~ dnorm(0, 50),
    sigma ~ dunif(0, 200)
  ),
  data=d1
)
m6.5 = rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * age + b2 * I(age ** 2) + b3 * I(age ** 3) + b4 * I(age ** 4) + b5 * I(age ** 5),
    a ~ dnorm(avg_height, 100),
    b1 ~ dnorm(0, 50),
    b2 ~ dnorm(0, 50),
    b3 ~ dnorm(0, 50),
    b4 ~ dnorm(0, 50),
    b5 ~ dnorm(0, 50),
    sigma ~ dunif(0, 200)
  ),
  data=d1
)
m6.6 = rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * age + b2 * I(age ** 2) + b3 * I(age ** 3) + b4 * I(age ** 4) + b5 * I(age ** 5) + b6 * I(age ** 6),
    a ~ dnorm(avg_height, 100),
    b1 ~ dnorm(0, 50),
    b2 ~ dnorm(0, 50),
    b3 ~ dnorm(0, 50),
    b4 ~ dnorm(0, 50),
    b5 ~ dnorm(0, 50),
    b6 ~ dnorm(0, 50),
    sigma ~ dunif(0, 200)
  ),
  data=d1
)

library(tidyverse)

# results below
height_models = compare(m6.1, m6.2, m6.3, m6.4, m6.5, m6.6)
#        WAIC pWAIC dWAIC weight    SE   dSE
# m6.4 1926.1   5.6   0.0   0.57 25.45    NA
# m6.5 1927.5   6.4   1.4   0.28 25.32  1.10
# m6.6 1928.6   7.7   2.6   0.16 25.04  2.82
# m6.3 1952.3   5.4  26.2   0.00 24.19 10.80
# m6.2 2149.8   5.1 223.7   0.00 22.55 26.67
# m6.1 2395.4   3.4 469.4   0.00 22.95 31.05
# The models with degrees 1, 2 and 3 got overwhelmed (3 not that much, actually, but still got zero weight).
# dSE is comparable to dWAIC from 5 and 6 to 4, and the weights of both of them were sensibly high compared
# to 4's.
plot(height_models, SE=TRUE, dSE=TRUE)

# 6H2&3

models = list(c(m6.1, 'm6.1'), c(m6.2, 'm6.2'), c(m6.3, 'm6.3'), c(m6.4, 'm6.4'), c(m6.5, 'm6.5'), c(m6.6, 'm6.6'))
age.cf = seq(from=-2, to=3, length.out=300)
links = bind_rows(map(models, function(m) {
  m.link = link(m[[1]], data=data.frame(age=age.cf), n=1e3)
  mu = apply(m.link, 2, mean)
  pi = apply(m.link, 2, PI, .97)
  return(tibble(age=age.cf, mu=mu, pimin=pi[1, ], pimax=pi[2, ], name=m[[2]]))
}))

height.ensemble = ensemble(m6.1, m6.2, m6.3, m6.4, m6.5, m6.6, data=data.frame(age=age.cf))
links = bind_rows(links, tibble(age=age.cf,
                                mu=apply(height.ensemble$link , 2 , mean),
                                pimin=apply(height.ensemble$link , 2 , PI, .97)[1, ],
                                pimax=apply(height.ensemble$link , 2 , PI, .97)[2, ],
                                name='ensemble'))

ggplot(links) +
  theme_light(base_family = 'Avenir') +
  geom_ribbon(aes(ymin=pimin, ymax=pimax, x=age), fill = "grey70", alpha=.3) +
  geom_line(aes(x=age, y=mu)) +
  scale_x_continuous(limits=c(-2, 3), breaks=seq(from=-2, to=3, by=1)) +
  geom_point(data=d1, aes(x=age, y=height), alpha=.1) +
  labs(x = 'Centered Age',
       y = 'Height',
       title = 'Height as a function of age') +
  facet_wrap(~ name, scales='free_y', ncol=3)

# 1 and 2 have huge intervals of systematic under or overprediction (one kind per interval, I mean).
# 3 fluctuates more wildly where there are less points. 4, 5 and 6 have increasing confidence intervals
# out of the boundaries of the training data (and increasingly wider). The ensemble gets a more stable
# behaviour and also the shaded areas get reduced under the curve, they lean upwards.

# 6H3: it doesn't differ at all, since the model with the lowest WAIC value got all the weight

# 6H4

oos.deviances = map(
  models, function(m) {
    
    dep_part = 0
    for(i in 2:(length(coef(m[[1]])) - 1)) {
      dep_part = dep_part + I(d2$age ** (i - 1)) * coef(m[[1]])[i]
      }
    mu = coef(m[[1]])[1] + dep_part
    sigma = coef(m[[1]])[length(coef(m[[1]]))]
  
    return(-2 * sum(dnorm(d2$height , mu , sigma , log=TRUE)))})
oos.deviances = unlist(oos.deviances)

# 6H5
oos.deviances - min(oos.deviances)
# dDEVIANCE: 545.6  261.0  55.7  0.0  1.8 1.0 (in order)
# dWAIC:     469.4  223.7  26.2  0.0  1.4 2.6 (in order)
# Quite amazingly, 5 and 6 now get inverted positions, and WAIC underestimates model 6's overfitting!
# But not that different. Also 4 is still the best, and the relations between it and 1, 2 and 3 (and
# between each pair of them) are still the same. Overall, WAIC does an amazing job of estimating the
# test deviance.

# 6H6
m6.6.2 = rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * age + b2 * I(age ** 2) + b3 * I(age ** 3) + b4 * I(age ** 4) + b5 * I(age ** 5) + b6 * I(age ** 6),
    a ~ dnorm(avg_height, 100),
    b1 ~ dnorm(0, 5),
    b2 ~ dnorm(0, 5),
    b3 ~ dnorm(0, 5),
    b4 ~ dnorm(0, 5),
    b5 ~ dnorm(0, 5),
    b6 ~ dnorm(0, 5),
    sigma ~ dunif(0, 200)
  ),
  data=d1
)
coefs = coef(m6.6.2)
# MAP estimates:
#            a           b1           b2           b3           b4           b5           b6        sigma 
# 155.85512885   5.97067477 -16.60220732  12.09547617  -3.51189205   0.23112925   0.05112099   8.19797381

m.link = link(m6.6.2, data=data.frame(age=age.cf), n=1e3)
mu = apply(m.link, 2, mean)
pi = apply(m.link, 2, PI, .97)

links = tibble(age=age.cf,
               mu=mu,
               pimin=pi[1, ],
               pimax=pi[2, ])

ggplot(links) +
  theme_light(base_family = 'Avenir') +
  geom_ribbon(aes(ymin=pimin, ymax=pimax, x=age), fill = "grey70", alpha=.3) +
  geom_line(aes(x=age, y=mu)) +
  scale_x_continuous(limits=c(-2, 3), breaks=seq(from=-2, to=3, by=1)) +
  geom_point(data=d1, aes(x=age, y=height), alpha=.1) +
  labs(x = 'Centered Age',
       y = 'Height',
       title = 'Height as a function of age')

a = d2$age
mu = coefs[1] + coefs[2] * a + coefs[3] * I(a ** 2) +
  coefs[4] * I(a ** 3) + coefs[5] * I(a ** 4) +
  coefs[6] * I(a ** 5) + coefs[7] * I(a ** 6)
sigma = coefs[8]
out.dev = -2 * sum(dnorm(d2$height , mu , sigma , log=TRUE))
out.dev - min(oos.deviances)
# dDEVIANCE = -1.102777
# It performs slightly better than the best WAIC model from earlier. The strongly regularizing priors
# avoided overfitting (which can be seen by the small values of b5 and b6 coefficients), by overwhelming
# the likelihoods ("making the golem skeptical").