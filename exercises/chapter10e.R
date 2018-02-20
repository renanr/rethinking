# EASY
# 1
# p = 0.35
log_odds = log(0.35 / (1 - 0.35))
log_odds

# 2
# log-odds = 3.2
p = exp(3.2) / (1 + exp(3.2))
p

# 3
plot(logistic(seq(from=-17, to=17, by=1.7)) ~ seq(21))
exp(1.7)

# 4
# Because frequencies might be expressed in different periods.
# For instance, RPG characters' damage in damage/hit or damage/second.

# MEDIUM
# 1
# The likelihood changes because the aggregated form loses information about order,
# so that is not accounted in the likelihood (but an average over all possible orders instead).

# 2
plot(exp(seq(from=-17, to=17, by=1.7)) ~ seq(21))
# lambda = e ^ (a + b * x)
# lambda_ratio = e ^ (a + b * (x + 1)) / e ^ (a + b * x) = exp(b)
exp(1.7)

# 3
# Because it maps the real numbers onto values between 0 and 1, matching them to probabilites.

# 4
# Because it maps the real numbers onto unbounded positive values, matching them to rates.

# 5
# It would imply that rates are bounded between 0 and 1. Could make sense if the exponential
# growth is believed to disappear at values larger than a threshold.

# 6
# Constraints: count outcome (discrete binary outcome) that comes from N trials with CONSTANT
# expected value n * p. The constraints are equal because Poisson is just a special case of
# the binomial.

# HARD
# 1
library(rethinking)
data(chimpanzees)
d <- chimpanzees

d2 <- d
d2$recipient <- NULL

m10.4mcmc <- map2stan(
                  alist(
                    pulled_left ~ dbinom( 1 , p ) ,
                    logit(p) <- a[actor] + (bp + bpC*condition)*prosoc_left ,
                    a[actor] ~ dnorm(0,10),
                    bp ~ dnorm(0,10),
                    bpC ~ dnorm(0,10)
                  ) ,
                  data=d2 , chains=2 , iter=2500 , warmup=500 )

m10.4map = map(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a[actor] + (bp + bpC*condition)*prosoc_left ,
    a[actor] ~ dnorm(0,10),
    bp ~ dnorm(0,10),
    bpC ~ dnorm(0,10)
  ) ,
  data=d2)

mcmcsamples = extract.samples(m10.4mcmc)
mapsamples = extract.samples(m10.4map)

ggplot() +
  geom_density(aes(mcmcsamples$a[, 1], colour=factor(1))) +
  geom_density(aes(mcmcsamples$a[, 3], colour=factor(3))) +
  geom_density(aes(mcmcsamples$a[, 4], colour=factor(4))) +
  geom_density(aes(mcmcsamples$a[, 5], colour=factor(5))) +
  geom_density(aes(mcmcsamples$a[, 6], colour=factor(6))) +
  geom_density(aes(mapsamples$a[, 1], colour=factor(1)), linetype=2) +
  geom_density(aes(mapsamples$a[, 3], colour=factor(3)), linetype=2) +
  geom_density(aes(mapsamples$a[, 4], colour=factor(4)), linetype=2) +
  geom_density(aes(mapsamples$a[, 5], colour=factor(5)), linetype=2) +
  geom_density(aes(mapsamples$a[, 6], colour=factor(6)), linetype=2)

ggplot() +
  geom_density(aes(mcmcsamples$a[, 2], colour=factor(2))) +
  geom_density(aes(mapsamples$a[, 2], colour=factor(2)), linetype=2)

ggplot() +
  geom_density(aes(mcmcsamples$a[, 7], colour=factor(7))) +
  geom_density(aes(mapsamples$a[, 7], colour=factor(7)), linetype=2)

# The similarities occurred when the probabilities were not near the borders (0 or 1),
# thus having somewhat precise logit values. But there was a huge difference when
# estimating for actor number 2, because he has a very high probability (very close to 1),
# thus it is not possible to indiscriminate between high values of the logit function.

# 2
m10.1 <- map2stan(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a ,
    a ~ dnorm(0,10)
  ) ,
  data=d )

m10.2 <- map2stan(
             alist(
               pulled_left ~ dbinom( 1 , p ) ,
               logit(p) <- a + bp*prosoc_left ,
               a ~ dnorm(0,10) ,
               bp ~ dnorm(0,10)
             ) ,
             data=d )

m10.3 <- map2stan(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a + (bp + bpC*condition)*prosoc_left ,
    a ~ dnorm(0,10) ,
    bp ~ dnorm(0,10) ,
    bpC ~ dnorm(0,10)
  ) ,
  data=d )

compare(m10.4mcmc, m10.3, m10.2, m10.1)

# The model with unique intercepts is estimated to be overwhelmingly better in out-of-sample
# deviance, getting all the akaike weight.

# 3
# (a)
library(MASS)
data(eagles)
e = eagles
e$P = ifelse(e$P == 'L', 1, 0)
e$A = ifelse(e$A == 'A', 1, 0)
e$V = ifelse(e$V == 'L', 1, 0)

me = map2stan(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- a + bP * P + bV * V + bA * A,
    a ~ dnorm(0, 10),
    c(bP, bA, bV) ~ dnorm(0, 5)
  ), data=e, iter=1500, warmup=400, chains=2
)

s = extract.samples(me)
ggplot() +
  geom_density(aes(s$a, colour=factor('a'))) +
  geom_density(aes(s$bP, colour=factor('bP'))) +
  geom_density(aes(s$bA, colour=factor('bA'))) +
  geom_density(aes(s$bV, colour=factor('bV')))

# bV is a left-(negative-)skewed, and bP is right-(positive-)skewed, so a quadratic
# approximation would not be okay.

# (b)
linke = link(me, e, 1e3)
ggplot() +
  geom_point(aes(x=seq(8), y=apply(linke, 2, mean))) +
  geom_errorbar(aes(x=seq(8), ymin=apply(linke, 2, HPDI, .89)[1, ],
                    ymax=apply(linke, 2, HPDI, .89)[2, ]))

sime = sim(me, e, n=1e3)
ggplot() +
  geom_point(aes(x=seq(8), y=apply(sime, 2, mean))) +
  geom_errorbar(aes(x=seq(8), ymin=apply(sime, 2, HPDI, .89)[1, ],
                    ymax=apply(sime, 2, HPDI, .89)[2, ]))

# the second one provides information about different number of attempts,
# the first one provides information that allows a more direct comparison
# between individuals.

# (c)

me2 = map2stan(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- a + bP * P + bV * V + (bA + bAP * P) * A,
    a ~ dnorm(0, 10),
    c(bP, bA, bV, bAP) ~ dnorm(0, 5)
  ), data=e, iter=1500, warmup=400, chains=2
)

compare(me2, me)
precis(me2)
# A little overfit, as we can see by the little weight attributed to the first model,
# but immensely more descriptive. The interaction is negative, so age is more relevant
# if the pirate is small.

# 4
data("salamanders")
ds = salamanders

# (a)
ms = map2stan(
  alist(
    salaman ~ dpois(lambda),
    log(lambda) <- a + b * pctcover,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 10)
  ),
  data=data.frame(salaman = ds$SALAMAN, pctcover = ds$PCTCOVER), iter=1500, warmup=300, chains=2
)

sms = extract.samples(ms)

ggplot() +
  geom_density(aes(sms$a, colour=factor('a')))

ggplot() +
  geom_density(aes(sms$b, colour=factor('b')))

# They're both pretty skewed, not a good idea to use quadratic approximations!

links = link(ms, data.frame(pctcover=ds$PCTCOVER), 1e3)
ggplot() +
  geom_point(aes(x=ds$PCTCOVER, y=apply(links, 2, mean), colour='PROJECTION')) +
  geom_errorbar(aes(x=ds$PCTCOVER, ymin=apply(links, 2, HPDI, .89)[1, ],
                    ymax=apply(links, 2, HPDI, .89)[2, ])) +
  geom_point(aes(x=ds$PCTCOVER, y=ds$SALAMAN, colour='REAL DEAL'))

# The model is okay for pctcover < 50%, but the errors are grotesque for
# pctcover > 75%.

# (b)
ms2 = map2stan(
  alist(
    salaman ~ dpois(lambda),
    log(lambda) <- a + bP * pctcover + bF * forestage,
    a ~ dnorm(0, 10),
    c(bP, bF) ~ dnorm(0, 10)
  ),
  data=data.frame(salaman = ds$SALAMAN,
                  pctcover = ds$PCTCOVER,
                  forestage = ds$FORESTAGE), iter=1500, warmup=300, chains=2,
  start=list(a=-1.5, bP=0, bF=0)
)

links2 = link(ms2, data.frame(pctcover=ds$PCTCOVER, forestage=ds$FORESTAGE), 1e3)
ggplot() +
  geom_point(aes(x=ds$PCTCOVER, y=apply(links2, 2, mean), colour='PROJECTION')) +
  geom_errorbar(aes(x=ds$PCTCOVER, ymin=apply(links2, 2, HPDI, .89)[1, ],
                    ymax=apply(links2, 2, HPDI, .89)[2, ])) +
  geom_point(aes(x=ds$PCTCOVER, y=ds$SALAMAN, colour='REAL DEAL'))

ggplot() +
  geom_point(aes(x=ds$FORESTAGE, y=apply(links2, 2, mean), colour='PROJECTION')) +
  geom_errorbar(aes(x=ds$FORESTAGE, ymin=apply(links2, 2, HPDI, .89)[1, ],
                    ymax=apply(links2, 2, HPDI, .89)[2, ])) +
  geom_point(aes(x=ds$FORESTAGE, y=ds$SALAMAN, colour='REAL DEAL'))

# FORESTAGE does not improve prediction at all. Let's try an interaction:

ms3 = map2stan(
  alist(
    salaman ~ dpois(lambda),
    log(lambda) <- a + bP * pctcover + (bF + bFP * pctcover) * forestage,
    a ~ dnorm(0, 10),
    c(bP, bF, bFP) ~ dnorm(0, 10)
  ),
  data=data.frame(salaman = ds$SALAMAN,
                  pctcover = ds$PCTCOVER,
                  forestage = ds$FORESTAGE), iter=1500, warmup=300, chains=2,
  start=list(a=-1.5, bP=0, bF=0, bFP=0)
)

links3 = link(ms3, data.frame(pctcover=ds$PCTCOVER, forestage=ds$FORESTAGE), 1e3)
ggplot() +
  geom_point(aes(x=ds$PCTCOVER, y=apply(links3, 2, mean), colour='PROJECTION')) +
  geom_errorbar(aes(x=ds$PCTCOVER, ymin=apply(links3, 2, HPDI, .89)[1, ],
                    ymax=apply(links3, 2, HPDI, .89)[2, ])) +
  geom_point(aes(x=ds$PCTCOVER, y=ds$SALAMAN, colour='REAL DEAL'))

# Forest age is just a poorer indicator of coverage, and the variable that directly
# influences salamander counts is coverage. That's why forest age doesn't help when we
# already have coverage.