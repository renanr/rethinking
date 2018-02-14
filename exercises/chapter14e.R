# EASY
# 1
# 
# T_i ~ Poisson(mu_i)
# log(mu_i) = alpha + beta * log(P_i)_est
# log(P_i)_obs ~ dnorm(log(P_i)_est, sigma_P)
# alpha ~ Normal(0, 10)
# beta ~ Normal(0, 1)
# sigma_P ~ HalfCauchy(0, 1)

# 2
# 
# T_i ~ Poisson(mu_i)
# log(mu_i) = alpha + beta * log(P_i)
# log(P_i) ~ dnorm(nu, sigma_P)
# alpha ~ Normal(0, 10)
# beta ~ Normal(0, 1)
# nu ~ Normal(8, 3)
# sigma_P ~ HalfCauchy(0, 2)

# MEDIUM
# 1
# 
# It is being assumed that the values have fixed variance and were randomly missing.
# (Normal distribution of missing values)

# 2

library(rethinking)
data(milk)

dc = milk
dc$neocortex = dc$neocortex.perc / 100
dc$logmass = log(dc$mass)

m14.m2_ = map2stan(
  alist(
    kcal ~ dnorm(mu, sigma),
    mu <- a,
    a ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 2)),
  data=list(
    kcal = dc$kcal.per.g),
  iter=1500, warmup=300, chains=2
)

m14.m2_m = map2stan(
  alist(
    kcal ~ dnorm(mu, sigma),
    mu <- a + bm * logmass,
    a ~ dnorm(0, 10),
    bm ~ dnorm(0, 1),
    sigma ~ dcauchy(0, 2)),
  data=list(
    kcal = dc$kcal.per.g,
    logmass = dc$logmass),
  iter=1500, warmup=300, chains=2
)

m14.m2_n = map2stan(
  alist(
    kcal ~ dnorm(mu, sigma),
    mu <- a + bn * neocortex,
    a ~ dnorm(0, 10),
    bn ~ dnorm(0, 1),
    neocortex ~ dnorm(nu, sigma_neo),
    sigma ~ dcauchy(0, 2),
    nu ~ dnorm(.5, .25),
    sigma_neo ~ dcauchy(0, 2)),
  data=list(
    kcal = dc$kcal.per.g,
    neocortex = dc$neocortex),
  iter=1500, warmup=300, chains=2
)

m14.m2_nm = map2stan(
  alist(
    kcal ~ dnorm(mu, sigma),
    mu <- a + bn * neocortex + bm * logmass,
    a ~ dnorm(0, 10),
    bn ~ dnorm(0, 1),
    neocortex ~ dnorm(nu, sigma_neo),
    bm ~ dnorm(0, 1),
    sigma ~ dcauchy(0, 2),
    nu ~ dnorm(.5, .25),
    sigma_neo ~ dcauchy(0, 2)),
  data=list(
    kcal = dc$kcal.per.g,
    neocortex = dc$neocortex,
    logmass = dc$logmass),
  iter=1500, warmup=300, chains=2
)

compare(m14.m2_, m14.m2_m, m14.m2_n, m14.m2_nm)
#            WAIC pWAIC dWAIC weight   SE  dSE
# m14.m2_nm -28.0   4.8   0.0   0.91 5.71   NA
# m14.m2_m  -22.5   2.1   5.6   0.06 5.68 2.10
# m14.m2_   -20.7   1.5   7.4   0.02 5.36 3.97
# m14.m2_n  -19.9   2.3   8.1   0.02 5.07 4.15
# They were not strongly affected, but the complete model weight
# got a little smaller, and the mass-only model became slightly
# more important.

# 3

library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

dlist <- list(
  div_obs=d$Divorce,
  div_sd=2 * d$Divorce.SE,
  R=d$Marriage,
  A=d$MedianAgeMarriage
)
m14.1e <- map2stan(
  alist(
    div_est ~ dnorm(mu,sigma),
    mu <- a + bA*A + bR*R,
    div_obs ~ dnorm(div_est,div_sd),
    a ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2.5)
  ) ,
  data=dlist ,
  start=list(div_est=dlist$div_obs) ,
  WAIC=FALSE , iter=1500 , warmup=300 , chains=2 , cores=2 ,
  control=list(adapt_delta=0.95) )

dlist <- list(
  div_obs=d$Divorce,
  div_sd=2 * d$Divorce.SE,
  mar_obs=d$Marriage,
  mar_sd=2 * d$Marriage.SE,
  A=d$MedianAgeMarriage )

m14.2e <- map2stan(
  alist(
    div_est ~ dnorm(mu,sigma),
    mu <- a + bA*A + bR*mar_est[i],
    div_obs ~ dnorm(div_est,div_sd),
    mar_obs ~ dnorm(mar_est,mar_sd),
    a ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2.5)
  ) ,
  data=dlist ,
  start=list(div_est=dlist$div_obs,mar_est=dlist$mar_obs) ,
  WAIC=FALSE , iter=1500 , warmup=300 , chains=2 , cores=2 ,
  control=list(adapt_delta=0.95) )

precis(m14.2, depth=2)
precis(m14.2e, depth=2)

# I do not see a big difference in inference...

# HARD
# 1

library(rethinking)
data(elephants)
d = elephants

m14.h1.a = map2stan(
  alist(
    MATINGS ~ dpois(lambda),
    log(lambda) <- a + b * AGE,
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1)),
  data=d, iter=1500, warmup=300, chains=2)

m14.h1.b = map2stan(
  alist(
    MATINGS ~ dpois(lambda),
    log(lambda) <- a + b * AGE_est[i],
    AGE ~ dnorm(AGE_est, 5),
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1)),
  data=d, iter=1500, warmup=300, chains=2,
  start=list(AGE_est=d$AGE))

precis(m14.h1.a)
precis(m14.h1.b)

# No sensible difference in the relationship was detected.

# 2
m14.h1.50 = map2stan(
  alist(
    MATINGS ~ dpois(lambda),
    log(lambda) <- a + b * AGE_est[i],
    AGE ~ dnorm(AGE_est, 50),
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1)),
  data=d, iter=1500, warmup=300, chains=2,
  start=list(AGE_est=d$AGE))

precis(m14.h1.50)

m14.h1.100 = map2stan(
  alist(
    MATINGS ~ dpois(lambda),
    log(lambda) <- a + b * AGE_est[i],
    AGE ~ dnorm(AGE_est, 100),
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1)),
  data=d, iter=1500, warmup=300, chains=2,
  start=list(AGE_est=d$AGE))

precis(m14.h1.100)

m14.h1.75 = map2stan(
  alist(
    MATINGS ~ dpois(lambda),
    log(lambda) <- a + b * AGE_est[i],
    AGE ~ dnorm(AGE_est, 75),
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1)),
  data=d, iter=1500, warmup=300, chains=2,
  start=list(AGE_est=d$AGE))

precis(m14.h1.75)

# It has to be around 67,5!

# 3

set.seed(100)
x <- c( rnorm(10) , NA )
y <- c( rnorm(10,x) , 100 )
d <- list(x=x,y=y)

d.cc = data.frame(d)[complete.cases(d), ]
m.cc = map(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + b * x,
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1),
    sigma ~ dcauchy(0, 1)),
  data=data.frame(d.cc))
precis(m.cc)

post.cc = extract.samples(m.cc)
dens(post.cc$b)

m14.h3 = map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + b * x,
    x ~ dnorm(0, 1),
    a ~ dnorm(0, 100),
    b ~ dnorm(0, 100),
    sigma ~ dcauchy(0, 1)),
  data=d, iter=1500, warmup=300, chains=2
)

post = extract.samples(m14.h3)
dens(post$b)

# Now there is a considerable mass for high negative values (even as high as -40).