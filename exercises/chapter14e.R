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
m14.1 <- map2stan(
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
  WAIC=FALSE , iter=5000 , warmup=1000 , chains=2 , cores=2 ,
  control=list(adapt_delta=0.95) )

precis( m14.1 , depth=2 )

dlist <- list(
  div_obs=d$Divorce,
  div_sd=2 * d$Divorce.SE,
  mar_obs=d$Marriage,
  mar_sd=2 * d$Marriage.SE,
  A=d$MedianAgeMarriage )

m14.2 <- map2stan(
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
  WAIC=FALSE , iter=5000 , warmup=1000 , chains=3 , cores=3 ,
  control=list(adapt_delta=0.95) )