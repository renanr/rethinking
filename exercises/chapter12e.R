# CHAPTER 12
# EASY
# 1
# Normal(0, 1) will produce more shrinkage, as it's more tightly concentrated around 0.

# 2
# Using mathematical (and not programming) notation:
# y_i ~ Binomial(1, p_i)
# logit(p_i) = a_(group[i]) + b * x_i
# a_group ~ Normal(0, 10)
# b ~ Normal(0, 1)
# 
# Turning into a multilevel model:
# y_i ~ Binomial(1, p_i)
# logit(p_i) = a_(group[i]) + b * x_i
# a_group ~ Normal(a, sigma)
# a ~ Normal(0, 10)
# sigma ~ HalfCauchy(0, 1)
# b ~ Normal(0, 1)

# 3
# y_i ~ Normal(mu_i, sigma)
# mu_i = a_(group[i]) + b * x_i
# a_group ~ Normal(0, 10)
# b ~ Normal(0, 1)
# sigma ~ HalfCauchy(0, 2)
# 
# Turning into a multilevel model:
# y_i ~ Normal(mu_i, sigma)
# mu_i = a_(group[i]) + b * x_i
# a_group ~ Normal(a, sigma_a)
# a ~ Normal(0, 10)
# b ~ Normal(0, 1)
# sigma ~ HalfCauchy(0, 2)
# sigma_a ~ HalfCauchy(0, 1)

# 4
# y_i ~ Poisson(lambda_i)
# log(lambda_i) = a_(x[i]) + b * x_i
# b ~ Normal(0, 1)
# a_x ~ Normal(a, sigma)
# a ~ Normal(0, 10)
# sigma ~ HalfCauchy(0, 1)

# 5
# y_i ~ Poisson(lambda_i)
# log(lambda_i) = a + a_(x[i]) + a_(y[i]) + b * z
# b ~ Normal(0, 1)
# a_x ~ Normal(0, sigma_x)
# a_y ~ Normal(0, sigma_y)
# a ~ Normal(0, 10)
# sigma_x ~ HalfCauchy(0, 1)
# sigma_y ~ HalfCauchy(0, 1)

# MEDIUM
# 1
library(rethinking)
data(reedfrogs)
d <- reedfrogs
d$tank <- 1:nrow(d)
d$pred = ifelse(d$pred == 'no', 0, 1)
d$is_big = ifelse(d$size == 'big', 1, 0)

# m12.3 <- map2stan(
#     alist(
#         surv ~ dbinom( density , p ),
#         logit(p) <- a_tank[tank],
#         a_tank[tank] ~ dnorm( a , sigma ),
#         a ~ dnorm(0,1),
#         sigma ~ dcauchy(0,1)
#     ),
#     data=d , iter=1e4 , warmup=1000 )

# pred only
m1pred <- map2stan(
  alist(
    surv ~ dbinom( density , p ),
    logit(p) <- a_tank[tank] + bp * pred,
    a_tank[tank] ~ dnorm( a , sigma ),
    a ~ dnorm(0,1),
    sigma ~ dcauchy(0,1),
    bp ~ dnorm(0, 1)
  ),
  data=d , iter=1e4 , warmup=1000 )

# size only
m1size <- map2stan(
  alist(
    surv ~ dbinom( density , p ),
    logit(p) <- a_tank[tank] + bs * is_big,
    a_tank[tank] ~ dnorm( a , sigma ),
    a ~ dnorm(0,1),
    sigma ~ dcauchy(0,1),
    bs ~ dnorm(0, 1)
  ),
  data=d , iter=1e4 , warmup=1000 )

# pred and size
m1predsize <- map2stan(
  alist(
    surv ~ dbinom( density , p ),
    logit(p) <- a_tank[tank] + bp * pred + bs * is_big,
    a_tank[tank] ~ dnorm( a , sigma ),
    a ~ dnorm(0,1),
    sigma ~ dcauchy(0,1),
    bp ~ dnorm(0, 1),
    bs ~ dnorm(0, 1)
  ),
  data=d , iter=1e4 , warmup=1000 )

# pred, size and interaction
m1complete <- map2stan(
  alist(
    surv ~ dbinom( density , p ),
    logit(p) <- a_tank[tank] + bp * pred + (bs + bps * pred) * is_big ,
    a_tank[tank] ~ dnorm( a , sigma ),
    a ~ dnorm(0,1),
    sigma ~ dcauchy(0,1),
    bp ~ dnorm(0, 1),
    bs ~ dnorm(0, 1),
    bps ~ dnorm(0, 1)
  ),
  data=d , iter=1e4 , warmup=1000 )

p = extract.samples(m1pred)
s = extract.samples(m1size)
ps = extract.samples(m1predsize)
psi = extract.samples(m1complete)
samples = tibble(a = c(p$a, s$a, ps$a, psi$a), sigma = c(p$sigma, s$sigma, ps$sigma, psi$sigma), model = c(rep('p', 1e4 - 1e3), rep('s', 1e4 - 1e3), rep('ps', 1e4 - 1e3), rep('psi', 1e4 - 1e3)))
ggplot(samples) + geom_density(aes(sigma, colour=model)) + geom_density(aes(a, colour=model, fill=1, alpha=TRUE))

# if I include information about predators (mostly), I reduce a lot of the variance among intercepts
# and also narrows the posterior of sigma. This means a lot of variance among tanks is due to predators,
# if I consider them, the tanks are much more uniform. This could also mean overfitting! WATCH OUT

# 2
compare(m1pred, m1size, m1predsize, m1complete)
# 'size' alone doen't convey much information. It really tells about the influence of predators,
# although it tends to overfit a bit: that's why predator alone still gets a big chunk of Akaike weight.

# 3

m12.3 <- map2stan(
  alist(
    surv ~ dbinom( density , p ),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dnorm( a , sigma ),
    a ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ),
  data=d , iter=1e4 , warmup=1000 )

m3 = map2stan(
  alist(
    surv ~ dbinom( density , p ),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dcauchy( a , sigma ),
    a ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ),
  data=d , iter=1e4 , warmup=1000 )
ggplot() + geom_point(aes(x=1:48, y=apply(s.c$a_tank, 2, mean), colour='chapter')) + geom_point(aes(x=1:48, y=apply(s.e$a_tank, 2, mean), colour='exercise'))
# Cauchy is thick-tailed. Bizarrely high values are still allowed with relatively high probabilities.

# 4

library(rethinking)
data(chimpanzees)
d <- chimpanzees
d$recipient <- NULL # get rid of NAs
d$block_id <- d$block  # name 'block' is reserved by Stan

m4c = map2stan(
  alist(
    pulled_left ~ dbinom( 1 , p ),
    logit(p) <- a + a_actor[actor] + a_block[block_id] + (bp + bpc*condition)*prosoc_left,
    a_actor[actor] ~ dnorm( 0 , sigma_actor ),
    a_block[block_id] ~ dnorm( 0 , sigma_block ),
    c(a,bp,bpc) ~ dnorm(0,10),
    sigma_actor ~ dcauchy(0,1),
    sigma_block ~ dcauchy(0,1)
  ),
  data=d, warmup=1000 , iter=6000 , chains=4 , cores=3 )

m4e = map2stan(
  alist(
    pulled_left ~ dbinom( 1 , p ),
    logit(p) <- a_actor[actor] + a_block[block_id] + (bp + bpc*condition)*prosoc_left,
    a_actor[actor] ~ dnorm( a , sigma_actor ),
    a_block[block_id] ~ dnorm( c , sigma_block ),
    c(a,c,bp,bpc) ~ dnorm(0,10),
    sigma_actor ~ dcauchy(0,1),
    sigma_block ~ dcauchy(0,1)
  ),
  data=d, warmup=1000 , iter=6000 , chains=4 , cores=3 )

precis(m4c)
precis(m4e)
# `a` and `c` are strongly correlated, they encode pretty much the same information, but if both are
# present the system gets undefined (one extra DoF). It's interesting to note how this difficults
# sampling, there are much less n_eff and Rhat gets a little problematic too.

# HARD
# 1

data("bangladesh")
d = bangladesh
d$district_id <- as.integer(as.factor(d$district))

library(tidyverse)

t = as.tibble(d) %>%
  group_by(district_id) %>%
  summarise(contra = sum(use.contraception),
            count = n()) %>%
  na.omit %>%
  data.frame

f_e_model = rethinking::map(
  alist(
    contra ~ dbinom(count, p),
    logit(p) <- a[district_id],
    a[district_id] ~ dnorm(-.53, 1)
  ), data=t)

v_e_model = rethinking::map2stan(
  alist(
    contra ~ dbinom(count, p),
    logit(p) <- a_dis[district_id],
    a_dis[district_id] ~ dnorm(a, sigma),
    a ~ dnorm(-.53, 1),
    sigma ~ dcauchy(0, 1)
  ), data=t, start=list(a=-.53, sigma=1),
  chains=2 , iter=4000 , warmup=1000)

post_f = extract.samples(f_e_model)
post_v = extract.samples(v_e_model)

comparison = tibble(
  district_id = t$district_id,
  f = logistic(apply(post_f$a, 2, mean)),
  v = logistic(apply(post_v$a_dis, 2, mean)),
  t = t$contra / t$count)

ggplot(comparison, aes(x=district_id)) +
  geom_point(aes(y=t, x=district_id, colour='true'), alpha=.5) +
  geom_point(aes(y=f, x=district_id, colour='fixed'), alpha=.5) +
  geom_point(aes(y=v, x=district_id, colour='varyi'), alpha=.5) +
  geom_segment(aes(y=t, yend=v, x=district_id, xend=district_id, colour='varyi')) +
  geom_segment(aes(y=t, yend=f, x=district_id, xend=district_id, colour='fixed'))

# The v-i model gets more shrunk towards the mean, because it regularizes the intercepts through partial pooling, and so the differences are more extreme
# where the values are further away from the total mean.

# 2
data("Trolley")
d = Trolley
m11.1stan <- map2stan(
  alist(
    response ~ dordlogit( phi , cutpoints ),
    phi <- ba * action + bi * intention + bc * contact,
    c(ba, bi, bc) ~ dnorm(0, 10),
    cutpoints ~ dnorm(0,10)
  ),
  data=list(response=d$response, action=d$action, intention=d$intention, contact=d$contact),
  start=list(cutpoints=c(-2,-1,0,1,2,2.5)) , chains=2 , cores=2 )

mv = map2stan(
  alist(
    response ~ dordlogit( phi , cutpoints ),
    phi <- ba * action + bi * intention + bc * contact + a_id[id],
    a_id[id] ~ dnorm(a, sigma),
    a ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 1),
    c(ba, bi, bc) ~ dnorm(0, 10),
    cutpoints ~ dnorm(0,10)
  ),
  data=list(response=d$response, action=d$action, intention=d$intention, contact=d$contact, id=d$id),
  start=list(cutpoints=c(-2,-1,0,1,2,2.5)) , chains=2 , cores=2 )

compare(m11.1stan, mv)

# Using WAIC: mv gets all of the weight, which means that individual variation is the overwhelmingly
# biggest source of variation.

# 3

mvs = map2stan(
  alist(
    response ~ dordlogit( phi , cutpoints ),
    phi <- ba * action + bi * intention + bc * contact + a + a_id[id] + a_story[story],
    a_id[id] ~ dnorm(0, sigma_id),
    a_story[story] ~ dnorm(0, sigma_story),
    a ~ dnorm(0, 10),
    sigma_id ~ dcauchy(0, 1),
    sigma_story ~ dcauchy(0, 1),
    c(ba, bi, bc) ~ dnorm(0, 10),
    cutpoints ~ dnorm(0,10)
  ),
  data=list(response=d$response, action=d$action, intention=d$intention, contact=d$contact, id=d$id, story=d$story),
  start=list(cutpoints=c(-2,-1,0,1,2,2.5)) , chains=2 , cores=2 )

compare(m11.1stan, mv, mvs)

# It's overwhelmingly higher-ranking on WAIC criterium! It gets all the akaike weight.
# This means that the stories are responsible for a lot of the variance in the responses!