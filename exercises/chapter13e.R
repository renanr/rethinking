# EASY
# 1
# 
# y_i ~ Normal(mu_i, sigma)
# mu_i = alpha_group[i] + beta_group[i] * x_i
# [alpha_group, beta_group] ~ MVNormal([alpha, beta], S)
# S = [[sigma_alpha, 0], [0, sigma_beta]] * R * [[sigma_alpha, 0], [0, sigma_beta]]
# alpha ~ Normal(0, 10)
# beta ~ Normal(0, 1)
# sigma ~ HalfCauchy(0, 2)
# sigma_alpha ~ HalfCauchy(0, 2)
# sigma_beta ~ HalfCauchy(0, 2)
# R ~ LKJcorr(2)
# 
# 2
# 
# mu_dps = alpha_player + beta_player * has_support
# DPS may be an indicator of how good a player is. The better the player, the better it
# harness its support, so alpha and beta are positively correlated.
# 
# 3
# 
# When the slopes and intercepts are highly correlated. The model with unpooled slopes
# will be limited to 1 parameter for the slopes, sure enough, but this may lead to a
# bigger number of effective parameters for the varying intercepts, bigger enough to
# compensate the reduction of parameters when both slopes and intercepts are modeled
# together with their correlation.

# MEDIUM
# 1

a <- 3.5 # average morning wait time
b <- (-1) # average difference afternoon wait time
sigma_a <- 1 # std dev in intercepts
sigma_b <- 0.5 # std dev in slopes
rho <- (-0.7) # rho = 0 # correlation between intercepts and slopes
Mu <- c( a , b )
sigmas <- c(sigma_a,sigma_b) # standard deviations
Rho <- matrix( c(1,rho,rho,1) , nrow=2 ) # correlation matrix
# now matrix multiply to get covariance matrix
Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)
N_cafes <- 20
library(MASS)
set.seed(5) # used to replicate example
vary_effects <- mvrnorm( N_cafes , Mu , Sigma )
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]
N_visits <- 10 * 2
afternoon <- rep(0:1,N_visits*N_cafes/2)
cafe_id <- rep( 1:N_cafes , each=N_visits )
mu <- a_cafe[cafe_id] + b_cafe[cafe_id]*afternoon
sigma <- 0.5 # std dev within cafes
wait <- rnorm( N_visits*N_cafes , mu , sigma )
d <- data.frame( cafe=cafe_id , afternoon=afternoon , wait=wait )

m13.m1_original <- map2stan(
  alist(
    wait ~ dnorm( mu , sigma ),
    mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
    c(a_cafe,b_cafe)[cafe] ~ dmvnorm2(c(a,b),sigma_cafe,Rho),
    a ~ dnorm(0,10),
    b ~ dnorm(0,10),
    sigma_cafe ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ) ,
  data=d ,
  iter=1200 , warmup=300 , chains=2 )

m13.m1_corr0 <- map2stan(
  alist(
    wait ~ dnorm( mu , sigma ),
    mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
    c(a_cafe,b_cafe)[cafe] ~ dmvnorm2(c(a,b),sigma_cafe,Rho),
    a ~ dnorm(0,10),
    b ~ dnorm(0,10),
    sigma_cafe ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ) ,
  data=d ,
  iter=1200 , warmup=300 , chains=2 )

post_org <- extract.samples(m13.m1_original)
post_corr0 <- extract.samples(m13.m1_corr0)

ggplot() +
  geom_density(aes(post_org$Rho[,1,2]), linetype=1) +
  geom_density(aes(post_corr0$Rho[,1,2]), linetype=2)

# The posterior distribution of the correlation correctly reflects the change in the
# underlying mechanics! The distribution has shifted towards zero, symmetric around
# it.

# 2

m13.m2 <- map2stan(
  alist(
    wait ~ dnorm( mu , sigma ),
    mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
    a_cafe[cafe] ~ dnorm(a, sigma_a),
    b_cafe[cafe] ~ dnorm(b, sigma_b),
    a ~ dnorm(0,10),
    b ~ dnorm(0,10),
    sigma ~ dcauchy(0,1),
    sigma_a ~ dcauchy(0,1),
    sigma_b ~ dcauchy(0,1)
  ) ,
  data=d ,
  iter=1200 , warmup=300 , chains=2 )

compare(m13.m1_original, m13.m2)

# It is almost as good as the other? I don't understand.

# 3

library(rethinking)
data(UCBadmit)
d <- UCBadmit
d$male <- ifelse( d$applicant.gender=="male" , 1 , 0 )
d$dept_id <- coerce_index( d$dept )

m13.m3_original <- map2stan(
  alist(
    admit ~ dbinom( applications , p ),
    logit(p) <- a_dept[dept_id] +
      bm_dept[dept_id]*male,
    c(a_dept,bm_dept)[dept_id] ~ dmvnorm2( c(a,bm) , sigma_dept , Rho ),
    a ~ dnorm(0,10),
    bm ~ dnorm(0,1),
    sigma_dept ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ) ,
  data=d , warmup=500 , iter=2000 , chains=4 , cores=3 )

m13.m3_nc <- map2stan(
  alist(
    admit ~ dbinom( applications , p ),
    logit(p) <- a + a_dept[dept_id] +
      (bm + bm_dept[dept_id])*male,
    c(a_dept,bm_dept)[dept_id] ~ dmvnormNC(sigma_dept , Rho ),
    c(a,bm) ~ dnorm(0, 1),
    sigma_dept ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ) ,
  data=d , warmup=500 , iter=2000 , chains=4 , cores=3 )

# The original one is better and sampled faster, wtf.

# 4

library(rethinking)
data(islandsDistMatrix)
# display short column names, so fits on screen
Dmat <- islandsDistMatrix
colnames(Dmat) <- c("Ml","Ti","SC","Ya","Fi","Tr","Ch","Mn","To","Ha")

data(Kline2) # load the ordinary data, now with coordinates
d <- Kline2
d$society <- 1:10 # index observations
m13.7 <- map2stan(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + g[society] + bp*logpop,
    g[society] ~ GPL2( Dmat , etasq , rhosq , 0.01 ),
    a ~ dnorm(0,10),
    bp ~ dnorm(0,1),
    etasq ~ dcauchy(0,1),
    rhosq ~ dcauchy(0,1)
  ),
  data=list(
    total_tools=d$total_tools,
    logpop=d$logpop,
    society=d$society,
    Dmat=islandsDistMatrix),
  warmup=2000 , iter=1e4 , chains=4 )

d$logpop <- log(d$population)
m12.6 <- map2stan(
  alist(
    total_tools ~ dpois(mu),
    log(mu) <- a + a_society[society] + bp*logpop,
    a ~ dnorm(0,10),
    bp ~ dnorm(0,1),
    a_society[society] ~ dnorm(0,sigma_society),
    sigma_society ~ dcauchy(0,1)
  ),
  data=d ,
  iter=4000 , chains=3 )

compare(m13.7, m12.6)

#       WAIC pWAIC dWAIC weight   SE  dSE
# m13.7 67.3   4.0   0.0   0.81 2.15   NA
# m12.6 70.2   5.1   2.9   0.19 2.60 1.33
# 1.1 effective parameters less! This is awesome, the continuous categorization
# by distance regularized the model harder!

# HARD
# 1

data("bangladesh")
d = bangladesh
d$district_id <- as.integer(as.factor(d$district))

m13.h1 = map2stan(
  alist(
    use.contraception ~ dbinom(1, p),
    logit(p) <- a_district[district_id] + bu_district[district_id] * urban,
    c(a_district, bu_district)[district_id] ~ dmvnorm2(c(a, bu), sigma_district, R),
    a ~ dnorm(0, 10),
    bu ~ dnorm(0, 1),
    sigma_district ~ dcauchy(0, 1),
    R ~ dlkjcorr(2)),
  data=d, iter=1500, warmup=300, chains=2
)

post = extract.samples(m13.h1)
rho = post$R[,1,2]
dens(rho)
bus = apply(post$bu_district, 2, median)
as = apply(post$a_district, 2, median)
ds = seq(length(as))
  
ggplot() +
  geom_point(aes(y=logistic(bus + as), x=ds, colour='Urban')) +
  geom_point(aes(y=logistic(as), x=ds, colour='Rural')) +
  geom_segment(aes(x=ds, xend=ds, y=logistic(as), yend=logistic(bus + as)))

# In a majority of districts, urban levels of contraception use were higher than
# rural ones. Also, the lower the level in the rural area, the bigger the difference
# to the urban one.

# 2

data("Oxboys")
d = Oxboys

m13.h2 = map2stan(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a_subject[Subject] + b_subject[Subject] * age,
    c(a_subject, b_subject)[Subject] ~ dmvnorm2(c(a, b), sigma_subject, Rho),
    a ~ dnorm(150, 15),
    b ~ dnorm(0, 1),
    c(sigma, sigma_subject) ~ dcauchy(0, 1),
    Rho ~ dlkjcorr(2)),
  data=d, iter=1500, warmup=300, chains=2, control=list(adapt_delta=0.95)
)

precis(m13.h2, depth=2)
8.3 / 140
1.86 / 8
# the slopes vary a lot more!

# 3

post = extract.samples(m13.h2)
rho = post$R[,1,2]
dens(rho)

# A very high (positive) correlation. I think this is the case because we start
# measuring heights when the growing process has already been going for awhile,
# and it is apparently pretty stable during the growing period. Therefore,
# the taller boys are product of more intense growths, which in turn will make
# them grow even more. It would make me predict more intense growths for taller
# boys.

# 4

library(MASS)
post_a = post$a[1:10]
post_b = post$b[1:10]
sigma_a = post$sigma_subject[1:10, 1]
sigma_b = post$sigma_subject[1:10, 2]
rho = post$R[1:10,1,2]
students = list()
for(i in seq(10)) {
  S = matrix( c( sigma_a[i]^2 ,
                 sigma_a[i]*sigma_b[i]*rho[i] ,
                 sigma_a[i]*sigma_b[i]*rho[i] ,
                 sigma_b[i]^2 ) , nrow=2 )
  students[[i]] = mvrnorm(n=1, mu=c(post_a[i], post_b[i]), Sigma=S)
}

age = d$age[1:9]
height = seq(130, 180, length.out=9)
plot(height ~ ages, col='white')
for(i in seq(10)) {
  abline(a=students[[i]][1], b=students[[i]][2])
}
