# simulate a pancake and return randomly ordered sides
sim_pancake <- function() {
  pancake <- sample(1:3,1)
  sides <- matrix(c(1,1,1,0,0,0),2,3)[,pancake]
  sample(sides)
}

# sim 10,000 pancakes
pancakes <- replicate( 1e4 , sim_pancake() )
up <- pancakes[1,]
down <- pancakes[2,]
# compute proportion 1/1 (BB) out of all 1/1 and 1/0
num_11_10 <- sum( up==1 )
num_11 <- sum( up==1 & down==1 )
num_11/num_11_10

library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
# points
plot( d$Divorce ~ d$MedianAgeMarriage , ylim=c(4,15) ,
      xlab="Median age marriage" , ylab="Divorce rate" )
# standard errors
for ( i in 1:nrow(d) ) {
  ci <- d$Divorce[i] + c(-1,1)*d$Divorce.SE[i]
  x <- d$MedianAgeMarriage[i]
  lines( c(x,x) , ci )
}

dlist <- list(
              div_obs=d$Divorce,
              div_sd=d$Divorce.SE,
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
  div_sd=d$Divorce.SE,
  mar_obs=d$Marriage,
  mar_sd=d$Marriage.SE,
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

library(rethinking)
data(milk)
d <- milk
d$neocortex.prop <- d$neocortex.perc / 100
d$logmass <- log(d$mass)

# prep data
data_list <- list(
  kcal = d$kcal.per.g,
  neocortex = d$neocortex.prop,
  logmass = d$logmass )
# fit model
m14.3 <- map2stan(
  alist(
    kcal ~ dnorm(mu,sigma),
    mu <- a + bN*neocortex + bM*logmass,
    neocortex ~ dnorm(nu,sigma_N),
    a ~ dnorm(0,100),
    c(bN,bM) ~ dnorm(0,10),
    nu ~ dnorm(0.5,1),
    sigma_N ~ dcauchy(0,1),
    sigma ~ dcauchy(0,1)
  ) ,
  data=data_list , iter=1e4 , chains=2 )

precis(m14.3,depth=2)

# prep data
dcc <- d[ complete.cases(d$neocortex.prop) , ]
data_list_cc <- list(
  kcal = dcc$kcal.per.g,
  neocortex = dcc$neocortex.prop,
  logmass = dcc$logmass )
# fit model
m14.3cc <- map2stan(
  alist(
    kcal ~ dnorm(mu,sigma),
    mu <- a + bN*neocortex + bM*logmass,
    a ~ dnorm(0,100),
    c(bN,bM) ~ dnorm(0,10),
    sigma ~ dcauchy(0,1)
  ) ,
  data=data_list_cc , iter=1e4 , chains=2 )

precis(m14.3cc)

m14.4 <- map2stan(
  alist(
    kcal ~ dnorm(mu,sigma),
    mu <- a + bN*neocortex + bM*logmass,
    neocortex ~ dnorm(nu,sigma_N),
    nu <- a_N + gM*logmass,
    a ~ dnorm(0,100),
    c(bN,bM,gM) ~ dnorm(0,10),
    a_N ~ dnorm(0.5,1),
    sigma_N ~ dcauchy(0,1),
    sigma ~ dcauchy(0,1)
  ) ,
  data=data_list , iter=1e4 , chains=2 )

precis(m14.4,depth=2)

nc_missing <- ifelse( is.na(d$neocortex.prop) , 1 , 0 )
nc_missing <- sapply( 1:length(nc_missing) ,
                      function(n) nc_missing[n]*sum(nc_missing[1:n]) )
nc_missing

nc <- ifelse( is.na(d$neocortex.prop) , -1 , d$neocortex.prop )

model_code <- '
data{
  int N;
  int nc_num_missing;
  vector[N] kcal;
  real neocortex[N];
  vector[N] logmass;
  int nc_missing[N];
}
parameters{
  real alpha;
  real<lower=0> sigma;
  real bN;
  real bM;
  vector[nc_num_missing] nc_impute;
  real mu_nc;
  real<lower=0> sigma_nc;
}
model{
  vector[N] mu;
  vector[N] nc_merged;
  alpha ~ normal(0,10);
  bN ~ normal(0,10);
  bM ~ normal(0,10);
  mu_nc ~ normal(0.5,1);
  sigma ~ cauchy(0,1);
  sigma_nc ~ cauchy(0,1);
  // merge missing and observed
  for ( i in 1:N ) {
    nc_merged[i] <- neocortex[i];
    if ( nc_missing[i] > 0 ) nc_merged[i] <- nc_impute[nc_missing[i]];
  }
  // imputation
  nc_merged ~ normal( mu_nc , sigma_nc );
  // regression
  mu <- alpha + bN*nc_merged + bM*logmass;
  kcal ~ normal( mu , sigma );
}'

data_list <- list(
  N = nrow(d),
  kcal = d$kcal.per.g,
  neocortex = nc,
  logmass = d$logmass,
  nc_missing = nc_missing,
  nc_num_missing = max(nc_missing)
)
start <- list(
  alpha=mean(d$kcal.per.g), sigma=sd(d$kcal.per.g),
  bN=0, bM=0, mu_nc=0.68, sigma_nc=0.06,
  nc_impute=rep( 0.5 , max(nc_missing) )
)
library(rstan)
m14.3stan <- stan( model_code=model_code , data=data_list , init=list(start) ,
                   iter=1e4 , chains=1 )
