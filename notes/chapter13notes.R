a <- 3.5 # average morning wait time
b <- (-1) # average difference afternoon wait time
sigma_a <- 1 # std dev in intercepts
sigma_b <- 0.5 # std dev in slopes
rho <- (-0.7) # correlation between intercepts and slopes
Mu <- c( a , b )
cov_ab <- sigma_a*sigma_b*rho
Sigma <- matrix( c(sigma_a^2,cov_ab,cov_ab,sigma_b^2) , ncol=2 )

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

plot( a_cafe , b_cafe , col=rangi2 ,
      xlab="intercepts (a_cafe)" , ylab="slopes (b_cafe)" )
# overlay population distribution
library(ellipse)
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines(ellipse(Sigma,centre=Mu,level=l),col=col.alpha("black",0.2))

N_visits <- 10
afternoon <- rep(0:1,N_visits*N_cafes/2)
cafe_id <- rep( 1:N_cafes , each=N_visits )
mu <- a_cafe[cafe_id] + b_cafe[cafe_id]*afternoon
sigma <- 0.5 # std dev within cafes
wait <- rnorm( N_visits*N_cafes , mu , sigma )
d <- data.frame( cafe=cafe_id , afternoon=afternoon , wait=wait )

R <- rlkjcorr( 1e4 , K=2 , eta=2 )
dens( R[,1,2] , xlab="correlation" )

m13.1 <- map2stan(
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
  iter=5000 , warmup=2000 , chains=2 )

post <- extract.samples(m13.1)
dens( post$Rho[,1,2] )

# compute unpooled estimates directly from data
a1 <- sapply( 1:N_cafes ,
              function(i) mean(wait[cafe_id==i & afternoon==0]) )
b1 <- sapply( 1:N_cafes ,
              function(i) mean(wait[cafe_id==i & afternoon==1]) ) - a1
# extract posterior means of partially pooled estimates
post <- extract.samples(m13.1)
a2 <- apply( post$a_cafe , 2 , mean )
b2 <- apply( post$b_cafe , 2 , mean )
# plot both and connect with lines
plot( a1 , b1 , xlab="intercept" , ylab="slope" ,
      pch=16 , col=rangi2 , ylim=c( min(b1)-0.1 , max(b1)+0.1 ) ,
      xlim=c( min(a1)-0.1 , max(a1)+0.1 ) )
points( a2 , b2 , pch=1 )
for ( i in 1:N_cafes ) lines( c(a1[i],a2[i]) , c(b1[i],b2[i]) )

# compute posterior mean bivariate Gaussian
Mu_est <- c( mean(post$a) , mean(post$b) )
rho_est <- mean( post$Rho[,1,2] )
sa_est <- mean( post$sigma_cafe[,1] )
sb_est <- mean( post$sigma_cafe[,2] )
cov_ab <- sa_est*sb_est*rho_est
Sigma_est <- matrix( c(sa_est^2,cov_ab,cov_ab,sb_est^2) , ncol=2 )
# draw contours
library(ellipse)
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines(ellipse(Sigma_est,centre=Mu_est,level=l),
        col=col.alpha("black",0.2))

# convert varying effects to waiting times
wait_morning_1 <- (a1)
wait_afternoon_1 <- (a1 + b1)
wait_morning_2 <- (a2)
wait_afternoon_2 <- (a2 + b2)

plot( wait_morning_1 , wait_afternoon_1 , xlab="mor" , ylab="aft" ,
      pch=16 , col=rangi2 , ylim=c( min(wait_afternoon_1)-0.1 , max(wait_afternoon_1)+0.1 ) ,
      xlim=c( min(wait_morning_1)-0.1 , max(wait_morning_1)+0.1 ) )
points( wait_morning_2 , wait_afternoon_2 , pch=1 )
for ( i in 1:N_cafes ) lines( c(wait_morning_1[i],wait_morning_2[i]) , c(wait_afternoon_1[i],wait_afternoon_2[i]) )

library(rethinking)
data(UCBadmit)
d <- UCBadmit
d$male <- ifelse( d$applicant.gender=="male" , 1 , 0 )
d$dept_id <- coerce_index( d$dept )

m13.2 <- map2stan(
                  alist(
                    admit ~ dbinom( applications , p ),
                    logit(p) <- a_dept[dept_id] + bm*male,
                    a_dept[dept_id] ~ dnorm( a , sigma_dept ),
                    a ~ dnorm(0,10),
                    bm ~ dnorm(0,1),
                    sigma_dept ~ dcauchy(0,2)
                  ) ,
                  data=d , warmup=500 , iter=4500 , chains=3 )
precis( m13.2 , depth=2 ) # depth=2 to display vector parameters

m13.3 <- map2stan(
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
  data=d , warmup=1000 , iter=5000 , chains=4 , cores=3 )
plot( precis(m13.3,pars=c("a_dept","bm_dept"),depth=2) )

m13.4 <- map2stan(
  alist(
    admit ~ dbinom( applications , p ),
    logit(p) <- a_dept[dept_id],
    a_dept[dept_id] ~ dnorm( a , sigma_dept ),
    a ~ dnorm(0,10),
    sigma_dept ~ dcauchy(0,2)
  ) ,
  data=d , warmup=500 , iter=4500 , chains=3 )
compare( m13.2 , m13.3 , m13.4 )

library(rethinking)
data(chimpanzees)
d <- chimpanzees
d$recipient <- NULL
d$block_id <- d$block

m13.6 <- map2stan(
  alist(
    # likeliood
    pulled_left ~ dbinom(1,p),
    
    # linear models
    logit(p) <- A + (BP + BPC*condition)*prosoc_left,
    A <- a + a_actor[actor] + a_block[block_id],
    BP <- bp + bp_actor[actor] + bp_block[block_id],
    BPC <- bpc + bpc_actor[actor] + bpc_block[block_id],
    
    # adaptive priors
    c(a_actor,bp_actor,bpc_actor)[actor] ~
      dmvnorm2(0,sigma_actor,Rho_actor),
    c(a_block,bp_block,bpc_block)[block_id] ~
      dmvnorm2(0,sigma_block,Rho_block),
    
    # fixed priors
    c(a,bp,bpc) ~ dnorm(0,1),
    sigma_actor ~ dcauchy(0,2),
    sigma_block ~ dcauchy(0,2),
    Rho_actor ~ dlkjcorr(4),
    Rho_block ~ dlkjcorr(4)
  ) , data=d , iter=5000 , warmup=1000 , chains=3 , cores=3 )

m13.6NC <- map2stan(
                    alist(
                      pulled_left ~ dbinom(1,p),
                      logit(p) <- A + (BP + BPC*condition)*prosoc_left,
                      A <- a + a_actor[actor] + a_block[block_id],
                      BP <- bp + bp_actor[actor] + bp_block[block_id],
                      BPC <- bpc + bpc_actor[actor] + bpc_block[block_id],
                      # adaptive NON-CENTERED priors
                      c(a_actor,bp_actor,bpc_actor)[actor] ~
                        dmvnormNC(sigma_actor,Rho_actor),
                      c(a_block,bp_block,bpc_block)[block_id] ~
                        dmvnormNC(sigma_block,Rho_block),
                      c(a,bp,bpc) ~ dnorm(0,1),
                      sigma_actor ~ dcauchy(0,2),
                      sigma_block ~ dcauchy(0,2),
                      Rho_actor ~ dlkjcorr(4),
                      Rho_block ~ dlkjcorr(4)
                    ) , data=d , iter=5000 , warmup=1000 , chains=3 , cores=3 )

# extract n_eff values for each model
neff_c <- precis(m13.6,2)@output$n_eff
neff_nc <- precis(m13.6NC,2)@output$n_eff
# plot distributions
boxplot( list( 'm13.6'=neff_c , 'm13.6NC'=neff_nc ) ,
         ylab="effective samples" , xlab="model" )

precis( m13.6NC , depth=2 , pars=c("sigma_actor","sigma_block") )

p <- link(m13.6NC)
str(p)

m12.5 <- map2stan(
  alist(
    pulled_left ~ dbinom( 1 , p ),
    logit(p) <- a + a_actor[actor] + a_block[block_id] +
      (bp + bpc*condition)*prosoc_left,
    a_actor[actor] ~ dnorm( 0 , sigma_actor ),
    a_block[block_id] ~ dnorm( 0 , sigma_block ),
    c(a,bp,bpc) ~ dnorm(0,10),
    sigma_actor ~ dcauchy(0,1),
    sigma_block ~ dcauchy(0,1)
  ) ,
  data=d, warmup=1000 , iter=6000 , chains=4 , cores=3 )

compare( m13.6NC , m12.5 )

m13.6nc1 <- map2stan(
                     alist(
                       pulled_left ~ dbinom(1,p),
                       # linear models
                       logit(p) <- A + (BP + BPC*condition)*prosoc_left,
                       A <- a + za_actor[actor]*sigma_actor[1] +
                         za_block[block_id]*sigma_block[1],
                       BP <- bp + zbp_actor[actor]*sigma_actor[2] +
                         zbp_block[block_id]*sigma_block[2],
                       BPC <- bpc + zbpc_actor[actor]*sigma_actor[3] +
                         zbpc_block[block_id]*sigma_block[3],
                       # adaptive priors
                       c(za_actor,zbp_actor,zbpc_actor)[actor] ~ dmvnorm(0,Rho_actor),
                       c(za_block,zbp_block,zbpc_block)[block_id] ~ dmvnorm(0,Rho_block),
                       # fixed priors
                       c(a,bp,bpc) ~ dnorm(0,1),
                       sigma_actor ~ dcauchy(0,2),
                       sigma_block ~ dcauchy(0,2),
                       Rho_actor ~ dlkjcorr(4),
                       Rho_block ~ dlkjcorr(4)
                     ) ,
                     data=d ,
                     start=list( sigma_actor=c(1,1,1), sigma_block=c(1,1,1) ),
                     constraints=list( sigma_actor="lower=0", sigma_block="lower=0" ),
                     types=list( Rho_actor="corr_matrix", Rho_block="corr_matrix" ),
                     iter=5000 , warmup=1000 , chains=3 , cores=3 )

# load the distance matrix
library(rethinking)
data(islandsDistMatrix)
# display short column names, so fits on screen
Dmat <- islandsDistMatrix
colnames(Dmat) <- c("Ml","Ti","SC","Ya","Fi","Tr","Ch","Mn","To","Ha")
round(Dmat,1)

# linear
curve( exp(-1*x) , from=0 , to=4 , lty=2 ,
       xlab="distance" , ylab="correlation" )
# squared
curve( exp(-1*x^2) , add=TRUE )

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

precis(m13.7,depth=2)

post <- extract.samples(m13.7)
# plot the posterior median covariance function
curve( median(post$etasq)*exp(-median(post$rhosq)*x^2) , from=0 , to=10 ,
       xlab="distance (thousand km)" , ylab="covariance" , ylim=c(0,1) ,
       yaxp=c(0,1,4) , lwd=2 )
# plot 100 functions sampled from posterior
for ( i in 1:100 )
  curve( post$etasq[i]*exp(-post$rhosq[i]*x^2) , add=TRUE ,
         col=col.alpha("black",0.2) )

# compute posterior median covariance among societies
K <- matrix(0,nrow=10,ncol=10)
for ( i in 1:10 )
  for ( j in 1:10 )
    K[i,j] <- median(post$etasq) *
  exp( -median(post$rhosq) * islandsDistMatrix[i,j]^2 )
diag(K) <- median(post$etasq) + 0.01

# convert to correlation matrix
Rho <- round( cov2cor(K) , 2 )
# add row/col names for convenience
colnames(Rho) <- c("Ml","Ti","SC","Ya","Fi","Tr","Ch","Mn","To","Ha")
rownames(Rho) <- colnames(Rho)
Rho

# scale point size to logpop
psize <- d$logpop / max(d$logpop)
psize <- exp(psize*1.5)-2
# plot raw data and labels
plot( d$lon2 , d$lat , xlab="longitude" , ylab="latitude" ,
      col=rangi2 , cex=psize , pch=16 , xlim=c(-50,30) )
labels <- as.character(d$culture)
text( d$lon2 , d$lat , labels=labels , cex=0.7 , pos=c(2,4,3,3,4,1,3,2,4,2) )
# overlay lines shaded by Rho
for( i in 1:10 )
  for ( j in 1:10 )
    if ( i < j )
      lines( c( d$lon2[i],d$lon2[j] ) , c( d$lat[i],d$lat[j] ) ,
             lwd=2 , col=col.alpha("black",Rho[i,j]^2) )

# compute posterior median relationship, ignoring distance
logpop.seq <- seq( from=6 , to=14 , length.out=30 )
lambda <- sapply( logpop.seq , function(lp) exp( post$a + post$bp*lp ) )
lambda.median <- apply( lambda , 2 , median )
lambda.PI80 <- apply( lambda , 2 , PI , prob=0.8 )
# plot raw data and labels
plot( d$logpop , d$total_tools , col=rangi2 , cex=psize , pch=16 ,
      xlab="log population" , ylab="total tools" )
text( d$logpop , d$total_tools , labels=labels , cex=0.7 ,
      pos=c(4,3,4,2,2,1,4,4,4,2) )
# display posterior predictions
lines( logpop.seq , lambda.median , lty=2 )
lines( logpop.seq , lambda.PI80[1,] , lty=2 )
lines( logpop.seq , lambda.PI80[2,] , lty=2 )
# overlay correlations
for( i in 1:10 )
  for ( j in 1:10 )
    if ( i < j )
      lines( c( d$logpop[i],d$logpop[j] ) ,
             c( d$total_tools[i],d$total_tools[j] ) ,
             lwd=2 , col=col.alpha("black",Rho[i,j]^2) )