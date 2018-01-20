library(rethinking)
data(reedfrogs)
d <- reedfrogs
str(d)

# make the tank cluster variable
d$tank <- 1:nrow(d)
# fit
m12.1 <- map2stan(
  alist(
    surv ~ dbinom( density , p ) ,
    logit(p) <- a_tank[tank] ,
    a_tank[tank] ~ dnorm( 0 , 5 )
  ),
  data=d )

m12.2 <- map2stan(
  alist(
    surv ~ dbinom( density , p ) ,
    logit(p) <- a_tank[tank] ,
    a_tank[tank] ~ dnorm( a , sigma ) ,
    a ~ dnorm(0,1) ,
    sigma ~ dcauchy(0,1)
    ), data=d , iter=4000 , chains=4 )

compare( m12.1 , m12.2 )

# extract Stan samples
post <- extract.samples(m12.2)
# compute median intercept for each tank
# also transform to probability with logistic
d$propsurv.est <- logistic( apply( post$a_tank , 2 , median ) )
# display raw proportions surviving in each tank
plot( d$propsurv , ylim=c(0,1) , pch=16 , xaxt="n" ,
      xlab="tank" , ylab="proportion survival" , col=rangi2 )
axis( 1 , at=c(1,16,32,48) , labels=c(1,16,32,48) )
# overlay posterior medians
points( d$propsurv.est )
# mark posterior median probability across tanks
abline( h=logistic(median(post$a)) , lty=2 )
# draw vertical dividers between tank densities
abline( v=16.5 , lwd=0.5 )
abline( v=32.5 , lwd=0.5 )
text( 8 , 0 , "small tanks" )
text( 16+8 , 0 , "medium tanks" )
text( 32+8 , 0 , "large tanks" )

# show first 100 populations in the posterior
plot( NULL , xlim=c(-3,4) , ylim=c(0,0.35) ,
      xlab="log-odds survive" , ylab="Density" )
for ( i in 1:100 )
  curve( dnorm(x,post$a[i],post$sigma[i]) , add=TRUE ,
         col=col.alpha("black",0.2) )
# sample 8000 imaginary tanks from the posterior distribution
sim_tanks <- rnorm( 8000 , post$a , post$sigma )
# transform to probability and visualize
dens( logistic(sim_tanks) , xlab="probability survive" )

a <- 1.4
sigma <- 1.5
nponds <- 60
ni <- as.integer( rep( c(5,10,25,35) , each=15 ) )
a_pond <- rnorm( nponds , mean=a , sd=sigma )
dsim <- data.frame( pond=1:nponds , ni=ni , true_a=a_pond )
dsim$si <- rbinom( nponds , prob=logistic(dsim$true_a) , size=dsim$ni )

dsim$p_nopool <- dsim$si / dsim$ni

m12.3 <- map2stan(
                  alist(
                    si ~ dbinom( ni , p ),
                    logit(p) <- a_pond[pond],
                    a_pond[pond] ~ dnorm( a , sigma ),
                    a ~ dnorm(0,1),
                    sigma ~ dcauchy(0,1)
                  ),
                  data=dsim , iter=1e4 , warmup=1000 )

precis(m12.3,depth=2)

estimated.a_pond <- as.numeric( coef(m12.3)[1:60] )
dsim$p_partpool <- logistic( estimated.a_pond )
dsim$p_true <- logistic( dsim$true_a )

nopool_error <- abs( dsim$p_nopool - dsim$p_true )
partpool_error <- abs( dsim$p_partpool - dsim$p_true )

plot( 1:60 , nopool_error , xlab="pond" , ylab="absolute error" ,
      col=rangi2 , pch=16 )
points( 1:60 , partpool_error )

a <- 1.4
sigma <- 1.5
nponds <- 60
ni <- as.integer( rep( c(5,10,25,35) , each=15 ) )
a_pond <- rnorm( nponds , mean=a , sd=sigma )
dsim <- data.frame( pond=1:nponds , ni=ni , true_a=a_pond )
dsim$si <- rbinom( nponds,prob=logistic( dsim$true_a ),size=dsim$ni )
dsim$p_nopool <- dsim$si / dsim$ni
newdat <- list(si=dsim$si,ni=dsim$ni,pond=1:nponds)
m12.3new <- map2stan( m12.3 , data=newdat , iter=1e4 , warmup=1000 )

# next section

y1 <- rnorm( 1e6 , 10 , 1 )
y2 <- 10 + rnorm( 1e6 , 0 , 1 )
ggplot() + geom_density(aes(y1, colour='All together')) + geom_density(aes(y2, colour='Mean +'))

library(rethinking)
data(chimpanzees)
d <- chimpanzees
d$recipient <- NULL # get rid of NAs
m12.4 <- map2stan(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a + a_actor[actor] + (bp + bpC*condition)*prosoc_left ,
    a_actor[actor] ~ dnorm( 0 , sigma_actor ),
    a ~ dnorm(0,10),
    bp ~ dnorm(0,10),
    bpC ~ dnorm(0,10),
    sigma_actor ~ dcauchy(0,1)
  ) ,
  data=d , warmup=1000 , iter=5000 , chains=4 , cores=3 )

plot(m12.4)
post <- extract.samples(m12.4)
ggplot() + geom_density(aes(post$sigma_actor))
total_a_actor <- sapply( 1:7 , function(actor) post$a + post$a_actor[,actor] )
round( apply(total_a_actor,2,mean) , 2 )

# prep data
d$block_id <- d$block # name 'block' is reserved by Stan
m12.5 <- map2stan(
  alist(
    pulled_left ~ dbinom( 1 , p ),
    logit(p) <- a + a_actor[actor] + a_block[block_id] + (bp + bpc*condition)*prosoc_left,
    a_actor[actor] ~ dnorm( 0 , sigma_actor ),
    a_block[block_id] ~ dnorm( 0 , sigma_block ),
    c(a,bp,bpc) ~ dnorm(0,10),
    sigma_actor ~ dcauchy(0,1),
    sigma_block ~ dcauchy(0,1)
  ) ,
  data=d, warmup=1000 , iter=6000 , chains=4 , cores=3 )

plot(m12.5)
precis(m12.5,depth=2) # depth=2 displays varying effects
plot(precis(m12.5,depth=2)) # also plot

post <- extract.samples(m12.5)
dens( post$sigma_block , xlab="sigma" , xlim=c(0,4) )
dens( post$sigma_actor , col=rangi2 , lwd=2 , add=TRUE )
text( 2 , 0.85 , "actor" , col=rangi2 )
text( 0.75 , 2 , "block" )

compare(m12.4,m12.5)

chimp <- 2
d.pred <- list(
  prosoc_left = c(0,1,0,1), # right/left/right/left
  condition = c(0,0,1,1), # control/control/partner/partner
  actor = rep(chimp,4)
)
link.m12.4 <- link( m12.4 , data=d.pred )
pred.p <- apply( link.m12.4 , 2 , mean )
pred.p.PI <- apply( link.m12.4 , 2 , PI )

post <- extract.samples(m12.4)
str(post)
dens( post$a_actor[,5] )
p.link <- function( prosoc_left , condition , actor ) {
  logodds <- with( post ,
                   a + a_actor[,actor] + (bp + bpC * condition) * prosoc_left
  )
  return( logistic(logodds) )
}
prosoc_left <- c(0,1,0,1)
condition <- c(0,0,1,1)
pred.raw <- sapply( 1:4 , function(i) p.link(prosoc_left[i],condition[i],2) )
pred.p <- apply( pred.raw , 2 , mean )
pred.p.PI <- apply( pred.raw , 2 , PI )

d.pred <- list(
  prosoc_left = c(0,1,0,1), # right/left/right/left
  condition = c(0,0,1,1), # control/control/partner/partner
  actor = rep(2,4) ) # placeholder
# replace varying intercept samples with zeros
# 1000 samples by 7 actors
a_actor_zeros <- matrix(0,1000,7)

# fire up link
# note use of replace list
link.m12.4 <- link( m12.4 , n=1000 , data=d.pred ,
                    replace=list(a_actor=a_actor_zeros) )
# summarize and plot
pred.p.mean <- apply( link.m12.4 , 2 , mean )
pred.p.PI <- apply( link.m12.4 , 2 , PI , prob=0.8 )
plot( 0 , 0 , type="n" , xlab="prosoc_left/condition" ,
      ylab="proportion pulled left" , ylim=c(0,1) , xaxt="n" ,
      xlim=c(1,4) )
axis( 1 , at=1:4 , labels=c("0/0","1/0","0/1","1/1") )
lines( 1:4 , pred.p.mean )
shade( pred.p.PI , 1:4 )

# replace varying intercept samples with simulations
post <- extract.samples(m12.4)
a_actor_sims <- rnorm(7000,0,post$sigma_actor)
a_actor_sims <- matrix(a_actor_sims,1000,7)

link.m12.4 <- link( m12.4 , n=1000 , data=d.pred ,
                    replace=list(a_actor=a_actor_sims) )

post <- extract.samples(m12.4)
sim.actor <- function(i) {
  sim_a_actor <- rnorm( 1 , 0 , post$sigma_actor[i] )
  P <- c(0,1,0,1)
  C <- c(0,0,1,1)
  p <- logistic(
    post$a[i] +
      sim_a_actor +
      (post$bp[i] + post$bpC[i]*C)*P
  )
  return(p)
}

# empty plot
plot( 0 , 0 , type="n" , xlab="prosoc_left/condition" ,
      ylab="proportion pulled left" , ylim=c(0,1) , xaxt="n" , xlim=c(1,4) )
axis( 1 , at=1:4 , labels=c("0/0","1/0","0/1","1/1") )
# plot 50 simulated actors
for ( i in 1:50 ) lines( 1:4 , sim.actor(i) , col=col.alpha("black",0.5) )

# prep data
library(rethinking)
data(Kline)
d <- Kline
d$logpop <- log(d$population)
d$society <- 1:10
# fit model
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

postcheck(m12.6)

post <- extract.samples(m12.6)
d.pred <- list(
  logpop = seq(from=6,to=14,length.out=30),
  society = rep(1,30)
)
a_society_sims <- rnorm(20000,0,post$sigma_society)
a_society_sims <- matrix(a_society_sims,2000,10)
link.m12.6 <- link( m12.6 , n=2000 , data=d.pred ,
                    replace=list(a_society=a_society_sims) )

# plot raw data
plot( d$logpop , d$total_tools , col=rangi2 , pch=16 ,
      xlab="log population" , ylab="total tools" )
# plot posterior median
mu.median <- apply( link.m12.6 , 2 , median )
lines( d.pred$logpop , mu.median )
# plot 97%, 89%, and 67% intervals (all prime numbers)
mu.PI <- apply( link.m12.6 , 2 , PI , prob=0.97 )
shade( mu.PI , d.pred$logpop )
mu.PI <- apply( link.m12.6 , 2 , PI , prob=0.89 )
shade( mu.PI , d.pred$logpop )
mu.PI <- apply( link.m12.6 , 2 , PI , prob=0.67 )
shade( mu.PI , d.pred$logpop )