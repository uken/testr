plot_revenue_prior <- function(alpha0=1, beta0=1, s_sq0=1, v0=1, m0=1, k0=1, n=1e4, expected_revenue_converted_users=NULL, expected_conversion_rate=NULL, plot=T, resolution=35){      
  
  # density and random generation for the inverse gamma distribution (not part of base R)
  
  dinvgamma2 <- function(x,shape,scale=1){
    a <- shape
    b <- scale
    (b^a/gamma(a)) * x^(-a-1) * exp(-b/x)
  }
  
  rinvgamma2 <- function (n, shape, scale = 1) 1/rgamma(n = n, shape = shape, rate = scale)
  
  # parameterize in terms of expected_revenue_converted_users and expected_conversion_rate if they're provided
  
  if (!is.null(expected_conversion_rate)) beta0 <- 2 - alpha0 + (alpha0 - 1) / expected_conversion_rate
  
  if (!is.null(expected_revenue_converted_users)) m0 <- log(expected_revenue_converted_users)- .5 * v0 * s_sq0 / (v0 + 2)    
  
  ##      
  # log revenue - inv gamma prior over variance 
  s2 <- quantile(rinvgamma2(n, shape=v0/2, scale=v0*s_sq0/2), c(.1, .5, .9, .95))
    
  conversion_rate <- rbeta(n, shape1=alpha0, shape2=beta0)
  
  s_sq <- rinvgamma2(n, shape=v0/2, scale=v0 * s_sq0/2)
  
  mu <- rnorm(n, mean=m0, sd=sqrt(s_sq/k0)) 
  
  revenue_converted_users <- exp(mu + s_sq/2)
  
  ltv <- conversion_rate * revenue_converted_users
  
  if (plot){
    par(mfrow=c(3,2)) 
    
    curve(dinvgamma2(x, shape=v0/2, scale=v0*s_sq0/2), from=0, to=s2[4], n=resolution, lwd=2, xlab='variance', ylab='density', main='Inverse gamma prior over variance' )
    abline(v=s2[1], col='darkblue', lwd=2, lty=2)
    abline(v=s2[2], col='darkred', lwd=2, lty=2)
    abline(v=s2[3], col='darkgreen', lwd=2, lty=2)
    
    # log revenue - normal prior over mu conditioned on sigma^2 
    lim <- m0 + 3*c(-1,1)*sqrt(s2[3]/k0)
    
    curve(dnorm(x,mean=m0, sd=sqrt(s2[1]/k0)), xlim=lim, n=resolution, lwd=2, xlab='log revenue', ylab='density', col='darkblue', main='Normal prior over mean | variance')
    curve(dnorm(x,mean=m0, sd=sqrt(s2[2]/k0)), xlim=lim, add=T, n=resolution, lwd=2, col='darkred')
    curve(dnorm(x,mean=m0, sd=sqrt(s2[3]/k0)), xlim=lim, add=T, n=resolution, lwd=2, col='darkgreen')
    legend('topright', legend= paste('variance =', round(s2,1)), col=c('darkblue', 'darkred', 'darkgreen'), lwd=rep(2,3))
    
    hist(mu, xlab='Log (mean revenue) amongst converted users', main='Log (Mean LTV) amongst converted users')
    
    hist(revenue_converted_users, xlab='Mean revenue amongst converted users', main='Mean revenue amongst converted users')
    
    #conversion_rate - beta prior over conversion_rate rate
    curve(dbeta(x, shape1=alpha0, shape2=beta0), from=0, to=1, n=resolution, xlab='conversion_rate rate', ylab='density', lwd=2, main='Beta prior over the conversion_rate rate')
    
    hist(ltv, xlab='Mean revenue', main='Mean revenue')
  }else{
    graph <- hist(ltv, breaks=resolution, plot=F) 
    data.frame(graph$mids, graph$density)
  }
  
}