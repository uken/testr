testr
=====

testr is an R package for Bayesian design and analysis of  of randomized experiments (A/B/../n tests). It uses an adaptive design (i.e. early stopping rules), allowing tests to end as early as possible [1]. 

Two probability models are currently supported:

- Beta binomial model for testing retention/conversion.
- Zero-inflated log-normal model for testing revenue. This model has two stages: modelling spenders vs. non-spenders with a beta-binomial model, and modelling revenue amongst spenders with a lognormal model [2]. 

References

 [1] http://www.bayesianwitch.com/blog/2014/bayesian_ab_test.html
 
 [2] http://engineering.richrelevance.com/bayesian-ab-testing-with-a-log-normal-model
 
Example Usage:

```
library(devtools)
install_github('uken/testr')
library(testr)

#test conversion with beta-binomial model
beta_binomial_ab_test(y=c(100,115), n=c(1000, 1000)) #flat prior
plot_conversion_prior(expected_conversion_rate=.1, alpha0=2) #try tweaking alpha0 
beta_binomial_ab_test(y=c(100,120), n=c(1000, 1000), expected_conversion_rate=.1, alpha0=2) #informative prior

#test revenue with zero-inflated lognormal model
A_data <- c(rep(0,1000), rlnorm(90, meanlog=0)) #revenue in group A
B_data <- c(rep(0,1000), rlnorm(110, meanlog=.05)) #revenue in group B
data <- data.frame(ab_group=c(rep('A', length(A_data)),rep('B', length(B_data))), ltv=c(A_data, B_data))

plot_revenue_prior(expected_conversion_rate=.1, expected_revenue_converted_users=1.5, s_sq0=1, v0=100, k0=100, alpha0=3)  #set a prior
lognormal_ab_test(data, expected_conversion_rate=.1, expected_revenue_converted_users=1.5, s_sq0=1, v0=100, k0=100, alpha0=3, plot.density = T)

```
