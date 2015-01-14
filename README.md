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

beta_binomial_ab_test(y=c(80,90), n=c(1000, 1000))

beta_binomial_ab_test(y=c(80,90), n=c(1000, 1000))

A_data <- c(rep(0,1000), rlnorm(50, meanlog=0))
B_data <- c(rep(0,1000), rlnorm(80, meanlog=.1))
data <- data.frame(ab_group=c(rep('A', length(A_data)),rep('B', length(B_data))), ltv=c(A_data, B_data))
lognormal_ab_test(data, plot.density = T)

```
