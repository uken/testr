testr
=====

testr is an R package for Bayesian design and analysis of  of randomized experiments (A/B/../n tests). It uses an adaptive design (i.e. early stopping rules), allowing tests to end as early as possible [1]. 

Two probability models are currently supported:

- Beta binomial model for testing retention/conversion.
- Zero-inflated log-normal model for testing revenue. This model has two stages: modelling spenders vs. non-spenders with a beta-binomial model, and modelling revenue amongst spenders with a lognormal model [2]. 

References

 [1] http://www.bayesianwitch.com/blog/2014/bayesian_ab_test.html
 [2] http://engineering.richrelevance.com/bayesian-ab-testing-with-a-log-normal-model
 
 

