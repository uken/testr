testr
=====

testr is an R package for design and analysis of Bayesian A/B/../n tests. 

Currently, two models are implemented: 

- Beta binomial model for testing retention/conversion.
- Log-normal model for testing revenue. This model has two stages: modelling spenders vs. non-spenders with a beta-binomial model, and modelling revenue amongst spenders with a lognormal model. 

An early stopping [2] is used to determine when to end a test.

References

 [1] http://engineering.richrelevance.com/bayesian-ab-testing-with-a-log-normal-model/
 
 [2] http://www.bayesianwitch.com/blog/2014/bayesian_ab_test.html
