testr: Tools for Bayesian analysis of A/B tests
===============================================

`testr` is an R package for Bayesian design and analysis of randomized experiments (A/B tests). It uses an adaptive design (i.e. optional stopping rules), allowing tests to end as early as possible [5]. 

Two probability models are currently supported:

1. Beta binomial model for testing retention/conversion [1], [7]
2. Zero-inflated log-normal model for testing revenue. This model has two stages: modelling spenders vs. non-spenders with a beta-binomial model, and modelling revenue amongst spenders with a lognormal model [3]. 

`testr` provides a convenient API to these models, and extends them in several ways:

* Generalized the early stopping rule [5] to the Zero-inflated log-normal model
* Uses Monte Carlo integration of the posterior to evaluate stopping rules
* Implements methods for visualizing the prior/posterior distributions

## Example Usage

Install the package using devtools:

```R
# install.packages('devtools')
devtools::install_github('uken/testr')
library(testr)
?testr
```

To run a simple test for conversion with a Beta-Binomial model:

```R
beta_binomial_ab_test(y = c(100,115), n = c(1000, 1000)) # flat prior
plot_conversion_prior(expected_conversion_rate = .1, alpha0 = 2) # try tweaking alpha0 to decrase variance
beta_binomial_ab_test(y = c(100,115), n = c(1000, 1000), expected_conversion_rate = .1, alpha0 = 2) # informative prior
```

To run a test for revenue with a zero-inflated lognormal model:

```R
# Simulate some data from an A/B/C test:
n <- 10000
A_data <- rbinom(n, 1, conversion) * rlnorm(n, meanlog = 0, sdlog = 0.2)
B_data <- rbinom(n, 1, conversion) * rlnorm(n, meanlog = 0.08, sdlog = 0.2)
C_data <- rbinom(n, 1, conversion) * rlnorm(n, meanlog = 0.15, sdlog = 0.2)
data <- data.frame(ab_group = rep(c('A', 'B', 'C'), each = n), ltv = c(A_data, B_data, C_data))

# Plot the priors and run the test:
plot_revenue_prior(expected_conversion_rate = 0.12, alpha0 = 15, expected_revenue_converted_users = 1.5, v0 = 73, k0 = 100, s_sq0 = 1.2) # specify prior
l  =  lognormal_ab_test(data, expected_conversion_rate = 0.65, alpha0 = 15, expected_revenue_converted_users = 1.5, v0 = 73, k0 = 100, s_sq0 = 1.2)
plot(l)
```

## To do 

- **A/B/C...N tests**: Handle tests with more than two groups using Hierarchical Models. [4], [10], [11] are good references on the topic. `testr` currently allows for more than two groups in a test, but does not take multiple comparisons into account.
- **Model checking**: Add methods for verifying that the probability model good fit for the data. Gelman describes some methods for doing this in [4].
- **Priors**: Add more ways to specify priors.

## References

[1]: http://engineering.richrelevance.com/bayesian-ab-tests
[2]: http://engineering.richrelevance.com/bayesian-analysis-of-normal-distributions-with-python/
[3]: http://engineering.richrelevance.com/bayesian-ab-testing-with-a-log-normal-model/
[4]: http://www.stat.columbia.edu/~gelman/book/ "Gelman, Andrew, et al. Bayesian data analysis. Vol.[2]: London: Chapman & Hall/CRC, 2014"
[5]: https://web.archive.org/web/20150419163005/http://www.bayesianwitch.com/blog/2014/bayesian_ab_test.html
[6]: https://github.com/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers
[7]: http://developers.lyst.com/data/2014/05/10/bayesian-ab-testing/ 
[8]: http://ewulczyn.github.io/How_Naive_AB_Testing_Goes_Wrong/
[9]: https://www.chrisstucchio.com/blog/2013/metrics_manifesto.html
[10]: http://blog.dominodatalab.com/ab-testing-with-hierarchical-models-in-python/
[11]: http://sl8r000.github.io/ab_testing_statistics/use_a_hierarchical_model/
[12]: http://varianceexplained.org/r/bayesian-ab-testing/
