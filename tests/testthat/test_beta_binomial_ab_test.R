context("Error handling")

test_that("beta_binomial_ab_test throws an error when given bad inputs", {
  expect_error(beta_binomial_ab_test(c(50, 60),
                                     c(100, 100),
                                     expected_conversion_rate = -0.1),
               regexp = "expected_conversion_rate must be positive.")
  expect_error(beta_binomial_ab_test(c(50, 60),
                                     c(100, 100),
                                     expected_conversion_rate = 0),
               regexp = "expected_conversion_rate must be positive.")
  expect_error(beta_binomial_ab_test(c(50, 60),
                                     c(100, 100),
                                     alpha0 = 0),
               regexp = "alpha0 and beta0 must be positive.")
  expect_error(beta_binomial_ab_test(c(50, 60),
                                     c(100, 100),
                                     alpha0 = -1),
               regexp = "alpha0 and beta0 must be positive.")
  expect_error(beta_binomial_ab_test(c(50, 60),
                                     c(100, 100),
                                     beta0 = 0),
               regexp = "alpha0 and beta0 must be positive.")
  expect_error(beta_binomial_ab_test(c(50, 60),
                                     c(100, 100),
                                     beta0 = -1),
               regexp = "alpha0 and beta0 must be positive.")
})
