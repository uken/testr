beta_binomial_ab_test <- function(y, n,
                                  alpha0 = 1, beta0 = 1,
                                  tolerance = 0.001,
                                  nsim = 1e5,
                                  plot.density = TRUE,
                                  conf.level = 0.1,
                                  expected_conversion_rate = NULL,
                                  groups = 1:length(y),
) {

  # parameterize either in terms of expected_conversion_rate if it is provided
  if (!is.null(expected_conversion_rate)) {
    beta0 <- 2 - alpha0 + (alpha0 - 1) / expected_conversion_rate
  }

  # Create container
  ngroups <- length(y)
  ci <- matrix(nrow = ngroups,
               ncol = 2,
               dimnames = list(NULL, c('lower', 'upper')))
  samps <- matrix(nrow = ngroups, ncol = nsim)

  # Calculate posterior parameters
  alpha <- alpha0 + y
  beta <- n - y + beta0

  # Sample the posterior to find P(B>A)
  for (g in 1:ngroups) {
    samps[g, ] <- rbeta(nsim, alpha[g], beta[g])
    ci[g, ] <- qbeta(c(conf.level / 2,
                       1 - conf.level / 2),
                     alpha[g],
                     beta[g])
  }

  # Compose output
  posterior.mean <- rowMeans(samps)

  # compute matrix where each entry is 1 if a sample is the best across all groups, 0 otherwise, and take the row means.
  prob.winning <- rowMeans(samps == matrix(rep(apply(samps,
                                                     FUN = max,
                                                     MARGIN = 2),
                                               ngroups),
                                           nrow = ngroups,
                                           byrow=TRUE))

  # plot posterior density
  if(plot.density) {
    d <- expand.grid(group = seq(1, ngroups, 1),
                     input = seq(0, 1, 0.001))
    d$output <- mapply(FUN = dbeta,
                       x = d$input,
                       shape1 = alpha,
                       shape2 = beta)
    print(
      ggplot2::ggplot(d,
                      ggplot2::aes(x = input,
                                   y = output,
                                   colour = factor(group))) +
        ggplot2::geom_line() +
        ggplot2::xlab("Conversion Rate") +
        ggplot2::ylab("Density") +
        ggplot2::ggtitle("Posterior Distribution(s)") +
        ggplot2::scale_colour_discrete(name = "Variant(s)")
    )
  }

  # compute risk associated with each possible decision
  risk <- rep(NA_real_, ngroups)
  for(g in 1:ngroups) {
    loss <- apply(samps,
                  FUN = max,
                  MARGIN = 2) - samps[g, ] #difference between the MAP mean and the group that is actually the best
    risk[g]  <- mean(loss)
  }

  return(list(y = y,
              n = n,
              risk = risk,
              winner = groups[risk < tolerance],
              stop.test = min(risk) < tolerance,
              tolerance = tolerance,
              prob.winning = prob.winning,
              posterior.mean = posterior.mean,
              ci = ci,
              conf.level = conf.level,
              posterior_parameters = data.frame(alpha = alpha,
                                                beta = beta),
              groups = groups)
  )
}
