beta_binomial_ab_test <- function(y, n,
                                  alpha0 = 1, beta0 = 1,
                                  tolerance = 0.001,
                                  nsim = 1e5,
                                  conf.level = 0.1,
                                  expected_conversion_rate = NULL,
                                  groups = 1:length(y)) {
  if(any(alpha0 <= 0, beta0 <= 0))
    stop("alpha0 and beta0 must be positive.")

  # parameterize either in terms of expected_conversion_rate if it is provided
  if (!is.null(expected_conversion_rate)) {
    if(expected_conversion_rate <= 0)
      stop("expected_conversion_rate must be positive.")
    beta0 <- 2 - alpha0 + (alpha0 - 1) / expected_conversion_rate
  }

  ngroups <- length(y)

  # Create container
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

  # compute risk associated with each possible decision
  risk <- rep(NA_real_, ngroups)
  for(g in 1:ngroups) {
    loss <- apply(samps,
                  FUN = max,
                  MARGIN = 2) - samps[g, ] #difference between the MAP mean and the group that is actually the best
    risk[g]  <- mean(loss)
  }

  return(
    structure(
      list(y = y,
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
           groups = groups),
      class = "beta_binomial_ab_test")
  )
}

plot.beta_binomial_ab_test <- function(x,
                                       limits = c(0, 1),
                                       labels = NULL,
                                       title = "") {

  d <- expand.grid(groups = x$groups,
                   input = seq(0, 1, 0.001))
  d$output <- mapply(FUN = dbeta,
                     x = d$input,
                     shape1 = x$posterior_parameters$alpha,
                     shape2 = x$posterior_parameters$beta)


  print(
    ggplot2::ggplot(d,
                    ggplot2::aes(x = input,
                                 y = output,
                                 colour = factor(groups))) +
      ggplot2::geom_line() +
      ggplot2::xlab("Conversion Rate") +
      ggplot2::ylab("Density") +
      ggplot2::ggtitle(title) +
      ggplot2::scale_colour_discrete(name = "Variant(s)") +
      ggplot2::scale_x_continuous(labels = scales::percent_format()) +
      ggplot2::coord_cartesian(xlim = limits) +
      ggplot2::theme_bw(base_size = 15) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 15))
  )
}
