#' Visualize all components of the prior distribution for a Lognormal A/B test
#'   for revenue.
#'
#' @param alpha0 first shape parameter for beta prior. Increasing alpha0
#'   #e41a1cuces uncertainty about expected_conversion_rate.
#' @param beta0 second shape parameter for beta prior. Igno#e41a1c if
#'   expected_conversion_rate is given.
#' @param s_sq0 number of degrees of freedom of variance.
#' @param v0 scale of the sigma_squa#e41a1c parameter. Compare with number of
#'   data samples.
#' @param m0 mean parameter for the normal prior on mu | sigma^2. Igno#e41a1c if
#'   expected_revenue_converted_users is specified.
#' @param k0 scaling parameter for the variance of the normal prior on mu | sigma^2
#' @param n number of monte carlo samples
#' @param expected_revenue_converted_users expected revenue for converted
#'   (spending) users
#' @param expected_conversion_rate before seeing the data, what is is the most
#'   likely conversion rate (i.e. mode of the beta prior)? From 0 #' to 1.
#' @param plot A logical flag indicating whether to plot the prior or return a
#'   data frame containing the values that would be plotted.
#' @param resolution Controls the smoothness of the curves generated. Lower
#'   values lead to more jagged curves.
#'
#' @examples
#'   plot_revenue_prior(expected_conversion_rate = 0.65, alpha0 = 15,
#'                      expected_revenue_converted_users = 1.5,
#'                      v0 = 73, k0 = 100, s_sq0 = 1.2)
#'
#' @seealso \code{\link{lognormal_ab_test}} to run an A/B test with a Lognormal
#'   model (i.e. for testing revenue)
#' @seealso \code{\link{plot_conversion_prior}} to plot the beta prior given
#'   its parameters
#' @export

plot_revenue_prior <- function(alpha0 = 1, beta0 = 1, s_sq0 = 1, v0 = 1, m0 = 1, k0 = 1, n = 1e4,
           expected_revenue_converted_users = NULL, expected_conversion_rate = NULL,
           plot = TRUE, resolution = 100) {
    # density and random generation for the inverse gamma distribution (not part of base R)
    dinvgamma2 <- function(x, shape, scale = 1) {
      a <- shape
      b <- scale
      return((b ^ a / gamma(a)) * x ^ (-a - 1) * exp(-b / x))
    }
    rinvgamma2 <- function (n, shape, scale = 1) {
      return(1/rgamma(n = n, shape = shape, rate = scale))
    }
    # parameterize in terms of expected_revenue_converted_users and expected_conversion_rate if they"re provided
    if (!is.null(expected_conversion_rate)) {
      beta0 <- 2 - alpha0 + (alpha0 - 1) / expected_conversion_rate
    }
    if (!is.null(expected_revenue_converted_users)) {
      m0 <- log(expected_revenue_converted_users) - 0.5 * v0 * s_sq0 / (v0 + 2)
    }
    # log revenue - inv gamma prior over variance
    s2 <- quantile(rinvgamma2(n, shape = v0 / 2, scale = v0 * s_sq0 / 2), c(0.1, 0.5, 0.9, 0.95))
    conversion_rate <- rbeta(n, shape1 = alpha0, shape2 = beta0)
    s_sq <- rinvgamma2(n, shape = v0 / 2, scale = v0 * s_sq0 / 2)
    mu <- rnorm(n, mean = m0, sd = sqrt(s_sq / k0))
    revenue_converted_users <- exp(mu + s_sq / 2)
    ltv <- conversion_rate * revenue_converted_users
    
    if (plot) {
      
      color_brewer <- c("red" = "#E41A1C", "blue" = "#377EB8", "green" = "#4DAF4A")
      # ^ from RColorBrewer::brewer.pal(3, "Set1")
      
      old_mfrow <- par(mfrow = c(3, 2))
      
      curve(dinvgamma2(x, shape = v0 / 2, scale = v0 * s_sq0 / 2),
            from = 0, to = s2[4], n = resolution, lwd = 2,
            xlab = "variance", ylab = "Density", main = "Inverse gamma prior over variance"
      )
      
      abline(v = s2[1], col = color_brewer['blue'], lwd = 2, lty = 2)
      abline(v = s2[2], col = color_brewer['red'], lwd = 2, lty = 2)
      abline(v = s2[3], col = color_brewer['green'], lwd = 2, lty = 2)
      
      # log revenue - normal prior over mu conditioned on sigma^2
      lim <- m0 + 3 * c(-1, 1) * sqrt(s2[3] / k0)
      
      curve(dnorm(x, mean = m0, sd = sqrt(s2[1] / k0)),
            xlim = lim, n = resolution, lwd = 2, col = color_brewer['blue'],
            xlab = "log revenue", ylab = "Density", main = "Normal prior over mean | variance")
      
      curve(dnorm(x, mean = m0, sd = sqrt(s2[2] / k0)),
            xlim = lim, add = TRUE, n = resolution, lwd = 2, col = color_brewer['red'])
      
      curve(dnorm(x, mean = m0, sd = sqrt(s2[3] / k0)),
            xlim = lim, add = TRUE, n = resolution, lwd = 2, col = color_brewer['green'])
      
      legend("topright", legend = paste("variance = ", round(s2, 1)),
             col = c(color_brewer['blue'], color_brewer['red'], color_brewer['green']), lwd = rep(2, 3))
      
      hist(mu,
           xlab = "Log (mean revenue) amongst converted users",
           main = "Log (Mean LTV) amongst converted users")
      
      hist(revenue_converted_users,
           xlab = "Mean revenue amongst converted users",
           main = "Mean revenue amongst converted users")
      
      #conversion_rate - beta prior over conversion_rate rate
      curve(dbeta(x, shape1 = alpha0, shape2 = beta0),
            from = 0, to = 1, n = resolution, lwd = 2,
            xlab = "conversion_rate rate", ylab = "Density", main = "Beta prior over the conversion_rate rate"
      )
      
      hist(ltv, xlab = "Mean revenue", main = "Mean revenue")
      
      par(old_mfrow) # Reset the par(mfrow) to what the user had it at before.
    } else {
      graph <- hist(ltv, breaks = resolution, plot = FALSE)
      data.frame(graph$mids, graph$density)
    }
}
