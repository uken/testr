#' Plot results from a lognormal_ab_test object
#'
#' Plot posterior distributions for a Zero-Inflated Lognormal A/B test using
#'   ggplot2.
#'   
#' @param x object of class lognormal_ab_test
#' @param limits vector with (min,max) x-axis values
#' @param labels vector with group names
#' @param title graph title
#' @param ... Additional arguments to pass on to \code{geom_density()}
#'
#' @return object of class ggplot2
#' @examples
#'  n = 1000 # users in each group
#'  conversion = 0.1 # conversion rate. In this example, it is common to all groups
#'  A_data <- rbinom(n,1,conversion) * rlnorm(n, meanlog=0)
#'  B_data <- rbinom(n,1,conversion) * rlnorm(n, meanlog=0.005)
#'  C_data <- rbinom(n,1,conversion) * rlnorm(n, meanlog=0.08)
#'  data <- data.frame(ab_group = rep(c('A','B','C'), each = n),
#'                     ltv = c(A_data, B_data, C_data))
#'  l = lognormal_ab_test(data)
#'  plot(l)
#'  plot(l, limits = c(0, 0.5), labels = c('A', 'B', 'C'), title = 'AB Test: Lifetime Value')
#'  g <- plot(l)
#'  g + ggplot2::theme_bw()
#' @seealso \code{\link{plot_revenue_prior}} to plot all components of the
#'   prior distribution
#' @seealso \code{\link{lognormal_ab_test}} to run an A/B test with a Lognormal
#'   model (i.e. for testing revenue)
#' @export

plot.lognormal_ab_test <- function(x, limits = c(0, 1), labels = NULL, title = "", ...) {
    posterior_samples <- data.frame(mean.revenue = as.numeric(t(x$rev.samps)),
                                    group = as.factor(rep(x$groups, each = x$nsim)))
    if (is.null(labels)) {
      labels <- x$groups
    }
    gg <- ggplot2::ggplot(posterior_samples, ggplot2::aes(x = mean.revenue, fill = factor(group))) +
      ggplot2::geom_density(...) +
      ggplot2::scale_x_continuous(limits = limits) +
      ggplot2::scale_fill_discrete(name = ifelse(length(x$groups) > 1, "Variants", "Variant"), labels = labels) +
      ggplot2::labs(title = title, y = "Density", x = "Mean Revenue")
    return(gg)
}
