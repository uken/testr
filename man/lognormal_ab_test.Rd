\name{lognormal_ab_test}
\alias{lognormal_ab_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
hybrid of beta-binomial model to classify spenders, lognormal for revenue amongst spenders
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lognormal_ab_test(data, nsim = 100000, alpha0 = 1, beta0 = 25, m0 = 4, k0 = 1, s_sq0 = 1, v0 = 5, plot.density = FALSE, conf.level = 0.1, tolerance = 0.01, save.hist = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data.frame or matrix of observed data. 1 row per group (pad with NAs for unequal sample sizes)
}
  \item{nsim}{
number of monte carlo samples
}
  \item{alpha0}{
first shape parameter for beta prior
}
  \item{beta0}{
second shape parameter for beta prior
}
  \item{m0}{
mean parameter for the normal prior on mu|sigma^2
}
  \item{k0}{
scaling parameter for the variance of the normal prior on mu|sigma^2
}
  \item{s_sq0}{
number of degrees of freedom of variance
}
  \item{v0}{
scale of the sigma_squared parameter. Compare with number of data samples.
}
  \item{plot.density}{
plot posterior densities? (T/F)
}
  \item{conf.level}{
specifies alpha for (1-alpha)*100\% credible intervals
}
  \item{tolerance}{
smallest difference we care about 
}
  \item{save.hist}{
return data for posterior histograms? (T/F)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, nsim = 100000, alpha0 = 1, beta0 = 25, m0 = 4, 
    k0 = 1, s_sq0 = 1, v0 = 5, plot.density = FALSE, conf.level = 0.1, 
    tolerance = 0.01, save.hist = TRUE) 
{
    nonzero.count <- rowSums(data > 0, na.rm = TRUE)
    sample.sizes <- rowSums(!is.na(data))
    ngroups <- nrow(data)
    conv.samps <- spenders.rev.samps <- rev.samps <- matrix(nrow = ngroups, 
        ncol = nsim)
    ci <- matrix(nrow = ngroups, ncol = 2, dimnames = list(NULL, 
        c("lower", "upper")))
    for (g in 1:ngroups) {
        nonzeros <- data[g, ]
        nonzeros <- nonzeros[nonzeros > 0 & !is.na(nonzeros)]
        conv.samps[g, ] <- rbeta(nsim, nonzero.count[g] + alpha0, 
            sample.sizes[g] - nonzero.count[g] + beta0)
        spenders.rev.samps[g, ] <- draw_log_normal_means(nonzeros, 
            m0, k0, s_sq0, v0, nsim)
        rev.samps[g, ] <- conv.samps[g, ] * spenders.rev.samps[g, 
            ]
        ci[g, ] <- quantile(rev.samps[g, ], probs = c(conf.level/2, 
            1 - conf.level/2))
    }
    posterior.mean <- rowMeans(rev.samps)
    prob.winning <- rowMeans(rev.samps == matrix(rep(apply(rev.samps, 
        FUN = max, MARGIN = 2), ngroups), nrow = ngroups, byrow = TRUE))
    posterior.samples <- data.frame(mean.revenue = as.numeric(t(rev.samps)), 
        group = as.factor(rep(1:ngroups, each = nsim)))
    if (plot.density) {
        x <- ggplot(posterior.samples, aes(x = mean.revenue, 
            y = ..ncount.., fill = group)) + geom_histogram(alpha = 0.2, 
            position = "identity", binwidth = 0.005)
        print(x)
    }
    if (save.hist) {
        hist.data <- data.frame(group = NA, bin = NA, density = NA)
        for (g in 1:ngroups) {
            h <- hist(posterior.samples[posterior.samples[, "group"] == 
                g, "mean.revenue"], plot = FALSE)
            hist.data <- rbind(hist.data, data.frame(group = g, 
                bin = h$mids, density = h$density))
        }
    }
    hist.data <- hist.data[-1, ]
    risk <- rep(NA_real_, ngroups)
    for (g in 1:ngroups) {
        loss <- apply(rev.samps, FUN = max, MARGIN = 2) - rev.samps[g, 
            ]
        risk[g] <- mean(loss)
    }
    return(list(risk = risk, winner = (1:ngroups)[risk < tolerance], 
        stop.test = min(risk) < tolerance, tolerance = tolerance, 
        prob.winning = prob.winning, posterior.mean = posterior.mean, 
        ci = ci, conf.level = conf.level, hist.data = hist.data))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
