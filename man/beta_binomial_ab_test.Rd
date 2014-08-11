\name{beta_binomial_ab_test}
\alias{beta_binomial_ab_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A/B Test using Beta-Binomial Model
}
\description{
Perform bayesian analysis of an experiment with a binary response variable. The experiment is modelled with a binomial likelihood and a beta prior.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
beta_binomial_ab_test(y, n, alpha0 = 1, beta0 = 1, tolerance = 0.001, nsim = 100000, plot.density = TRUE, conf.level = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
vector of counts of successes (i.e. number of converted users across the groups)
}
  \item{n}{
vector of counts of trials (i.e number of total users)
}
  \item{alpha0}{
first shape parameter for beta prior
}
  \item{beta0}{
second shape parameter for beta prior
}
  \item{tolerance}{
smallest difference we care about 
}
  \item{nsim}{
number of monte carlo samples
}
  \item{plot.density}{
plot posterior densities? (T/F)
}
  \item{conf.level}{
specifies alpha for (1-alpha)*100\% credible intervals
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
http://engineering.richrelevance.com/bayesian-ab-tests/
}
\author{
Alex Yakubovich, Uken Games Inc.
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
function (y, n, alpha0 = 1, beta0 = 1, tolerance = 0.001, nsim = 100000, 
    plot.density = TRUE, conf.level = 0.1) 
{
    ngroups <- length(y)
    ci <- matrix(nrow = ngroups, ncol = 2, dimnames = list(NULL, 
        c("lower", "upper")))
    samps <- matrix(nrow = ngroups, ncol = nsim)
    alpha <- alpha0 + y
    beta <- n - y + beta0
    for (g in 1:ngroups) {
        samps[g, ] <- rbeta(nsim, alpha[g], beta[g])
        ci[g, ] <- qbeta(c(conf.level/2, 1 - conf.level/2), alpha[g], 
            beta[g])
    }
    posterior.mean <- rowMeans(samps)
    prob.winning <- rowMeans(samps == matrix(rep(apply(samps, 
        FUN = max, MARGIN = 2), ngroups), nrow = ngroups, byrow = TRUE))
    if (plot.density) {
        for (g in 1:ngroups) curve(dbeta(x, alpha[g], beta[g]), 
            from = 0, to = 0.15, n = 100000, col = "darkblue", 
            lwd = 2, xlab = "retention", ylab = "density", add = g > 
                1)
    }
    risk <- rep(NA_real_, ngroups)
    for (g in 1:ngroups) {
        loss <- apply(samps, FUN = max, MARGIN = 2) - samps[g, 
            ]
        risk[g] <- mean(loss)
    }
    return(list(y = y, n = n, risk = risk, winner = (1:ngroups)[risk < 
        tolerance], stop.test = min(risk) < tolerance, tolerance = tolerance, 
        prob.winning = prob.winning, posterior.mean = posterior.mean, 
        ci = ci, conf.level = conf.level, posterior_parameters = data.frame(alpha = alpha, 
            beta = beta)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
