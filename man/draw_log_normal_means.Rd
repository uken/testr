\name{draw_log_normal_means}
\alias{draw_log_normal_means}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample mean parameter from Log-normal posterior 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
draw_log_normal_means(data, m0, k0, s_sq0, v0, nsim = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  vector of observed data, assumed to be generated from a Log-Normal distribution
}
  \item{m0}{
mean parameter for prior on mu | sigma^2
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
  \item{nsim}{
number of monte carlo samples
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
http://engineering.richrelevance.com/bayesian-ab-testing-with-a-log-normal-model/
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
function (data, m0, k0, s_sq0, v0, nsim = 10000) 
{
    log_data <- log(data)
    mu_sig_sq_samples <- draw_mus_and_sigmas(log_data, m0, k0, 
        s_sq0, v0, nsim)
    log_normal_mean_samples <- exp(mu_sig_sq_samples[[1]] + mu_sig_sq_samples[[2]]/2)
    return(log_normal_mean_samples)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
