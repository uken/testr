\name{draw_mus_and_sigmas}
\alias{draw_mus_and_sigmas}

\title{
Sample parameters of normal posterior P(mu, sigma^2 | X) assuming an inverse gamma prior on sigma^2 and a normal prior on the mu|sigma^2
}
\description{

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
draw_mus_and_sigmas(data, m0, k0, s_sq0, v0, n_samples = 10000, plot.density = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  vector of observed data, assumed to be generated from a Normal distribution
}
  \item{m0}{
mean parameter for prior on mu | sigma^2
}
  \item{k0}{
scaling parameter for the variance of the normal prior on mu | sigma^2
}
  \item{s_sq0}{
number of degrees of freedom of variance
}
  \item{v0}{
scale of the sigma_squared parameter. Compare with number of data samples.
}
  \item{n_samples}{
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
[1] http://engineering.richrelevance.com/bayesian-analysis-of-normal-distributions-with-python/

[2] Gelman, Andrew, et al. Bayesian data analysis. CRC press, 2013.
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
function (data, m0, k0, s_sq0, v0, n_samples = 10000) 
{
    N <- length(data)
    the_mean <- mean(data)
    SSD <- sum((data - the_mean)^2)
    kN <- k0 + N
    mN <- (k0/kN) * m0 + (N/kN) * the_mean
    vN <- v0 + N
    vN_times_s_sqN <- v0 * s_sq0 + SSD + (N * k0 * (m0 - the_mean)^2)/kN
    alpha <- vN/2
    beta <- vN_times_s_sqN/2
    sig_sq_samples <- beta * 1/rgamma(n = n_samples, shape = alpha, 
        rate = 1)
    mean_norm <- mN
    sd_norm <- sqrt(sig_sq_samples/kN)
    mu_samples <- rnorm(n = n_samples, mean = mean_norm, sd = sd_norm)
    return(list(mu_samples = mu_samples, sig_sq_samples = sig_sq_samples))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
