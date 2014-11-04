\name{plot_prior_lognormal_model}
\alias{plot_prior_lognormal_model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the priors for each component of the lognormal model.
}
\description{
}
\usage{
plot_prior_lognormal_model(alpha0 = 1, beta0 = 1, s_sq0 = 1, v0 = 1, m0 = 1, k0 = 1, n = 10000, expected_revenue_converted_users = NULL, expected_conversion_rate = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alpha0}{
first shape parameter for beta prior. Increasing alpha0 reduces uncertainty about expected_conversion_rate.
}
  \item{beta0}{
second shape parameter for beta prior. Ignored if expected_conversion_rate is given. 
}
  \item{s_sq0}{
number of degrees of freedom of variance.
}
  \item{v0}{
scale of the sigma_squared parameter. Compare with number of data samples.
}
  \item{m0}{
mean parameter for the normal prior on mu | sigma^2.  Ignored if expected_revenue_converted_users is specified.
}
  \item{k0}{
scaling parameter for the variance of the normal prior on mu | sigma^2
}
  \item{n}{
number of monte carlo samples
}
  \item{expected_revenue_converted_users}{
expected revenue for converted (spending) users
}
  \item{expected_conversion_rate}{
before seeing the data, what is is the most likely conversion rate (i.e. mode of the beta prior)? From 0 to 1.
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
plot_prior_lognormal_model(s_sq0=1.2, v0=73, k0=73, alpha0=10, expected_conversion=.05, expected_revenue_converted_users=5)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
