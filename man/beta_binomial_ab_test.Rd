\name{beta_binomial_ab_test}
\alias{beta_binomial_ab_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A/B Test using Beta-Binomial Model
}
\description{
Perform bayesian analysis of an experiment with a binary response variable. The experiment is modelled with a binomial likelihood and a beta prior. The beta is parameterized either with two shape parameters {alpha0, beta0} or with one shape parameter alpha0 and the  expected_conversion_rate (mode of the beta prior)
}
\usage{
beta_binomial_ab_test(y, n, alpha0 = 1, beta0 = 1, tolerance = 0.001, nsim = 100000, plot.density = TRUE, conf.level = 0.1, expected_conversion_rate=NULL)
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
first shape parameter for beta prior. Increasing alpha0 reduces uncertainty about expected_conversion_rate.
}
  \item{beta0}{
second shape parameter for beta prior. Ignored if expected_conversion_rate is given.
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
\code{\link{lognormal_ab_test}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
beta_binomial_ab_test(y=c(100,120), n=c(1000, 999), expected_conversion_rate=.1, alpha0=.1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
