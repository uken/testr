\name{lognormal_ab_test}
\alias{lognormal_ab_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
hybrid of beta-binomial model to classify spenders, lognormal model for spenders' revenue
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
data frame where first column = group, second column = observations
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
 \item{expected_revenue_converted_users}{
expected revenue for converted (spending) users
}
 \item{expected_conversion_rate}{
before seeing the data, what is is the most likely conversion rate (i.e. mode of the beta prior)? From 0 to 1.
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
\item{n}{
vector of sample sizes for all groups
}
\item{sample.mean}{
vector of sample mean revenue across groups 
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
\code{\link{plot_prior_lognormal_model}}
}
\examples{
## simulate A/B/C test for revenue 
A_data <- c(rep(0,1000), rlnorm(50, meanlog=0))
B_data <- c(rep(0,1000), rlnorm(50, meanlog=.005))
C_data <- c(rep(0,1000), rlnorm(50, meanlog=.08))

data <- data.frame(ab_group=rep(c('A','B','C'), each=1050*3), ltv=c(A_data, B_data, C_data))

lognormal_ab_test(data)

plot_prior_lognormal_model(expected_conversion_rate=0.65, alpha0=15, expected_revenue_converted_users=1.5, v0=73, k0=100, s_sq0=1.2) #specify prior
lognormal_ab_test(data, plot.density=TRUE, expected_conversion_rate=0.65, expected_revenue_converted_users=1.5, v0=73, k0=100, s_sq0=1.2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
