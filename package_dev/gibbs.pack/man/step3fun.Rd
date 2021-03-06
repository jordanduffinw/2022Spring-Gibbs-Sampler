% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step3fun.R
\name{step3fun}
\alias{step3fun}
\alias{step3}
\title{Sample \eqn{\theta_i} in the third step of the MCMC sampler.}
\usage{
step3fun(beta_j, f_prior_i, y_tilde_i, sigma2_theta = 1)
}
\arguments{
\item{beta_j}{A Jx1 matrix, or data.frame corresponding to the first column of
\eqn{\tilde{\beta} = [\beta_j, \alpha_j]^T} created in Step 2, where each row indexes a response item j.}

\item{f_prior_i}{A single scalar element of the f_prior vector of length N.}

\item{sigma2_theta}{A scalar, default is set to 1.}

\item{y_tilde}{A single row vector of length J, from the matrix given by
\eqn{[\{\kappa_{ij} / \omega_{ij}^{(t)} + \alpha_{j}^{(t)}\}_{j=1}^{J}]^T}.}
}
\value{
A single scalar, corresponding to \eqn{\theta_i^{(t)}} for a unique demographic profile:
 \item{\code{theta_i}}{The sampled \eqn{\theta_i^{(t)}} from the from the
 univariate normal distribution.}
}
\description{
For internal use only. Runs the third step of the Gibbs sampler, which samples \theta_i ideal point
for a unique demographic profile in 1...N.
}
\note{
Need some clarity on the other values
}
\examples{


}
\seealso{
step1fun, step2fun
}
\author{
Jacob Montgomery, Bryant Moy, Noa Dasanaike, Santiago Olivella
}
\keyword{internal}
