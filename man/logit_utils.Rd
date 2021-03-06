% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logit_utils.R
\name{logit_utils}
\alias{logit_utils}
\alias{logit}
\alias{invlogit}
\alias{check_logit_inputs}
\title{Logit and inverse logit functions}
\usage{
logit(x, domain_lower = 0, domain_upper = 1)

invlogit(x, domain_lower = 0, domain_upper = 1)

check_logit_inputs(x, domain_lower, domain_upper)
}
\arguments{
\item{x}{[\code{numeric()}]\cr Value to take the logit or inverse logit of.}

\item{domain_lower}{[\code{numeric()}]\cr Lower bound of domain in non-logit
space, inclusive. Default 0. Logit at lower bound is -Inf.}

\item{domain_upper}{[\code{numeric()}]\cr Upper bound of domain in non-logit
space, inclusive. Default 1. Logit at upper bound is Inf.}
}
\value{
[\code{numeric()}]\cr The calculated logit or inverse logit value.
}
\description{
Logit and inverse logit functions, with option to perform a
transformation such that the domain in non-logit space is as specified
(standard is (0, 1)).
}
\details{
Standard:
\itemize{
\item logit (x) = log (x / (1-x))
\item inverse logit (x) = exp (x) / (1 + exp (x))
}

Optional logit with transformed domain:
\itemize{
\item logit (x, l, u) = log (x' / (1 - x')) where x' = (x - l) / (u - l)
\item inverse logit (x, l, u) = ( exp (x) / (1 + exp (x)) ) * (u - l) + l
}

Other notes: Values of x outside of (\code{domain_lower}, \code{domain_upper})
will return NaN and result in a warning from \code{logit} function.
}
\examples{
# Standard
logit(0.1)
invlogit(-2)

# Domain shift
x <- stats::runif(n = 100, min = 10, max = 20)
logit_x <- logit(x, domain_lower = 10, domain_upper = 20)
x_again <- invlogit(logit_x, domain_lower = 10, domain_upper = 20)

}
