\name{print.cv.m.clime}
\alias{print.cv.m.clime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{print a cv.m.clime object}
\description{
  Print a summary of the m.clime object.
} \usage{ \method{print}{cv.m.clime}(x,digits = max(3, getOption("digits")
- 3), ... ) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{cv.m.clime object.}
  \item{digits}{significant digits in printout.}
  \item{...}{additional print options.}
}


\details{
  This call simply outlines the options used for computing a cv.m.clime object.
}

\value{
  The output above is invisiblly returned.
}

\references{Cai, T.T., Li, H., Liu, W., and Xie, J. (2010).   \emph{
     Simultaneous estimation of the regression coefficients and precision matrix.
  } Technical Report, University of Pennsylvania.
}

\author{T. Tony Cai,  Hongzhe Li, Weidong Liu and Jichun Xie.\cr
  Maintainer: Jichun Xie \email{jichun@mail.med.upenn.edu} }

% \note{
% \section{TODO}{More features will be added very soon.}
% }

\examples{
x <- matrix(rnorm(100),10,10)
Gamma0 <- rbind(toeplitz(c(5,0,0,0,0)),toeplitz(c(4,0,0,3,0)))
y <- x%*%Gamma0 + matrix(rnorm(50),10,5)

## Solve for the m.clime object.
mc.obj <- m.clime(x,y,lambda.max=0.3,nlambda=5,tau.max=0.3,ntau=10,linsolver.Gamma="simplex",
    linsolver.Omega="simplex")

## Choose the best fist stage estimator of the regression coefficient using cross-validation.
cv.mc.obj <- cv.m.clime(mc.obj)

## Print the object.
print(cv.mc.obj)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{multivariate}% __ONLY ONE__ keyword per line
