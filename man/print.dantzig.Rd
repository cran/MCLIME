\name{print.dantzig} \alias{print.dantzig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{print a dantzig object}
\description{
  Print a summary of the dantzig object.
} \usage{ \method{print}{dantzig}(x,digits = max(3, getOption("digits")
- 3), ... ) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{dantzig object.}
  \item{digits}{significant digits in printout.}
  \item{...}{additional print options.}
}


\details{
  This call simply outlines the options used for computing a dantzig object.
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


\examples{
x <- matrix(rnorm(100,sd=0.5),10,10)
Gamma0 <- rbind(toeplitz(c(5,0,0,0,0)),toeplitz(c(4,0,0,3,0)))
y <- x%*%Gamma0 + matrix(rnorm(50),10,5)
dantzig.obj <- dantzig(x,y,lambda=0.1)
print(dantzig.obj)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{multivariate}% __ONLY ONE__ keyword per line
