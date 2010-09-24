\name{print.m.clime} \alias{print.m.clime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{print a m.clime object}
\description{
  Print a summary of the m.clime object.
} \usage{ \method{print}{m.clime}(x,digits = max(3, getOption("digits")
- 3), ... ) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{m.clime object.}
  \item{digits}{significant digits in printout.}
  \item{...}{additional print options.}
}


\details{
  This call simply outlines the options used for computing a m.clime object.
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


%\examples{
%}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{multivariate}% __ONLY ONE__ keyword per line
