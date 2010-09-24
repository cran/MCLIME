\name{make.pd} \alias{make.pd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{solve for the regression coefficients}
\description{
  Increase the diagonal value of a symmetric matrix with positive largest eigenvalue to make
  it positive definite and with desired condition number.
} \usage{ make.pd(m, cond.num) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{Input a symmetric matrix with positive largest eigenvalue.}
  \item{cond.num}{The desired condition number.}
} \details{
  The perturbation \eqn{\delta} is imposed on the diagonal of the
  original matrix \eqn{M} in order to make it positive definite and
  has the desired condition number.
  \deqn{
    M_2 = M + \delta I.
  }
} \value{
  A positive definite matrix with the desired condition number.
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
m <- matrix(1:100,10,10)
m <- m+t(m)
make.pd(m,cond.num=10)
}
