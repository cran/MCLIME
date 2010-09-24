\name{dantzig}
\alias{dantzig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{solve for the regression coefficients}
\description{
  Solve for the regression coefficients using the Danziger selector
  constrained by  a prespecified or default matrix lambda.
}
\usage{
dantzig(x, y, lambda = NULL,
                       nlambda=ifelse(is.null(lambda),100,length(lambda)),
                       lambda.max= max(cov(x)*(1-1/nrow(x)))*
                                   max(cov(y)*(1-1/nrow(y)))*
                                   sqrt(4*log(ncol(x))*log(ncol(y))/n),
                       lambda.min=ifelse(nrow(x)>ncol(x), 1e-3, 1e-2),
                       logspaced=TRUE,
                       linsolver = c("primaldual", "simplex"),
                       pdtol = 1e-3, pdmaxiter = 50 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Input the design matrix of size n (observations) times q (variables).
    Each column is a covariate of length n.}
  \item{y}{Input the outcome matrix of size n (observations) times p (variables).
    Each column is an outcome variable of length n.}
  \item{lambda}{Grid of non-negative values for the constraint
    parameter lambda. If missing, nlambda  values from lambda.min to
    lambda.max will be generated.}
  \item{nlambda}{Number of values for program generated lambda.  Default 100.}
  \item{lambda.max}{Maximum value of program generated lambda.
    The default value is \eqn{\max(\sigma_X)\max(\sigma_Y)\sqrt{4(\log p+\log q)/n}}.}
  \item{lambda.min}{Minimum value of program generated lambda.
    Default 1e-5(\eqn{n > p}) or 1e-3(\eqn{n < p}).
  }
  \item{logspaced}{Whether program generated lambda should be
    log-spaced or linear spaced.  Default TRUE.
  }
  \item{linsolver}{Whether primaldual (default) or simplex method
    should be employed. Rule of thumb: primaldual for large p,
    simplex for small p.}
  \item{pdtol}{Tolerance for the duality gap, ignored if simplex
    is employed.}
  \item{pdmaxiter}{Maximum number of iterations for primaldual,
    ignored if simplex is employed.}
}
\details{
  A constrained \eqn{L_1}
  minimization approach for multi-chancel normal mean regression problem (details
  in references) is implemented here using linear programming (revised
  simplex or primal-dual interior point method). It solves for \eqn{\Gamma_0} using the following objective function:
  \deqn{
    \min |\Gamma |_1 \quad \textrm{subject to: } ||\hat{cov(X,Y) - \hat{\var{X}}\Gamma}||_\infty \leq \lambda.
  }
}

\value{
  An object with S3 class "dantzig". You can also use it as a
  regular R list with the following fields:
  \item{Gammalist}{List of estimated regression coefficient matrices for a grid of
    values for lambda.}
  \item{x}{Actual covariate matrix x used in the program.}
  \item{y}{Actual outcome matrix y used in the program.}
  \item{lambda}{Actual constraint matrix lambda used in the program.}
  \item{lpfun}{Linear programming solver used.}
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
set.seed(922)
x <- matrix(rnorm(1500,sd=0.5),50,30)
Gamma0 <- matrix(rnorm(600),30,20)*matrix(rbinom(600,1,0.2),30,20)
y <- x%*%Gamma0 + matrix(rnorm(1000),50,20)
dantzig.obj <- dantzig(x,y,lambda=0.1,linsolver="simplex")
dantzig.obj
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{multivariate}% __ONLY ONE__ keyword per line
