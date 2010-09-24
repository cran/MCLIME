\name{m.clime}
\alias{m.clime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{simultaneously solve for the regression coefficients estimates and the precision matrix estimates for multivariate regression models.}
\description{
  Solve for a series of the regression coefficients matrix estimates and inverse covariance matrix estimates at a grid of
  values of the constraint lambda and tau.
}
\usage{ m.clime( x, y, lambda = NULL, nlambda = ifelse(is.null(lambda),10,length(lambda)), lambda.max = max(cov(x)*(1-1/nrow(x)))*max(cov(y)*(1-1/nrow(y)))*sqrt(4*log(ncol(x))*log(ncol(y))/n),
lambda.min=ifelse(nrow(x)>ncol(x), 1e-3, 1e-2), logspaced.lambda = TRUE, linsolver.Gamma = c("primaldual", "simplex"),
ntau = ifelse(is.null(tau),50,length(tau)), tau = NULL, tau.max=0.8, tau.min=ifelse(nrow(x)>ncol(x), 1e-3, 1e-2),
perturb=FALSE, logspaced.tau=TRUE, linsolver.Omega=c("primaldual", "simplex"), pdtol = 1e-3, pdmaxiter = 50)
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
    The default value is \eqn{\max(\sigma_X)\max(\sigma_Y)\sqrt(4(\log p+\log q)/n)}.}
  \item{lambda.min}{Minimum value of program generated lambda.
    Default 1e-5(\eqn{n > p}) or 1e-3(\eqn{n < p}).
  }
  \item{logspaced.lambda}{Whether program generated lambda should be
    log-spaced or linear spaced.  Default TRUE.
  }
  \item{linsolver.Gamma}{Whether primaldual (default) or simplex method should be employed.
    Rule of thumb: primaldual for large q, simplex for small q.}
  \item{tau}{Grid of non-negative values for the constraint
                 parameter tau. If missing, ntau  values from tau.min to
                 tau.max will be generated.}
  \item{ntau}{Number of values for program generated tau.  Default 100.}
  \item{tau.max}{Maximum value of program generated tau.
    Default 0.8.}
  \item{tau.min}{Minimum value of program generated tau.
    Default 1e-4(\eqn{n > p}) or 1e-2(\eqn{n < p}).
  }
  \item{perturb}{Whether a perturbed Sigma should be used or
    the positive perturbation added if it is numerical.  Default FALSE.}
  \item{logspaced.tau}{Whether program generated lambda should be
    log-spaced or linear spaced.  Default TRUE.
  }
  \item{linsolver.Omega}{Whether primaldual (default) or simplex method
    should be employed. Rule of thumb: primaldual for large p,
    simplex for small p.}
  \item{pdtol}{Tolerance for the duality gap, ignored if simplex
    is employed.}
  \item{pdmaxiter}{Maximum number of iterations for primaldual,
    ignored if simplex is employed.}
}
\details{
  A constrained \eqn{L_1}
  minimization approach for sparse regression coefficients matrix and precision matrix estimation for multivariate regrssion model
  is implemented here using linear programming (revised
  simplex or primal-dual interior point method).

  First, it solves for \eqn{\Gamma_0} using the following objective function using a sequence of lambda:
  \deqn{
    \min |\Gamma|_1 \quad \textrm{subject to: } || \hat{cov(X,Y) - \hat{\var{X}}\Gamma} ||_\infty \leq \lambda.
  }

  Then, for a given \eqn{\Omega_0}, it solves for \eqn{\Omega_0} using
  a sequence of tau values on the following objective function\cr
  \deqn{
    \min |\Omega|_1 \quad \textrm{subject to: } ||cov(Y-X\Gamma_0)
    \Omega - I||_\infty \leq \tau,
  }
  where \eqn{S_n} is the sample covariance matrix of \eqn{Z=Y-X\hat{\Gamma}} and \eqn{\Omega}
  is the inverse we want to estimate.
}
\value{
  An object with S3 class "m.clime". You can also use it as a
  regular R list with the following fields:
  \item{Gammalist}{List of estimated regression coefficient matrices for a grid of
    values for lambda.}
  \item{Omegalist}{List of estimated inverse covariance matrix for a grid of
    values for tau and lambda.}
  \item{x}{Actual covariate matrix x used in the program.}
  \item{y}{Actual outcome matrix y used in the program.}
  \item{lambda}{Actual sequance of the regression coefficient constraints lambda used in the program.}
  \item{tau}{Actual sequence of the precision matrix constraints tau used in the program.}
  \item{perturb}{Actual perturbation used in the program.}
  \item{lpfun.Gamma}{Linear programming solver used for estimating the regression coefficient.}
  \item{lpfun.Omega}{Linear programming solver used for estimating the precision matrix.}
}

\references{Cai, T.T., Li, H., Liu, W., and Xie, J. (2010).   \emph{
     Simultaneous estimation of the regression coefficients and precision matrix.
  } Technical Report, University of Pennsylvania.
}

\author{T. Tony Cai,  Hongzhe Li, Weidong Liu and Jichun Xie.\cr
  Maintainer: Jichun Xie \email{jichun@mail.med.upenn.edu} }


\examples{
set.seed(922)
x <- matrix(rnorm(1500,sd=0.5),50,30)
Gamma0 <- matrix(rnorm(600),30,20)*matrix(rbinom(600,1,0.2),30,20)

Omega0.tmp <- matrix(rnorm(400),20,20)*matrix(rbinom(400,1,0.1),20,20)
Omega0.tmp <- (Omega0.tmp+t(Omega0.tmp))
diag(Omega0.tmp) <- 1
Omega0 <- make.pd(Omega0.tmp,cond.num=20)

v <- matrix(rnorm(1000),50,20)
y <- x%*%Gamma0+v%*%Mpower(solve(Omega0),1/2)

mc.obj <- m.clime(x,y,lambda=0.1, tau=0.1)
mc.obj
}

\keyword{models}
\keyword{multivariate}
