\name{function-internal}
\alias{meanclime}
\alias{linprogPD2}
\alias{linprogS2}
\alias{likelihood.fun}
\alias{tracel2.fun}
\alias{Mpower}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{internal MCLIME functions}
\description{
  Internal MCLIME functions
}
\usage{meanclime(dantzig.obj,tau=NULL,ntau=ifelse(is.null(tau),100,length(tau)),
              tau.max=0.8,tau.min=ifelse(nrow(x)>ncol(x), 1e-3, 1e-2),
              perturb=FALSE,logspaced=TRUE,linsolver.Omega=c("primaldual", "simplex"),
              pdtol=1e-3, pdmaxiter=50)
       likelihood.fun(Sigma, Omega)
       tracel2.fun(Sigma, Omega)
       linprogPD2(x0,A,b,epsilon,pdtol=1e-3,pdmaxiter=50)
       linprogS2(Sigma, e, lambda)
       Mpower(Sig,q)
}

%- maybe also 'usage' for other objects documented here.

\details{
  They are not intended for use by users.
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{internal}
