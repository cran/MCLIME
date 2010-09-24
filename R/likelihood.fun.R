likelihood.fun <- function(Sigma, Omega) {
  tmp <- (sum(diag(Sigma%*%Omega))  - log(det(Omega)) - dim(Omega)[1])
  if(is.finite(tmp)) {
    return(tmp)
  } else {
    return(Inf)
  }
}
