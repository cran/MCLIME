cv.m.clime <- function(m.clime.obj, loss=c("likelihood", "tracel2"), fold = 5){
  x <- m.clime.obj$x
  y <- m.clime.obj$y
  n <- nrow(x)
  q <- ncol(x)
  p <- ncol(y)
    
  lambda <- m.clime.obj$lambda
  tau <- m.clime.obj$tau 
  
  part.list <- cv.part(n, fold)
  
  lpfun.Omega <- m.clime.obj$lpfun.Omega
  lpfun.Gamma <- m.clime.obj$lpfun.Gamma
  perturb <- m.clime.obj$perturb
  
  lossname <- match.arg(loss, c("likelihood", "tracel2"))
  #lossfun <- match.fun(lossname )
  
  ntau <- length(tau)
  nlambda <- length(lambda)
  
  loss.re <- matrix(0, nrow=fold, ncol=nlambda*ntau)
  
  for (k in 1:fold){
    x.train <- x[part.list$trainMat[,k],]
    y.train <- y[part.list$trainMat[,k],]
    danclime.obj <- m.clime( x = x.train, y = y.train, lambda = lambda, tau = tau, 
                      linsolver.Gamma = lpfun.Gamma, linsolver.Omega = lpfun.Omega,
                      perturb = perturb)
    Gammalist <- danclime.obj$Gammalist
    Omegalist <- danclime.obj$Omegalist
    x.test <- x[part.list$testMat[,k],]
    y.test <- y[part.list$testMat[,k],]
    ntest <- nrow(x.test)
    for (j in 1:(ntau*nlambda)){
      j.lambda <- ceiling(j/ntau)
      j.tau <- j - ntau*(j.lambda-1) 
      Gamma <- Gammalist[[j.lambda]]
      Omega <- Omegalist[[j]]
      re <- y.test - x.test %*% Gamma
      S <- (1-1/ntest)*var(re)
      if (lossname == "likelihood"){
        loss.re[k,j] <- likelihood.fun(S,Omega)
      }else{
        loss.re[k,j] <- tracel2.fun(S,Omega)
      }
    } 
  }
  
  loss.mean <- apply(loss.re, 2, mean)
  loss.sd <- apply(loss.re, 2, sd)
  
  j.opt <- which.min(loss.mean)
  j.opt.lambda <- ceiling(j.opt/ntau)
  j.opt.tau <- j.opt - ntau*(j.opt.lambda-1)
  
  lambdaopt <- lambda[j.opt.lambda]
  tauopt <- tau[j.opt.tau]
  
  outlist <- list(lambdaopt = lambdaopt, tauopt=tauopt, lambda=lambda, tau=tau, loss = lossname, loss.mean=loss.mean, loss.sd = loss.sd, 
                  lpfun.Gamma=lpfun.Gamma, lpfun.Omega=lpfun.Omega)
  class(outlist) <- c("cv.m.clime")
  return(outlist)
  
}
