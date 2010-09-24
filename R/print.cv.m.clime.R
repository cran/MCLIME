print.cv.m.clime <- function(x,digits = max(3, getOption("digits") - 3), ... ) {
  cat("\n cv.m.clime options summary: \n")
  print(cbind(lambda=signif(x$lambda,digits), mean=signif(x$loss.mean, digits), sd=signif(x$loss.sd, digits) ))
  cat("\n CV loss used:",x$loss,"\n ")
  cat("CV optimal lambda=", signif(x$lambdaopt, digits), "\n")
  cat("CV optimal tau=", signif(x$tauopt, digits), "\n")
}
