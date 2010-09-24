print.m.clime <- function(x,digits = max(3, getOption("digits") - 3), ... ) {
  cat("\n m.clime options summary: \n")
  cat("\n lambdas used:\n")
  print(signif(x$lambda,digits))
  cat("\n taus used:\n")
  print(signif(x$tau,digits))
  cat("\n lp solver solving for Gamma used: x$lpfun.Gamma\n")
  cat("\n lp solver solving for Omega used: x$lpfun.Omega\n")
  cat(" perturb=", signif(x$perturb, digits),"\n")
}
