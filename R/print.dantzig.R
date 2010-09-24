print.dantzig <- function(x,digits = max(3, getOption("digits") - 3), ... ) {
  cat("\n dantzig options summary: \n")
  cat("\n lambdas used:\n")
  print(signif(x$lambda,digits))
  cat("\n lp solver used: x$lpfun\n")
}
