globalVariables(c(".", "Cat", "Cat1", "Cat2", "Corr", "Groups", "MADM", "Med", "Median", "Num", "Num1", "Num2", "Variable",
                  "cramerV", "data", "groups", "n", "prop", "qu", "r", "value", "variables", "x", "y", "n_na", "n_0", "n_Inf", "..density.."))

.onLoad <- function(libname, pkgname) {
  report_options()
}


.onUnLoad <- function(libname, pkgname) {
  rm(config_report)
}
