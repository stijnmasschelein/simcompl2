#' Perform a single equation regression model on the data
#' @export
#' @param data dataframe for the regression model
#' @param formula formula for the regression model
#' @param label character string with the name of the test
#' @param variable character string with the name of the variable that tests for
#' the interdependency

single_reg <- function(data, formula, label = as.character(formula),
                       variable){
  reg <- lm(formula = formula, data = data)
  summ <- summary(reg)
  result <-  data.frame(
               coefficient = reg$coefficients[variable],
               stat = summ$coefficients[variable, "t value"],
               pvalue = summ$coefficients[variable, "Pr(>|t|)"],
               r2 = summ$r.squared,
               label = label
             )
  return(result)
}
#' Perform a single equation regression model on the data
#' @export
#' @param data dataframe for the regression model
#' @param formula formula for the regression model
#' @param label character string with the name of the test
#' @param variable character string with the name of the variable that tests for
#' the interdependency
#' @param bootR integer value for the number of bootstrap repetitions

boot_reg <- function(data, formula, label = as.character(formula),
                     variable, bootR = 2000){
  boot_out <- boot::boot(data = data,
                         statistic = boot_helper,
                         formula = formula, variable = variable,
                         R = bootR)
  boot_ci <- boot::boot.ci(boot.out = boot_out, type = "bca")
  result <- data.frame(
    coefficient = boot_out$t0,
    sterror = sd(boot_out$t),
    lower_interval = boot_ci$bca[4],
    higher_interval = boot_ci$bca[5]
  )
}

boot_helper <- function(data, formula, variable, indices){
  d <- data[indices,]
  lm <- lm(formula = formula, data = d)
  return(coef(lm)[variable])
}
