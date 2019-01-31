#' Perform a single equation regression model on the data
#' @export
#' @param data dataframe for the regression model
#' @param formula formula for the regression model
#' @param label character string with the name of the test
#' @param variable character string with the name of the variable that tests for
#' @param nearly_correction boolean for nearly exact correction
#' according to Young (2016)
#' the interdependency

single_reg <- function(data, formula, label = as.character(formula),
                       variable, nearly_correction = FALSE){
  reg <- lm(formula = formula, data = data)
  summ <- summary(reg)
  result <-  data.frame(
               label = label,
               coefficient = reg$coefficients[variable],
               se = summ$coefficients[variable, "Std. Error"],
               df = summ$df[2],
               stat = summ$coefficients[variable, "t value"],
               pvalue = summ$coefficients[variable, "Pr(>|t|)"],
               r2 = summ$r.squared
    )
  if (nearly_correction) {
    x <- model.matrix(object = formula, data = data)
    covHC <- sandwich::vcovHC(x = reg, type = "HC1")
    seHC <- sqrt(diag(covHC)[variable])
    hypothesis <- colnames(x) == variable
    correction <- nearly_exact(x = x, w = hypothesis)
    se = seHC / sqrt(correction$mu)
    stat <- reg$coefficients[variable]/se
    result <- rbind(result,
      data.frame(
        label = paste("nearly_exact", label, sep = "_"),
        coefficient = reg$coefficients[variable],
        se = se,
        df = correction$edf,
        stat = stat,
        pvalue = 2 * pt(abs(stat), correction$edf,
                        lower.tail = FALSE),
        r2 = summ$r.squared
      )
    )
  }
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
  x <- model.matrix(object = formula, data = data)
  boot_out <- boot::boot(data = cbind(data$y, x),
                         statistic = boot_helper,
                         formula = formula, variable = variable,
                         R = bootR)
  boot_ci <- boot::boot.ci(boot.out = boot_out, type = "bca")
  result <- data.frame(
    label = label,
    coefficient = boot_out$t0,
    sterror = sd(boot_out$t),
    lower_interval = boot_ci$bca[4],
    higher_interval = boot_ci$bca[5]
  )
}

boot_helper <- function(data, formula, variable, indices){
  x <- data[indices, 2:ncol(data)]
  y <- data[indices, 1]
  lm <- lm.fit(x = x, y = y)
  return(coef(lm)[variable])
}
#' Calculate the nearly exact bias and edf for the HC1 Variance Estimator.
#' @param x design matrix of the regression
#' @param w hypothesis test
nearly_exact <- function(x, w){
  N = nrow(x)
  k = ncol(x) - 1
  c = N / (N - k)
  xx <- solve(crossprod(x))
  M = diag(rep(1, N)) - tcrossprod(x %*% xx, x)
  z = tcrossprod(t(w) %*% xx, x)
  zz = z %*% t(z)
  mu = c/zz * sum(z^2 * diag(M))
  z2 = z^2
  nu = 2 * c^2 / zz^2 * (tcrossprod(z2 %*% M^2, z2))
  edf = 2 * mu^2 / nu
  return(list(mu = as.numeric(mu), edf = as.numeric(edf)))
}
