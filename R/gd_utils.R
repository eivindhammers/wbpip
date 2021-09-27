#' Perform linear regression on Lorenz formatted input
#'
#' `regres()` performs linear regression on lorenz formatted input (Beta or
#' Quadratic). There is no intercept in the regression. The coefficients of
#' regressions are estimated by ordinary least squares.
#'
#' @param data list: Output of `create_functional_form_lq()` or
#' `create_functional_form_lb()`.
#' @param is_lq logical: TRUE if Lorenz Quadratic, FALSE if Beta Lorenz.
#'
#' @return list
#' @keywords internal
regres <- function(data, is_lq = TRUE) {

  y = data$y
  X = data$X

  # Run regression
  res <- fast_lm(y = y, X = X)

  # Calculate stats
  ymean <- sum(y) / length(y)
  sst <- sum((y - ymean)^2) # sum of square total
  coef <- as.vector(res$coefficients) # regression coefs
  sse <- sum(res$residuals^2) # sum of square error
  r2 <- 1 - sse / sst # R-square (This is the R2 formula for models with an intercept)
  mse <- sse / (length(y) - length(coef)) # Mean squared error
  se <- as.vector(res$stderr) # Standard error

  # REVIEW:
  # Why exp() if isLQ == FALSE?
  if (!is_lq) {
    coef[1] <- exp(coef[1])
  }

  return(list(
    ymean = ymean,
    sst = sst,
    coef = coef,
    sse = sse,
    r2 = r2,
    mse = mse,
    se = se
  ))
}
