#' Linear model with infinite value check
#'
#' @param formula Linear regression model y ~ x
#' @param data Data frame containing the data
#'
#' @return A linear model object with slope replaced with NA if the data contains any Inf values
#' @export
#'
#' @examples NULL
lm_with_inf_check <- function(formula, data) {
  # Check if the data contains any Inf values
  if (any(is.infinite(rowSums(data)))) {

    # remove rows with Inf values
    data <- data[is.finite(rowSums(data)),]

    # Fit the model normally
    model <- lm(formula, data = data)

    # Modify the coefficients to set the slope to NA
    model$coefficients[2] <- NA
     return(model)

  }

  else {
    # Fit and return the normal model
    return(lm(formula, data = data))
  }
}
