# AUC function

my_auc_func_spline <- function(x, y) {
  
  if(length(x) > 2) {
    result <- MESS::auc(x,
                  y,
                  from = min(0, na.rm = TRUE),
                  to = max(x, na.rm = TRUE),
                  type = c("spline"),
                  absolutearea = FALSE,
                  subdivisions = 100)
  }
  
  if(length(x) < 3) {
    result <- NA
  }
  result
}

my_auc_func_linear <- function(x, y) {
  
  if(length(x) > 2) {
    result <- MESS::auc(x,
                        y,
                        type = c("linear"),
                        absolutearea = FALSE)
  }
  
  if(length(x) < 3) {
    result <- NA
  }
  result
}



my_auc_func_raw <- function(x, y) {
  
  if(length(x) > 2) {
    result <- sum(y)
  }
  
  if(length(x) < 3) {
    result <- NA
  }
  result
}


