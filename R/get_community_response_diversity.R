#' Get measures of community response diversity
#'
#' @param temp_derivs Connection to database containing temporal derivatives
#'
#' @return A dataset containing measures of community response diversity
#' @export
#'
#' @examples NULL
get_community_response_diversity <- function(temp_derivs) {

  temp2 <- temp_derivs |>
    group_by(case_id, temperature) %>%
    summarise(mean_deriv = mean(derivative),
              sum_deriv = sum(derivative),
              max_igr = max(igr)) |>
    collect()
  summary_derivs <- temp2 |>
    group_by(case_id) |>
    summarise(mean_abs_deriv = mean(abs(mean_deriv)),
              sum_abs_deriv =  sum(abs(mean_deriv)),
              sum_max_igr = sum(max_igr))

  return(summary_derivs)

}
