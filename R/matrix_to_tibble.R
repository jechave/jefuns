#' turn any matrix into a tibble
#'
#' @export
matrix_to_tibble <- function(m, row_name = "i", col_name = "j", value_name = "mij") {
  result <- m %>%
    as.data.frame() %>%
    dplyr::mutate(i = seq(nrow(m))) %>%
    tidyr::pivot_longer(cols = 1:ncol(m),
                 names_to = "j",
                 names_prefix = "V",
                 values_to = "mij") %>%
    dplyr::mutate(j = as.integer(j))  %>%
    tibble::as_tibble() %>%
    arrange(j, i)

  names(result) <-  c(row_name, col_name, value_name)

  result
}
