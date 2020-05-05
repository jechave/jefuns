#' Tile plot of a matrix
#'
plot_matrix <- function(m, row_name = "i", col_name = "j", value_name = "mij") {
  df <- matrix_to_tibble(m)
  df %>%
    ggplot(aes(x = j, y = i, fill = mij)) +
    geom_tile() +
    scale_fill_viridis_b() +
    theme_cowplot() +
    xlab(col_name) +
    ylab(row_name) +
    labs(fill = value_name) +
    coord_fixed() +
    NULL
}
