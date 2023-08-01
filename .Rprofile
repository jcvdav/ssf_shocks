source("renv/activate.R")

if(!require(pacman)) {install.packages("pacman")}

periods <- dplyr::tibble(year = 1980:2022,
                         period = dplyr::case_when(year <= 2013 ~ "0",
                                            dplyr::between(year, 2014, 2016) ~ "1",
                                            year > 2016 ~ "2"),
                         period_long = dplyr::case_when(period == "0" ~ "Before MHW",
                                                 period == "1" ~ "During MHW",
                                                 period == "2" ~ "After MHW")) |>
  dplyr::mutate(period_long = forcats::fct_reorder(period_long, year))


period_palette_old <- c("steelblue", "#E41A1C", "darkorange1", "cadetblue")
period_palette <- c("#4cacba", "#f22300", "#e1af00")

ihs <- function(x){
  log(x + sqrt((x ^ 2) + 1))
}

# Set a global theme -----------------------------------------------------------
ggplot2::theme_set(
  ggplot2::theme_bw()
  )
ggplot2::theme_update(
  panel.grid.major = ggplot2::element_line(color = "black",
                                           linewidth = 0.1,
                                           linetype = "dashed"),
  panel.grid.minor = ggplot2::element_blank(),
  legend.background = ggplot2::element_blank(),
  legend.key = ggplot2::element_blank(),
  strip.background = ggplot2::element_blank(),
  strip.text = ggplot2::element_text(hjust = 0 ),
  text = ggplot2::element_text(size = 8),
  axis.text.y = ggplot2::element_text(size = 5)
)
