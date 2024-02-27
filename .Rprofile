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

period_palette <- c("#006D2C", "#A40F15", "#08519B", "#3181BC")

# From https://www.ipcc.ch/site/assets/uploads/2022/09/IPCC_AR6_WGI_VisualStyleGuide_2022.pdf
# Page 9
ssp_palette <- c("#173c66", "#f79420", "#951b1e")

ihs <- function(x){
  log(x + sqrt((x ^ 2) + 1))
}

# Set a global theme -----------------------------------------------------------
ggplot2::theme_set(
  ggplot2::theme_bw()
  )
ggplot2::theme_update(
  line = ggplot2::element_line(color = "black",
                               linewidth = 0.176389),
  panel.grid = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  legend.background = ggplot2::element_blank(),
  legend.key = ggplot2::element_blank(),
  strip.background = ggplot2::element_blank(),
  strip.text = ggplot2::element_text(hjust = 0 ),
  text = ggplot2::element_text(size = 8)
)


ipcc_temp <- dplyr::tribble(~"R", ~"G", ~"B",
                            103, 0, 31,
                            178, 24, 43,
                            214, 96, 77,
                            244, 165, 130,
                            253, 219, 199,
                            247, 247, 247,
                            209, 229, 240,
                            146, 197, 222,
                            67, 147, 195,
                            33, 102, 172,
                            5, 48, 97)

ipcc_temp <- grDevices::rgb(red = ipcc_temp$R,
                            green = ipcc_temp$G,
                            blue = ipcc_temp$B,
                            maxColorValue = 256)

# Build readme

build_readme <- function (repo = "Repository") {
  twee <- function(path, level = 1) {
    fad <- list.files(path = path, recursive = TRUE, no.. = TRUE, 
                      include.dirs = TRUE)
    fad_split_up <- strsplit(fad, "/")
    too_deep <- lapply(fad_split_up, length) > level
    fad_split_up[too_deep] <- NULL
    jfun <- function(x) {
      n <- length(x)
      if (n > 1) 
        x[n - 1] <- "|__"
      if (n > 2) 
        x[1:(n - 2)] <- "   "
      x <- if (n == 1) 
        c("-- ", x)
      else c("   ", x)
      x
    }
    fad_subbed_out <- lapply(fad_split_up, jfun)
    tree <- unlist(lapply(fad_subbed_out, paste, collapse = ""))
    return(tree)
  }
  writeLines(text = c(paste0("# ", repo, "\n\n"), "## Repository structure \n", 
                      "```", twee(path = getwd(), level = 3), "```", "\n---------"), 
             con = "README.md")
}
