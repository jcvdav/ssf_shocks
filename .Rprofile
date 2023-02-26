source("renv/activate.R")

# mex_data_path <-
#   "/Users/juancarlosvillasenorderbez/GitHub/data_mex_fisheries/data"
# remote_data_path <-
#   "/Users/juancarlosvillasenorderbez/GitHub/data_remote/data"

# coop_eurnpa <- data.frame(
#   eu_rnpa = c(
#     "0301000089",
#     "0301000113",
#     "0313000028",
#     "0301000097",
#     "0301000105",
#     "0313000036",
#     "0203000021",
#     "0310000013",
#     "0203000278",
#     "0203000302",
#     "0203008305",
#     "0203008990",
#     "0203008198",
#     "0203126602",
#     "0203009261",
#     "0203010715",
#     "0203005673",
#     "0203002829",
#     "0203004726",
#     "0203126974",
#     "0203014063",
#     "0203006168",
#     "0203000351",
#     "0203007901"
#   ),
#   coop_name = c(
#     "Buzos y Pescadores",
#     "Bahia de Tortugas",
#     "California de San Ignacio",
#     "Emancipacion",
#     "La Purisima",
#     "Leyes de Reforma",
#     "Progreso",
#     "Punta Abreojos",
#     "Isla Cedros",
#     "Ensenada",
#     "Litoral de Baja California",
#     "Manchuria",
#     "Morro de Rosarito",
#     "Jauregui Hernandez",
#     "Pescadores Ribereños",
#     "Punta Canoas",
#     "Buzos de la Costa Occidental",
#     "Abuloneros y Langosteros",
#     "Regasa",
#     "Isla San Geronimo",
#     "Roca San Martin",
#     "Patrimonio en el mar",
#     "Rafael Ortega Cruz",
#     "Pescadores Ribereños de BC"
#   )
# )

periods <- dplyr::tibble(year = 1980:2022,
                         period = dplyr::case_when(year <= 2013 ~ "0",
                                            dplyr::between(year, 2014, 2017) ~ "1",
                                            year > 2017 ~ "2"),
                         period_long = dplyr::case_when(period == "0" ~ "Before MHW",
                                                 period == "1" ~ "During MHW",
                                                 period == "2" ~ "After MHW")) |> 
  dplyr::mutate(period_long = forcats::fct_reorder(period_long, year))


period_palette <- c("steelblue", "#E41A1C", "darkorange1", "cadetblue")

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
