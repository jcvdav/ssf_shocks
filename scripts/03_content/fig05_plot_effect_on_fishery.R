################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  cowplot,
  broom,
  magrittr,
  tidyverse
)

# Load data --------------------------------------------------------------------
models <- readRDS(file = here("data", "output", "effect_on_fishery_models.rds"))

# Define functions -------------------------------------------------------------
count_effects <- function(model) {
  tidy(model) %>%
    filter(str_detect(term, "[:digit:]")) %>%
    mutate(neg = estimate < 0, sig = p.value < 0.05) %>%
    count(neg, sig)
}
coefplot <- function(fishery, data, indep, model, pattern = "norm_mhw_int_cumulative:befTRUE"){
  
  # Get the title --------------------------------------------------------------
  title <- str_to_title(str_replace_all(fishery, "_", " "))
  
  critter <- ifelse(title == "Urchin",
                    here("data", "img", paste0(title, ".png")),
                    here("data", "img", paste0(title, "_90.png")))
  
  set_shape <- case_when(fishery == "lobster" ~ 21,
                         fishery == "sea_cucumber" ~ 22,
                         fishery == "urchin" ~ 24)
  
  # Build plotting data --------------------------------------------------------
  xlab <- expression(hat(beta[i]))
  
  plot_data <- broom::tidy(model) 
  
  ref <- model %>% 
    broom::tidy() %>%
    filter(str_detect(term, pattern)) %>% 
    pull(estimate) %>% 
    mean()
  
  plot_data <- plot_data %>% 
    filter(str_detect(term, pattern)) %>%
    mutate(term = str_extract(term, "[:digit:]{10}")) %>% 
    mutate(term = fct_reorder(term, -estimate),
           p_fill = p.value < 0.05,
           fill2 = (1 * p_fill) * estimate)
  
  hist <- ggplot(data = plot_data,
                 mapping = aes(x = estimate)) +
    geom_rect(xmin = -Inf, xmax = 0,
              ymin = -Inf, ymax = Inf,
              color = "#B1182B",
              fill = "#B1182B") +
    geom_rect(xmin = 0, xmax = Inf,
              ymin = -Inf, ymax = Inf,
              color = "#2166AB",
              fill = "#2166AB") +
    geom_histogram(aes(y = after_stat(count / sum(count))),
                   binwidth = 0.25,
                   color = "black",
                   fill = "gray") +
    scale_y_continuous(expand = expansion(mult = 0.05, 0),
                       labels = scales::percent) +
    scale_x_continuous(expand = expansion(0.05, 0)) +
    theme(axis.title = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
  # Build the plot -------------------------------------------------------------
  p <- ggplot(data = plot_data,
              mapping = aes(x = estimate,
                            y = term)) +
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = ref,
               linetype = "dashed") +
    geom_errorbarh(aes(xmin = estimate - std.error,
                       xmax = estimate + std.error),
                   color = "gray10",
                   height = 0) + 
    geom_point(aes(fill = fill2),
               shape = set_shape) +
    scale_fill_gradientn(colours = ipcc_temp, limits = c(-1, 1)) +
    scale_x_continuous(limits = c(-1.2, 1)) +
    labs(title = title,
         x = xlab,
         y = NULL,
         fill = xlab) +
    guides(fill = guide_colorbar(title = expression(hat(beta[i])),
                                 frame.colour = "black",
                                 ticks.colour = "black"),
           color = "none") +
    theme(legend.position = "none",
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
  if(title == "Lobster") {
    p <- p + theme(legend.position = c(0, 0),
                   legend.justification = c(0, 0))
  }
  
  if(class(model) == "lmerMod") {
    p <- p +
      geom_vline(xintercept = coef,
                 linetype = "dashed",
                 color = "red")
  }
  
  
  if(title == "Lobster") {
  p <- plot_grid(p,
                 hist,
                 ncol = 1,
                 rel_heights = c(4, 1),
                 labels = "auto",
                 align = "hv")
  } else {
    p <- plot_grid(p,
                   hist,
                   ncol = 1,
                   rel_heights = c(4, 1),
                   labels = c("", ""),
                   align = "hv")
  }
  
  p <- ggdraw() +
    draw_plot(p) +
    draw_image(critter,
               scale = 0.2,
               halign = 0.95,
               valign = 0.925)
  
  return(p)
}

## VISUALIZE ###################################################################
fe_plots <- models %>% 
  filter(indep == "norm_mhw_int_cumulative") %>% 
  arrange(indep) %>%
  mutate(plot = pmap(.l = list(fishery,
                               data,
                               indep,
                               fe_model),
                     .f = coefplot,
                     pattern = "norm_mhw_int_cumulative"),
         counts = map(fe_model, count_effects))

# Extract plots
fe_land_mhw_cum_int <- fe_plots %$% 
  plot_grid(plotlist = plot,
            ncol = 3, align = "hv")

fe_land_mhw_cum_int
# Extract tables
fe_plots %>% 
  select(fishery, counts) %>%
  unnest(counts)

fe_plots %>% 
  select(fishery, counts) %>%
  unnest(counts) %>%
  group_by(neg, sig) %>%
  summarize(n = sum(n)) %>% 
  janitor::adorn_percentages(denominator = "col") %>% 
  janitor::adorn_pct_formatting()

## EXPORT ######################################################################
startR::lazy_ggsave(plot = fe_land_mhw_cum_int,
                    filename = "fig05_effect_on_fishery",
                    width = 18,
                    height = 15)
