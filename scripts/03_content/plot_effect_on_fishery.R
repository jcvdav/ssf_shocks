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
library(here)
# library(sf)
library(lme4)
library(ggimage)
library(cowplot)
library(broom)
library(magrittr)
library(tidyverse)

# Load data --------------------------------------------------------------------
models <- readRDS(file = here("data", "output", "effect_on_fishery_models.rds"))

# Define functions -------------------------------------------------------------
coefplot <- function(fishery, data, model){
  # browser()
  
  # Get the title --------------------------------------------------------------
  title <- str_to_title(str_replace_all(fishery, "_", " "))
  
  img <- ifelse(title == "Urchin",
                here("data", "img", paste0(title, ".png")),
                here("data", "img", paste0(title, "_90.png")))
  
  # Get the max MHW ------------------------------------------------------------
  max_mhw <- data %>% 
    group_by(eu_rnpa) %>% 
    summarize(max_mhw_int_cumulative = max(norm_mhw_int_cumulative)) %>% 
    ungroup()
  
  # Build plotting data --------------------------------------------------------
  if(class(model) == "fixest") {
    
    xlab <- expression(hat(beta[i]))
    
    plot_data <- broom::tidy(model) %>% 
      filter(!str_detect(term, "year")) %>% 
      mutate(term = str_extract(term, "[:digit:]{10}")) %>% 
      left_join(max_mhw, by = c("term" = "eu_rnpa")) %>% 
      mutate(term = fct_reorder(term, -estimate),
             p_fill = (p.value < 0.05) * estimate)  
  }
  
  if(class(model) == "lmerMod") {
    
    xlab <- expression(beta[re])
    singular <- isSingular(model)
    
    coef <- fixef(model)[2]
    vars <- model %>% vcov() %>% diag()
    sd <- sqrt(vars[2])
    
    plot_data <- model %>%
      ranef() %>% 
      as_tibble() %>%
      filter(str_detect(term, "temp|mhw")) %>% 
      mutate(estimate = coef + condval) %>% 
      select(term = grp, estimate, std.error = condsd) %>% 
      left_join(max_mhw, by = c("term" = "eu_rnpa")) %>% 
      mutate(term = fct_reorder(term, -estimate))  
  }
  
  # Build the plot -------------------------------------------------------------
  p <- ggplot(data = plot_data,
              mapping = aes(x = estimate,
                            y = term)) +
    geom_image(image = img,
               x = 0.9 * max(plot_data$estimate),
               y = 0.9 * length(unique(plot_data$term)),
               inherit.aes = F,
               size = 0.2) +
    geom_vline(xintercept = 0,
               linetype = "dashed",
               linewidth = 1) +
    geom_pointrange(aes(fill = p_fill,
                        xmin = estimate - std.error,
                        xmax = estimate + std.error),
                    shape = 21) + 
    scale_fill_gradient2(low = "#E41A1C", mid = "white", high = "steelblue", midpoint = 0) +
    # scale_alpha_manual(values = c(0.5, 1)) +
    labs(title = title,
         x = xlab,
         y = NULL,
         fill = xlab) +
    guides(fill = guide_colorbar(frame.colour = "black",
                                 ticks.colour = "black")) +
    theme(legend.position = "None")
  
  if(class(model) == "lmerMod") {
    p <- p +
      geom_vline(xintercept = coef,
                 linetype = "dashed", color = "red")
  }
  
  # if(singular){
  #   p <- ggplot() + geom_text(x = 0, label = "ESTIMATION IS VOID")
  # }
  
  return(p)
}

## VISUALIZE ###################################################################
fe_plots <- models %>% 
  arrange(indep) %>%
  mutate(plot = pmap(.l = list(fishery,
                               data, 
                               fe_model),
                     .f = coefplot))

fe_land_mhw_cum_int <- fe_plots %>% 
  filter(indep == "norm_mhw_int_cumulative") %$% 
  plot_grid(plotlist = plot, ncol = 3)

fe_land_mhw_cum_int

# re_plots <- models %>% 
#   arrange(indep) %>% 
#   mutate(plot = pmap(.l = list(fishery,
#                                data, 
#                                re_model),
#                      .f = coefplot))

## EXPORT ######################################################################
startR::lazy_ggsave(plot = fe_land_mhw_cum_int,
                    filename = "land_coef_plot",
                    width = 18,
                    height = 10)


# NEEd to order panels so that they are lobster, cucumber, urchin panels