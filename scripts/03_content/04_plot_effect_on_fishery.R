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
  lme4,
  cowplot,
  broom,
  magrittr,
  tidyverse
)

# Load data --------------------------------------------------------------------
models <- readRDS(file = here("data", "output", "effect_on_fishery_models.rds"))

# Define functions -------------------------------------------------------------
coefplot <- function(fishery, data, indep, model, pattern = "norm_mhw_int_cumulative:befTRUE"){
  
  # Get the title --------------------------------------------------------------
  title <- str_to_title(str_replace_all(fishery, "_", " "))
  
  critter <- ifelse(title == "Urchin",
                    here("data", "img", paste0(title, ".png")),
                    here("data", "img", paste0(title, "_90.png")))
  
  # Build plotting data --------------------------------------------------------
  if(class(model) == "fixest") {
    
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
             p_fill = p.value < 0.05)#estimate)
      
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
    # geom_image(image = img,
    # x = 0.75,
    # y = 0.9 * length(unique(plot_data$term)),
    # inherit.aes = F,
    # size = 0.2) +
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = ref,
               linetype = "dashed") +
    geom_pointrange(aes(fill = estimate,
                        xmin = estimate - std.error,
                        xmax = estimate + std.error),
                    color = "gray10",
                    shape = 21) + 
    geom_point(aes(color = p_fill)) + 
    scale_fill_gradient2(low = "#E41A1C",
                         mid = "white",
                         high = "steelblue",
                         midpoint = 0) +
    scale_color_manual(values = c("gray50", "transparent")) +
    scale_x_continuous(limits = c(-0.5, 0.8)) +
    labs(title = title,
         x = xlab,
         y = NULL,
         fill = xlab) +
    guides(fill = guide_colorbar(title = expression(hat(beta[i])),
                                 frame.colour = "black",
                                 ticks.colour = "black"),
           color = "none") +
    theme(legend.position = c(0, 0),
          legend.justification = c(0, 0))
  
  if(class(model) == "lmerMod") {
    p <- p +
      geom_vline(xintercept = coef,
                 linetype = "dashed", color = "red")
  }
  
  # if(singular){
  #   p <- ggplot() + geom_text(x = 0, label = "ESTIMATION IS VOID")
  # }
  
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
                     pattern = "norm_mhw_int_cumulative:befTRUE"))

fe_land_mhw_cum_int <- fe_plots %$% 
  plot_grid(plotlist = plot, ncol = 3)

fe_land_mhw_cum_int


## EXPORT ######################################################################
startR::lazy_ggsave(plot = fe_land_mhw_cum_int,
                    filename = "land_coef_plot",
                    width = 18,
                    height = 12)
