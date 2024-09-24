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
  broom,
  tidyverse
)

# Load data --------------------------------------------------------------------
models <- readRDS(file = here("data", "output", "effect_on_fishery_models.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
eu_level_effects <- models %>% 
  filter(indep == "norm_mhw_int_cumulative") %>% 
  arrange(indep) %>% 
  mutate(coefs = map(fe_model, tidy)) %>% select(fishery, coefs) %>% unnest(coefs) %>%
  filter(str_detect(term, "norm_mhw_int_cumulative")) %>%
  mutate(term = str_extract(term, "[:digit:]{10}"),
         term = str_replace_all(term, "0", "*"),
         term = str_replace_all(term, "8", "°")) %>% 
  group_by(term) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n >= 2) %>% 
  mutate(term = fct_reorder(term, -estimate),
         p_fill = p.value < 0.05)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
p <- ggplot(data = eu_level_effects,
            mapping = aes(y = term, x = estimate, shape = fishery)) +
  geom_pointrange(aes(xmin = estimate - std.error,
                      xmax = estimate + std.error),
                  size = 1,
                  fill = period_palette[2],
                  position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_shape_manual(values = c(21, 22, 23)) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  labs(y = "Economic Unit",
       x = expression(hat(beta[i])),
       shape = "Fishery") +
  theme(legend.position = c(0, 0),
        legend.justification = c(0, 0))


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = p,
                    filename = "figS03_coef_plots_for_eus_many_spp",
                    width = 18,
                    height = 9)
