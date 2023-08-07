# Plot the coefficients of those with more than one species, just to look at concordance of effects

models <- readRDS(file = here("data", "output", "effect_on_fishery_models.rds"))

eu_level_effects <- models %>% 
  filter(indep == "norm_mhw_int_cumulative") %>% 
  arrange(indep) %>% 
  mutate(coefs = map(fe_model, tidy)) %>% select(fishery, coefs) %>% unnest(coefs) %>%
  filter(str_detect(term, "norm_mhw_int_cumulative:befTRUE")) %>%
  mutate(term = str_extract(term, "[:digit:]{10}")) %>% 
  group_by(term) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n >= 2) %>% 
  mutate(term = fct_reorder(term, -estimate),
         p_fill = p.value < 0.05)

p <- ggplot(data = eu_level_effects,
            mapping = aes(y = term, x = estimate, shape = fishery)) +
  geom_pointrange(aes(xmin = estimate - std.error,
                      xmax = estimate + std.error),
                  size = 1,
                  fill = period_palette[2],
                  position = position_jitter(width = 0, height = 0.25)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_shape_manual(values = c(21, 22, 23)) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  labs(y = "Economic Unit",
       x = expression(hat(beta[i])),
       shape = "Fishery")

startR::lazy_ggsave(plot = p,
                    filename = "figSx_coef_plots_for_eus_many_spp",
                    width = 16,
                    height = 12)
