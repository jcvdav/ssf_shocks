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
  tidyverse
)

# Load data --------------------------------------------------------------------
coef_data <- readRDS(file = here("data", "output", "effect_on_fishery_and_biophysical.rds"))
con_p_mhw_threshold_future <- readRDS(file = here("data", "output", "con_p_mhw_threshold_future.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
plot_data <- con_p_mhw_threshold_future %>% 
  left_join(coef_data, by = c("fishery", "eu_rnpa")) %>% 
  mutate(neg_neg = 1 * ((estimate < 0) & (mean >= 0.025)),
         ssp = str_remove(ssp, "delta_"),
         ssp = case_when(ssp == "ssp126" ~ "SSP1-2.6",
                         ssp == "ssp245" ~ "SSP2-4.5",
                         ssp == "ssp585" ~ "SSP5-8.5"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

p1 <- ggplot(data = plot_data,
            aes(x = mean, y = estimate, fill = ssp, shape = fishery)) +
  geom_rect(xmin = 0.025, xmax = Inf, ymin = -Inf, ymax = 0, inherit.aes = F, fill = "gray", alpha = 0.05) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.025, linetype = "dashed") +
  geom_errorbarh(aes(xmin = mean - se, xmax = mean + se),
                 linewidth = 0.2, height = 0) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                linewidth = 0.2, width = 0) +
  geom_point(aes(alpha = neg_neg), size = 2) +
  scale_fill_manual(values = ssp_palette) +
  scale_shape_manual(values = c(21, 22, 24)) +
  scale_alpha_binned(range = c(0.25, 1)) +
  facet_grid(fishery ~ ssp) +
  guides(fill = guide_legend(title = "SSP",
                             override.aes = list(shape = 21),
                             title.position = "top"),
         shape = guide_legend(title = "Fishery",
                              title.position = "top"),
         alpha = "none") +
  labs(x = expression("P((Cum. Int. ">="hist Cum. Int.) | MHW Occurs)"),
       y = expression(hat(beta[i]))) +
  theme(legend.position = "bottom")

p2 <- plot_data %>%
  group_by(fishery, ssp) %>%
  summarize(n = sum(neg_neg)) %>%
  ggplot(aes(x = ssp, y = n, shape = fishery, fill = ssp)) +
  geom_line(aes(group = fishery)) +
  geom_point(size = 4) +
  scale_y_continuous(breaks = seq(0, 10, by = 5), labels = seq(0, 10, by = 5), limits = c(0, 12)) +
  scale_fill_manual(values = ssp_palette) +
  scale_shape_manual(values = c(21, 22, 24)) +
  theme(legend.position = "None") +
  labs(x = "SSP",
       y = "Number of economic units")

p <- plot_grid(p1, p2,
               ncol = 2,
               rel_widths = c(3, 1),
               labels = "auto", label_x = 0.85)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(
  plot = p,
  filename = "fig_7_future_mhw_and_coefficients",
  width = 18,
  height = 14
)
