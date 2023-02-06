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
library(cowplot)
library(tidyverse)

# Load data --------------------------------------------------------------------
data <- readRDS(here("data", "estimation_panels", "env_eco_panel.rds")) %>% 
  filter(coop_name == "Buzos y Pescadores")

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
ts <- ggplot(data = data) +
  geom_rect(xmin= 2013.5, xmax = 2017.5, ymin = -Inf, ymax = Inf, fill = "gray") +
  stat_summary(mapping = aes(x = year, y = density, fill = period_long),
               geom = "pointrange", fun.data = mean_se, color = "gray10", shape = 21) +
  scale_x_continuous(breaks = seq(2006, 2020, by = 2), labels = seq(2006, 2020, by = 2)) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_fill_manual(values = period_palette) +
  guides(fill = guide_legend()) +
  theme_bw() +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_blank(),
        text = element_text(size = 6),
        ) +
  labs(x = "Year",
       y = bquote(Density~(org.~m^-1)),
       fill = "Period")

sst <- ggplot(data = data,
              mapping = aes(x = temp_mean, y = density)) +
  geom_smooth(method = "lm", color = "black") +
  stat_summary(aes(fill = period_long, group = year),
               geom = "pointrange", fun.data = mean_se, color = "gray10", shape = 21) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_fill_manual(values = period_palette) +
  theme_bw() +
  theme(legend.position = "None",
        text = element_text(size = 6)) +
  labs(x = "Mean SST",
       y = bquote(Density~(org.~m^-1)),
       fill = "Period")

c_mhw <- ggplot(data = data,
                mapping = aes(x = mhw_int_cumulative, y = density)) +
  geom_smooth(method = "lm", color = "black") +
  stat_summary(aes(fill = period_long, group = year),
               geom = "pointrange", fun.data = mean_se, color = "gray10", shape = 21) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_fill_manual(values = period_palette) +
  theme_bw() +
  theme(legend.position = "None",
        text = element_text(size = 6)) +
  labs(x = "MHW intensity",
       y = bquote(Density~(org.~m^-1)),
       fill = "Period")

n_mhw <- ggplot(data = data,
                mapping = aes(x = mhw_events, y = density)) +
  geom_smooth(method = "lm", color = "black") +
  stat_summary(aes(fill = period_long, group = year),
               geom = "pointrange", fun.data = mean_se, color = "gray10", shape = 21) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_fill_manual(values = period_palette) +
  theme_bw() +
  theme(legend.position = "None",
        text = element_text(size = 6)) +
  labs(x = "MHW events",
       y = bquote(Density~(org.~m^-1)),
       fill = "Period")

den_env <- plot_grid(sst, c_mhw, n_mhw, ncol = 3)
p <- plot_grid(ts, den_env, ncol = 1, rel_heights = c(1.5, 1))

startR::lazy_ggsave(plot = p,
                    filename = "env_ecol",
                    width = 17.4,
                    height = 12.4)

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------


reg <- data %>% 
  mutate_at(.vars = vars(temp_mean, mhw_int_cumulative, mhw_days, mhw_events),
            .funs = ~(.x - mean(.x)) / sd(.x))


m1 <- feols(log(density) ~ temp_mean + depth_m, reg)
m2 <- feols(log(density) ~ mhw_int_cumulative + depth_m, reg)
m3 <- feols(log(density) ~ mhw_events + depth_m, reg)
m4 <- feols(log(density) ~ period + depth_m, reg)

models <- list(m1, m2, m3, m4) %>%
  set_names(c("SST", "MHW int", "MHW events", "Periods"))

modelsummary::modelsummary(models = models,
                           stars = T,
                           coef_rename = c("temp_mean" = "Effect",
                                           "mhw_int_cumulative" = "Effect",
                                           "mhw_days" = "Effect",
                                           "mhw_events" = "Effect",
                                           "period0" = "Before MHW",
                                           "period1" = "During MHW",
                                           "period2" = "After MHW",
                                           "depth_m" = "Depth (m)"))

s1 <- stan_glm(log(density) ~ temp_mean + depth_m,
               data = reg)
s2 <- stan_glm(log(density) ~ mhw_int_cumulative + depth_m,
               data = reg)
s3 <- stan_glm(log(density) ~ mhw_events + depth_m,
               data = reg)
s4 <- stan_glm(log(density) ~ period + depth_m,
               data = reg,
               QR = T)

get_post <- function(model){
  ci50 <- posterior_interval(model, prob = c(0.5)) %>%
    as_tibble(rownames = "term", .name_repair = make.names)
  ci90 <- posterior_interval(model) %>%
    as_tibble(rownames = "term", .name_repair = make.names)
  tibble(term = names(coefficients(model)),
         estimate = coefficients(model)) %>%
    left_join(ci50, by = "term") %>%
    left_join(ci90, by = "term")
}

smodels <- list(s1, s2, s3, s4) %>% 
  set_names(c("SST", "MHW int", "MHW events", "Periods"))

map_dfr(smodels, get_post, .id = "model") %>%
  filter(!str_detect(term, "Inter"),
         !model == "Periods") %>%
  mutate(term = case_when(term == "temp_mean" ~ "SST",
                          term == "mhw_int_cumulative" ~ "MHW Cummulative intensity",
                          term == "mhw_events" ~ "MHW events",
                          term == "mhw_days" ~ "MHW days")) %>% 
  drop_na(term) %>% 
  ggplot(aes(x = term, y = estimate)) +
  geom_errorbar(aes(ymin = X5., ymax = X95.), size = 1, width = 0) +
  geom_errorbar(aes(ymin = X25., ymax = X75.), size = 2, width = 0) +
  geom_point(size = 5, shape = 21, fill = "cadetblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  theme_bw() +
  labs(x = "",
       y = "Effect on density")

plot(s4, plotfun = "areas", pars = c("period1", "period2")) +
  labs(title = "Posterior distributions") +
  scale_y_discrete(labels = c("During MHW", "After MHW"))


