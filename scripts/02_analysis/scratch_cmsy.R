library(here)
library(datalimited2)
library(tidyverse)

fishery_data <- readRDS("data/processed/annual_fishery_panel.rds") 

env_data <- readRDS(here("data", "processed", "annual_environmental_panel.rds")) %>%
  select(year, coop_name, temp_mean, mhw_int_cumulative)

# ecol_data <- readRDS(here("data", "processed", "annual_ecological_panel.rds")) %>% 
  # filter(coop_name == "Buzos y Pescadores") %>% 
  # group_by(year) %>% 
  # summarize(density = mean(density, na.rm = T))


cmsy_wraper <- function(data) {
  cmsy_fit <- cmsy2(year = data$year,
        catch = data$landed_weight,
        r.low = 0.015, r.hi = 2)
  
  df <- cmsy_fit$ref_ts %>% 
    select(year, b, catch) %>% 
    mutate(blead = lead(b),
           sp = blead - b + catch,
           sst = (data$temp_mean - mean(data$temp_mean)),
           tb_sd = b / max(b, na.rm = T),
           sp_sd = sp / max(sp, na.rm = T)) %>% 
    drop_na()
  
  return(df)
}

my_data <- fishery_data %>% 
  left_join(env_data, by = c("year", "coop_name")) %>%
  select(coop_name, year, landed_weight, temp_mean) %>% 
  group_by(coop_name) %>% 
  nest() %>% 
  mutate(est = map(data, cmsy_wraper))

est_data <- my_data %>% 
  select(coop_name, est) %>% 
  unnest(est)


bsm_fit <- bsm(year = my_data$year,
               catch = my_data$landed_weight,
               biomass = my_data$density,
               btype = "CPUE",
               r.low = 0.015, r.hi = 1.5)

cmsy_data_obs <- cmsy_fit$ref_ts %>% 
  select(year, b, catch) %>% 
  mutate(blead = lead(b),
         sp = blead - b + catch,
         sst = (my_data$temp_mean - mean(my_data$temp_mean)),
         tb_sd = b / max(b, na.rm = T),
         sp_sd = sp / max(sp, na.rm = T)) %>% 
  drop_na()

bsm_data_obs <- bsm_fit$ref_ts %>% 
  select(year, b, catch) %>% 
  mutate(blead = lead(b),
         sp = blead - b + catch,
         sst = (my_data$temp_mean - mean(my_data$temp_mean)),
         tb_sd = b / max(b, na.rm = T),
         sp_sd = sp / max(sp, na.rm = T)) %>% 
  drop_na()

pop_sim <- function(r = 0.466, K = 1383945, N0 = 100, s = 0.5, y = 100) {
  
  set.seed(10)
  N <- numeric(10)
  N[1] <- N0#587916
  SST <- rnorm(n = y, mean = 0, sd = 1)
  
  for(i in 2:y) {
    N[i] <- N[i -1] + (((r * N[i-1] * (1 - (N[i-1] / K))) * exp(s * SST[i])))
  }
  
  result <- tibble(tb = N,
                     sst = SST,
                     sp = lead(N) - N,
                     tb_sd = tb / max(tb, na.rm = T),
                     sp_sd = sp / max(sp, na.rm = T)) %>% 
    drop_na()
  
  return(result)
  
}


data_sim <- pop_sim(s = 1)

simple <- nls(formula = sp_sd ~ r * tb_sd * (1 - (tb_sd / B0)),
              start = list(r = 0.4, B0 = 1.5),
              data = data_sim,
              control = list(maxiter = 1e3))

with_sst <- nls(formula = sp_sd ~ r * tb_sd * (1 - (tb_sd / B0)) * exp(sst * BetaT),
                start = list(r = 0.4, B0 = 1.5, BetaT = 0.5),
                data = data_sim,
                control = list(maxiter = 1e3))

modelsummary::modelsummary(list(simple, with_sst))

# plot(sp_sd ~ tb_sd, data = data_sim)
# lines(seq(0, 1, length.out = 100), predict(simple, newdata = tibble(tb_sd = seq(0, 1, length.out = 100))),
#       col = "blue")
# lines(seq(0, 1, length.out = 100), predict(with_sst, newdata = tibble(tb_sd = seq(0, 1, length.out = 100),
#                                             SST = 0)),
#       col = "red")

















## GOOD STUFF STARTS HERE

# load("ramldb_v3.8_production_data_final.Rdata")
short <- data %>% 
  ungroup() %>% 
  filter(stockid == "ACADRED2J3K") 

# # For list-based methods
# data_short <- list(
#   tb_sd = short$tb_sd,
#   sp_sd = short$sp_sd,
#   sst = short$had_sst_c_sd
# )

# Vars
B_t <-    bsm_data_obs$tb_sd
P_t <-    bsm_data_obs$sp_sd
Temp_t <- bsm_data_obs$sst

# Approach 1, NLS
simple <- nls(formula = P_t ~ r * B_t * (1 - (B_t / B0)),
              start = list(r = 0.4, B0 = 1.5),
              control = list(maxiter = 1e3))

with_nls <- nls(formula = P_t ~ r * B_t * (1 - (B_t / B0)) * exp(Temp_t * BetaT),
                start = list(r = 0.4, B0 = 1.5, BetaT = 0.5),
                control = list(maxiter = 1e3))

plot(P_t ~ B_t)
lines(seq(0, 1, length.out = 100), predict(simple, newdata = tibble(B_t = seq(0, 1, length.out = 100))),
      col = "blue")
lines(seq(0, 1, length.out = 100), predict(with_nls, newdata = tibble(B_t = seq(0, 1, length.out = 100),
                                                                      Temp_t = 0)),
      col = "red")

modelsummary::modelsummary(list(simple, with_nls))

# Define log-likelihood model for OPTIM
nll <- function(params) {
  # Data
  # B_t <- data$tb_sd
  # P_t <- data$sp_sd
  # Temp_t <- data$sst
  
  # Parameters
  ln_B0 <- params["ln_B0"]
  ln_r <- params["ln_r"]
  BetaT <- params["BetaT"]
  ln_sigmaP <- params["ln_sigmaP"]
  # mu_T <- params["mu_T"]
  # ln_sd_T <- params["ln_sd_T"]
  
  # Convert
  nll <- 0
  # sd_T <- exp(ln_sd_T)
  B0 <- exp(ln_B0)
  r <- exp(ln_r)
  sigmaP <- exp(ln_sigmaP)
  
  
  P_t_exp <- r * B_t * (1 - (B_t / B0)) * exp(Temp_t * BetaT)
  
  nll <- -sum(dnorm(P_t, P_t_exp, sigmaP, log = TRUE))
  
  # Check for random effects
  # nll <- nll -sum(dnorm(BetaT, mu_T, sd_T, log = TRUE))
  # Penalty
  pen <- 0
  eps <- 1e-3
  B_check <- 5 - B0
  pen <- pen + ifelse(B_check < eps, 0.01 * ((B_check - eps) ^ 2), 0)
  B_check <- ifelse(B_check >= eps, B_check, (eps / (2 - B_check / eps)))
  
  nll <- nll + pen
  
  return(nll)
}

p <- c(
  ln_B0=log(1),
  ln_r=log(0.4),
  BetaT=0.0,
  ln_sigmaP=log(0.2)
)

optim_fit <- optim(par = p,
                   fn = nll,
                   method = "BFGS",
                   hessian = TRUE,
                   control = list(maxit = 1e4))

# Define log-likelihood model for MLE
nll2 <- function(ln_B0, ln_r, BetaT, ln_sigmaP) {
  # Data
  #B_t <- data$tb_sd
  #P_t <- data$sp_sd
  #Temp_t <- data$sst
  
  # Parameters
  # ln_B0 <- params["ln_B0"]
  # ln_r <- params["ln_r"]
  # BetaT <- params["BetaT"]
  # mu_T <- params["mu_T"]
  # ln_sd_T <- params["ln_sd_T"]
  
  # Convert
  nll <- 0
  # sd_T <- exp(ln_sd_T)
  B0 <- exp(ln_B0)
  r <- exp(ln_r)
  sigmaP <- exp(ln_sigmaP)
  
  
  P_t_exp <- r * B_t * (1 - (B_t / B0)) * exp(Temp_t * BetaT)
  nll <- -sum(dnorm(P_t, P_t_exp, sigmaP, log = TRUE))
  
  # Penalty
  pen <- 0
  eps <- 1e-3
  B_check <- 5 - B0
  pen <- pen + ifelse(B_check < eps, 0.01 * ((B_check - eps) ^ 2), 0)
  B_check <- ifelse(B_check >= eps, B_check, (eps / (2 - B_check / eps)))
  
  nll <- nll + pen
  
  return(nll)
}

# mle_fit <- mle(minuslogl = nll2,
#                start = p,
#                nobs = length(B_t),
#                control = list(maxit = 1e3))

mle_wraper <- function(data){
  browser()
  # with(data = list(
    B_t = data$tb_sd
    P_t = data$sp_sd
    Temp_t = data$sst
  # ), expr = {
    
    stats4::mle(minuslogl = nll2,
                start = p,
                nobs = length(B_t),
                control = list(maxit = 1e3))
  # }
  # )
  
}


coop_mle <- my_data %>% 
  mutate(mle_fit = map(est, mle_wraper))

### ASSAMBlE ESTIMATES
nls_est <- broom::tidy(with_nls) %>% 
  select(param = term, estimate, stderror = std.error) %>% 
  mutate(source = "nls",
         estimate = ifelse(param %in% c("r", "B0"), log(estimate), estimate),
         stderror = ifelse(param %in% c("r", "B0"), log(stderror), stderror))

optim_est <- tibble(param = names(optim_fit$par),
                    estimate = optim_fit$par,
                    stderror = sqrt(diag(solve(optim_fit$hessian))),
                    source = "optim") 

mle_est <- mle_fit %>%
  summary() %>%
  coef() %>%
  as_tibble(rownames = "param") %>% 
  mutate(source = "mle") %>% 
  select(param, estimate = Estimate, stderror = `Std. Error`, source)

# #load("ramldb_v3.8_spsst_had.Rdata")
# free_est <- results.df %>% 
#   # filter(stockid == "ACADRED2J3K") %>% 
#   # filter(param %in% c("r", "B0", "BetaT")) %>% 
#   # select(-stockid) %>% 
#   mutate(source = "free")

re <- rbind(nls_est, optim_est, mle_est) %>% 
  mutate(param = ifelse(param == "B0", "ln_B0", param),
         param = ifelse(param == "r", "ln_r", param)) %>% 
  filter(!str_detect(param, "sigma")) 

ggplot(data = re, aes(x = source, y = estimate, color = source)) + 
  geom_pointrange(aes(ymin = estimate - stderror, ymax = estimate + stderror)) +
  facet_wrap(~param, scales = "free_y")


re %>%
  select(param, estimate, source) %>%
  pivot_wider(names_from = param, values_from = estimate)


stock_id <- c(1:2)
nstocks <- length(stock_id)


B_t <- list(
  B_t,
  B_t
)

P_t <- list(
  P_t,
  P_t
)


Temp_t <- list(
  Temp_t,
  Temp_t
)


re_params <- list(ln_B0 = rep(log(1.5), nstocks),
                  ln_r = rep(log(0.4), nstocks),
                  BetaT = rep(0.0, nstocks),
                  ln_sigmaP = rep(-2.5, nstocks), # 0 before, -2.5 based on model fits
                  mu_T = 0.0,
                  ln_sd_T = -1.25) # -3 before, -1.25 based on model fits

# Implement random effects
re_sst <- function(ln_B0 = log(1.5),
                   ln_r = log(0.4),
                   BetaT = 0.0,
                   ln_sigmaP = -2.5,
                   mu_T = 0.0,
                   ln_sd_T = 1.25) {
  # browser()
  # Data
  # B_t <- data$tb_sd
  # P_t <- data$sp_sd
  # Temp_t <- data$sst
  
  # Parameters
  # ln_B0 <- params$ln_B0
  # ln_r <- params$ln_r
  # BetaT <- params$BetaT
  # ln_sigmaP <- params$ln_sigmaP
  # mu_T <- params$mu_T
  # ln_sd_T <- params$ln_sd_T
  
  # Convert
  nll <- 0
  sd_T <- exp(ln_sd_T)
  B0 <- exp(ln_B0)
  r <- exp(ln_r)
  sigmaP <- exp(ln_sigmaP)
  
  for(i in 1:length(stock_id)) {
    # Expected productivity
    P_t_exp <- r[i] * B_t[[i]] * (1 - (B_t[[i]] / B0[i])) * exp(Temp_t[[i]] * BetaT[i])
    
    # Accumulate negative log likelihood
    nll <- nll -sum(dnorm(P_t[[i]], P_t_exp, sigmaP[i], log = TRUE)) 
  }
  
  # Check for random effects
  nll <- nll -sum(dnorm(BetaT, mu_T, sd_T, log = TRUE))
  # Penalty
  pen <- 0
  eps <- 1e-3
  B_check <- 5 - B0
  for(i in 1:length(stock_id)){
    pen <- pen + ifelse(B_check[i] < eps, 0.01 * ((B_check[i] - eps) ^ 2), 0)
    B_check[i] <- ifelse(B_check[i] >= eps, B_check[i], (eps / (2 - B_check[i] / eps))) 
  }
  
  nll <- nll + pen
  
  return(nll)
}

# re_sst()

# re_optim <- optim(par = re_params,
#                   fn = re_sst,
#                   control = list(maxit = 1e3))

re_mle <- mle2(minuslogl = re_sst,
               start = re_params)
