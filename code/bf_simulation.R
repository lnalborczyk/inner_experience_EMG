########################################################
# Re-analysing the data from Moffatt et al. (2020)     #
# Simulating the effect of sample size on BFs          #
# ---------------------------------------------------- #
# Ladislas Nalborczyk                                  #
# Last updated on 30.09.2020                           #
# https://github.com/lnalborczyk/inner_experience_EMG  #
########################################################

library(BayesFactor)
library(tidyverse)
library(brms)

# imports data
df <- read_excel("data/RUM_master_data.xlsx")

# reshapes data
df2 <- df %>%
    select(ID, order = Order, RUM_FRO_MAX_ln:BAS_OOI_MAX_ln) %>%
    na.omit %>%
    # reshapes data from wide to long format
    pivot_longer(
        cols = starts_with(c("RUM", "DIS", "BAS") ),
        names_to = "condition",
        values_to = "value",
        values_drop_na = TRUE
    ) %>%
    separate(
        col = condition,
        into = c("condition", "muscle"),
        sep = "_", extra = "merge"
    ) %>%
    # extracts muscle name
    mutate(muscle = substr(muscle, 1, 3) ) %>%
    # from long to wide
    pivot_wider(names_from = muscle, values_from = value)

# imports model
varying_effects <- readRDS("models/varying_effects.rds")

# function for simulating BFs
simulating_bfs <- function (n_obs) {
    
    # extracting posterior samples from varying-effects model
    post <- posterior_samples(varying_effects)
    
    # number of simulated participants
    n_ppts <- n_obs
    
    # what are the conditions?
    nd <- crossing(
        distinct(df2, condition),
        data.frame(ID = as.numeric(201:(201 + n_ppts - 1) ) )
        )
    
    # simulating data
    bf_simulated_data <- posterior_predict(
        varying_effects,
        newdata = nd,
        allow_new_levels = TRUE,
        sample_new_levels = "uncertainty", # "gaussian"
        summary = FALSE,
        nsamples = 1
        )[1, , ] %>%
        data.frame() %>%
        bind_cols(nd) %>%
        mutate(condition = factor(condition, labels = unique(nd$condition) ) ) %>%
        pivot_longer(cols = FRO:OOI, names_to = "muscle") %>%
        mutate(muscle = factor(muscle) )
    
    # reshaping the simulated data
    bf_simulated_data_wide <- bf_simulated_data %>%
        pivot_wider(names_from = muscle, values_from = value) %>%
        mutate(ID = as.numeric(ID), condition = as.character(condition) )
    
    bf_fro <- ttestBF(
        x = bf_simulated_data_wide$FRO[bf_simulated_data_wide$condition == "RUM"],
        y = bf_simulated_data_wide$FRO[bf_simulated_data_wide$condition == "DIS"],
        paired = TRUE, rscale = "medium"
    ) %>% data.frame %>% pull(bf)
    
    bf_ooi <- ttestBF(
        x = bf_simulated_data_wide$OOI[bf_simulated_data_wide$condition == "RUM"],
        y = bf_simulated_data_wide$OOI[bf_simulated_data_wide$condition == "DIS"],
        paired = TRUE, rscale = "medium"
    ) %>% data.frame %>% pull(bf)
    
    bf_oos <- ttestBF(
        x = bf_simulated_data_wide$OOS[bf_simulated_data_wide$condition == "RUM"],
        y = bf_simulated_data_wide$OOS[bf_simulated_data_wide$condition == "DIS"],
        paired = TRUE, rscale = "medium"
    ) %>% data.frame %>% pull(bf)
    
    return(c(bf_fro, bf_ooi, bf_oos) )
    
}

# plotting the cauchy prior
# x <- seq(-5, 5, by = 0.01)
# plot(x = x, dcauchy(x, location = 0, scale = sqrt(2) / 2), col = "red", type = "l")
# lines(x = x, dcauchy(x, location = 0, scale = 1), col = "steelblue", type = "l")
# lines(x = x, dcauchy(x, location = 0, scale = sqrt(2)), col = "darkgreen", type = "l")

# number of simulations
nsims <- 1e3

# defining the range of sample sizes
sample_size <- seq.int(from = 20, to = 200, by = 10)
sample_size <- rep(sample_size, each = nsims)

# initialising results
overall_results <- data.frame(
    nobs = sample_size,
    nsim = rep(1:nsims, length(sample_size) / nsims),
    bf_fro = numeric(length = length(sample_size) ),
    bf_ooi = numeric(length = length(sample_size) ),
    bf_oos = numeric(length = length(sample_size) )
    )

# looping over these sample sizes
for (i in 1:nrow(overall_results) ) {
    
    if (i == 1) {
        
        # time of stimulation start
        start_time <- Sys.time()
        
    }
    
    # prints progression
    print(
        paste(
            "Current sample size is", overall_results[i, ]$nobs,
            "- Simulation number", overall_results[i, ]$nsim
        )
    )
    
    # gets BFs for this sample size and stores it in "overall_results"
    overall_results[i, 3:5] <- simulating_bfs(n_obs = overall_results[i, ]$nobs)
    
    if (i == nrow(overall_results) ) {
        
        # time of simulation end
        stop_time <- Sys.time()
        
        # print start and end times
        print(paste("Start of simulation:", start_time) )
        print(paste("End of simulation:", stop_time) )
        
        # print time difference
        print(stop_time - start_time)
        
    }
    
}

# saving the results
save(overall_results, file = "results/overall_results.Rda")

# plotting the results
overall_results %>%
    na.omit() %>%
    filter_all(any_vars(. != 0) ) %>%
    pivot_longer(cols = bf_fro:bf_oos, names_to = "bf_type") %>%
    mutate(value = log(value) ) %>%
    mutate(bf_type = factor(bf_type, labels = c("FRO", "OOI", "OOS") ) ) %>%
    group_by(nobs, bf_type) %>%
    summarise(
        across(
            .cols = value,
            .fns = list(mean = mean, median = median, se = ~sd(.x) / sqrt(nsims), mad = mad)
            )
        ) %>%
    ungroup() %>%
    ggplot(aes(x = nobs, y = value_mean, colour = bf_type, fill = bf_type) ) +
    # ggplot(aes(x = nobs, y = value_median, colour = bf_type, fill = bf_type) ) +
    geom_hline(yintercept = 0, lty = 3) +
    geom_ribbon(
        aes(x = nobs, ymin = value_mean - 1.96 * value_se, ymax = value_mean + 1.96 * value_se, colour = NULL),
        # aes(x = nobs, ymin = value_median - value_mad, ymax = value_median + value_mad, colour = NULL),
        alpha = 0.5, show.legend = FALSE
        ) + 
    geom_line(show.legend = FALSE) +
    # geom_line(aes(y = value_median), show.legend = FALSE) +
    geom_point(show.legend = FALSE) +
    facet_wrap(~bf_type, scales = "free") +
    scale_x_continuous(breaks = unique(sample_size) ) +
    theme_bw(base_size = 12) +
    labs(x = "Number of participants", y = "Natural logarithm of the Bayes factor") +
    scale_colour_brewer(palette = "Dark2", direction = 1) +
    scale_fill_brewer(palette = "Dark2", direction = 1)
