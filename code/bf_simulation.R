########################################################
# Re-analysing the data from Moffatt et al. (2020)     #
# Simulating the effect of sample size on BFs          #
# ---------------------------------------------------- #
# Ladislas Nalborczyk                                  #
# Last updated on 02.10.2020                           #
# https://github.com/lnalborczyk/inner_experience_EMG  #
########################################################

# function for simulating BFs with varying sample size
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
        sample_new_levels = "gaussian",
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
    
    # computing the BF10 for the FRO muscle
    bf_fro <- ttestBF(
        x = bf_simulated_data_wide$FRO[bf_simulated_data_wide$condition == "RUM"],
        y = bf_simulated_data_wide$FRO[bf_simulated_data_wide$condition == "DIS"],
        paired = TRUE, rscale = "medium"
        ) %>% data.frame %>% pull(bf)
    
    # computing the BF10 for the OOI muscle
    bf_ooi <- ttestBF(
        x = bf_simulated_data_wide$OOI[bf_simulated_data_wide$condition == "RUM"],
        y = bf_simulated_data_wide$OOI[bf_simulated_data_wide$condition == "DIS"],
        paired = TRUE, rscale = "medium"
        ) %>% data.frame %>% pull(bf)
    
    # computing the BF10 for the OOS muscle
    bf_oos <- ttestBF(
        x = bf_simulated_data_wide$OOS[bf_simulated_data_wide$condition == "RUM"],
        y = bf_simulated_data_wide$OOS[bf_simulated_data_wide$condition == "DIS"],
        paired = TRUE, rscale = "medium"
        ) %>% data.frame %>% pull(bf)
    
    # returning the results
    return(c(bf_fro, bf_ooi, bf_oos) )
    
}

# number of simulations
nsims <- 1e3

# defining the range of sample sizes
sample_size <- seq.int(from = 20, to = 200, by = 10)
sample_size <- rep(sample_size, each = nsims)

# initialising an empty dataframe to store the results
overall_results <- data.frame(
    nobs = sample_size,
    nsim = rep(1:nsims, length(sample_size) / nsims),
    bf_fro = numeric(length = length(sample_size) ),
    bf_ooi = numeric(length = length(sample_size) ),
    bf_oos = numeric(length = length(sample_size) )
    )

# looping over the sample sizes
for (i in 1:nrow(overall_results) ) {
    
    if (i == 1) {
        
        # when did the simulation start?
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
        
        # when did the simulation end?
        stop_time <- Sys.time()
        
        # prints start and end times
        print(paste("Start of simulation:", start_time) )
        print(paste("End of simulation:", stop_time) )
        
        # prints time difference
        print(stop_time - start_time)
        
    }
    
}

# saving the results
save(overall_results, file = "results/overall_results.Rda")
