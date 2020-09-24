library(ggbeeswarm)
library(tidyverse)
library(patchwork)
library(magrittr)
library(parallel)
library(readxl)
library(papaja)
library(dplyr)
library(knitr)
library(BEST)
library(here)
library(glue)
library(brms)

# imports data
df <- read_excel("data/RUM_master_data.xlsx")

# position for groups
pd <- position_dodge(0.8)

# plotting for order 1 (BAS>DIS>RUM)
p1 <- df %>%
    select(ID, order = Order, RUM_FRO_MAX_ln:BAS_OOI_MAX_ln) %>%
    na.omit %>%
    # reshapes data from wide to long format
    pivot_longer(
        cols = starts_with(c("RUM", "DIS", "BAS") ),
        names_to = "condition",
        # names_prefix = "wk",
        values_to = "value",
        values_drop_na = TRUE
        ) %>%
    separate(
        col = condition,
        into = c("condition", "muscle"),
        sep = "_", extra = "merge"
        ) %>%
    filter(order == 1) %>%
    mutate(order = as.factor(order) ) %>%
    # plots the data
    ggplot(aes(x = condition, y = value) ) +
    # violin plots
    geom_violin(
        scale = "count",
        fill = "black",
        alpha = 0.2,
        colour = "white",
        position = pd,
        # adjust = 0.8,
        # draw_quantiles = 0.5,
        show.legend = FALSE
        ) +
    # plotting individual data points
    geom_dotplot(
        stackdir = "center",
        binaxis = "y",
        position = pd,
        # binwidth = 1,
        dotsize = 1,
        alpha = 0.3
        ) +
    stat_summary(
        fun.y = mean,
        geom = "line", size = 1,
        aes(group = order),
        position = pd,
        show.legend = FALSE
        ) +
    # plotting means
    stat_summary(
        fun.y = "mean", geom = "point", shape = 16, size = 3,
        position = pd,
        show.legend = TRUE
        ) +
    # plotting confidence intervals
    stat_summary(
        fun.data = mean_cl_normal,
        geom = "errorbar", size = 1, width = 0,
        fun.args = list(mult = 1.96),
        show.legend = FALSE,
        position = pd
        ) +
    # geom_point(alpha = 0.25, size = 0.5) + 
    # alxtheme +
    facet_grid(~muscle) +
    # scale_color_manual (values = colors2) +
    # scale_fill_manual (values = colors2) +
    labs(
        x = "Condition", y = "EMG log-amplitude",
        title = "Order 1 (rumination > distraction)"
        ) +
    # theme(legend.title = element_blank() ) +
    # forces lower xlim to 0
    # coord_cartesian(xlim = c(0, 1) ) +
    theme_bw(base_size = 12) +
    scale_colour_brewer(palette = "Dark2", direction = 1) +
    scale_fill_brewer(palette = "Dark2", direction = 1)

# plotting for order 1 (BAS>DIS>RUM)
p2 <- df %>%
    select(ID, order = Order, RUM_FRO_MAX_ln:BAS_OOI_MAX_ln) %>%
    na.omit %>%
    # reshapes data from wide to long format
    pivot_longer(
        cols = starts_with(c("RUM", "DIS", "BAS") ),
        names_to = "condition",
        # names_prefix = "wk",
        values_to = "value",
        values_drop_na = TRUE
        ) %>%
    separate(
        col = condition,
        into = c("condition", "muscle"),
        sep = "_", extra = "merge"
        ) %>%
    filter(order == 2) %>%
    mutate(order = as.factor(order) ) %>%
    # plots the data
    ggplot(aes(x = condition, y = value) ) +
    # violin plots
    geom_violin(
        scale = "count",
        fill = "black",
        alpha = 0.2,
        colour = "white",
        position = pd,
        # adjust = 0.8,
        # draw_quantiles = 0.5,
        show.legend = FALSE
        ) +
    # plotting individual data points
    geom_dotplot(
        stackdir = "center",
        binaxis = "y",
        position = pd,
        # binwidth = 1,
        dotsize = 1,
        alpha = 0.3
        ) +
    stat_summary(
        fun.y = mean,
        geom = "line", size = 1,
        aes(group = order),
        position = pd,
        show.legend = FALSE
        ) +
    # plotting means
    stat_summary(
        fun.y = "mean", geom = "point", shape = 16, size = 3,
        position = pd,
        show.legend = TRUE
        ) +
    # plotting confidence intervals
    stat_summary(
        fun.data = mean_cl_normal,
        geom = "errorbar", size = 1, width = 0,
        fun.args = list(mult = 1.96),
        show.legend = FALSE,
        position = pd
        ) +
    # geom_point(alpha = 0.25, size = 0.5) + 
    # alxtheme +
    facet_grid(~muscle) +
    # scale_color_manual (values = colors2) +
    # scale_fill_manual (values = colors2) +
    labs(
        x = "Condition", y = "EMG log-amplitude",
        title = "Order 2 (distraction > rumination)"
        ) +
    # theme(legend.title = element_blank() ) +
    # forces lower xlim to 0
    # coord_cartesian(xlim = c(0, 1) ) +
    theme_bw(base_size = 12) +
    scale_colour_brewer(palette = "Dark2", direction = 1) +
    scale_fill_brewer(palette = "Dark2", direction = 1)

p1 / p2

# general plot

df %>%
    select(ID, order = Order, RUM_FRO_MAX_ln:BAS_OOI_MAX_ln) %>%
    na.omit %>%
    # reshapes data from wide to long format
    pivot_longer(
        cols = starts_with(c("RUM", "DIS", "BAS") ),
        names_to = "condition",
        # names_prefix = "wk",
        values_to = "value",
        values_drop_na = TRUE
        ) %>%
    separate(
        col = condition,
        into = c("condition", "muscle"),
        sep = "_", extra = "merge"
        ) %>%
    # filter(order == 2) %>%
    # mutate(order = as.factor(order) ) %>%
    # plots the data
    ggplot(aes(x = condition, y = value) ) +
    # violin plots
    geom_violin(
        scale = "count",
        fill = "black",
        alpha = 0.2,
        colour = "white",
        position = pd,
        # adjust = 0.8,
        # draw_quantiles = 0.5,
        show.legend = FALSE
        ) +
    # plotting individual data points
    geom_dotplot(
        stackdir = "center",
        binaxis = "y",
        position = pd,
        # binwidth = 1,
        dotsize = 0.5,
        alpha = 0.3
        ) +
    stat_summary(
        fun.y = mean,
        geom = "line", size = 1,
        aes(group = muscle),
        position = pd,
        show.legend = FALSE
        ) +
    # plotting means
    stat_summary(
        fun.y = "mean",
        geom = "point", shape = 16, size = 3,
        position = pd,
        show.legend = TRUE
        ) +
    # plotting confidence intervals
    stat_summary(
        fun.data = mean_cl_normal,
        geom = "errorbar", size = 1, width = 0,
        fun.args = list(mult = 1.96),
        show.legend = FALSE,
        position = pd
        ) +
    # geom_point(alpha = 0.25, size = 0.5) + 
    # alxtheme +
    facet_grid(~muscle) +
    # scale_color_manual (values = colors2) +
    # scale_fill_manual (values = colors2) +
    labs(
        x = "Condition", y = "EMG log-amplitude",
        title = "General plot"
        ) +
    # theme(legend.title = element_blank() ) +
    # forces lower xlim to 0
    # coord_cartesian(xlim = c(0, 1) ) +
    theme_bw(base_size = 12) +
    scale_colour_brewer(palette = "Dark2", direction = 1) +
    scale_fill_brewer(palette = "Dark2", direction = 1)

############################################################
# analysis
# compute bf for interaction
# with and without considering order
# univariate vs. multivariate
# baseline standardisation or not
######################################################

# NB: 26 participants in the final dataset...

df2 <- df %>%
    select(ID, order = Order, RUM_FRO_MAX_ln:BAS_OOI_MAX_ln) %>%
    na.omit %>%
    # reshapes data from wide to long format
    pivot_longer(
        cols = starts_with(c("RUM", "DIS", "BAS") ),
        names_to = "condition",
        # names_prefix = "wk",
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

#############################################
# Bayesian multilevel models
########################################

# priors for the constant-effects model
priors_constant <- c(
    # prior(normal(0, 10), class = b, coef = Intercept, resp = "FRO"),
    prior(normal(0, 1), class = b, resp = "FRO"),
    prior(exponential(1), class = sigma, resp = "FRO"),
    # prior(normal(0, 10), class = b, coef = Intercept, resp = "OOS"),
    prior(normal(0, 1), class = b, resp = "OOS"),
    prior(exponential(1), class = sigma, resp = "OOS"),
    # prior(normal(0, 10), class = b, coef = Intercept, resp = "OOI"),
    prior(normal(0, 1), class = b, resp = "OOI"),
    prior(exponential(1), class = sigma, resp = "OOI")
    )

# fitting the model
constant_effects <- brm(
    mvbind(FRO, OOS, OOI) ~ 0 + condition,
    family = gaussian(),
    prior = priors_constant,
    data = df2,
    chains = 4, cores = detectCores(),
    warmup = 2000, iter = 5000,
    control = list(adapt_delta = 0.95),
    sample_prior = TRUE,
    file = here("models/constant_effects")
    )

# BF testing
hypothesis(
    constant_effects,
    "FRO_conditionDIS = FRO_conditionRUM"
    )

hypothesis(
    constant_effects,
    "OOS_conditionDIS = OOS_conditionRUM"
    )

hypothesis(
    constant_effects,
    "OOI_conditionDIS = OOI_conditionRUM"
    )

# extracts posterior samples
post <- posterior_samples(constant_effects)

# plotting contrasts for each muscle
plotPost(
    post$b_FRO_conditionRUM - post$b_FRO_conditionDIS,
    compVal = 0, ROPE = c(-0.1, 0.1)
    )

plotPost(
    post$b_OOI_conditionRUM - post$b_OOI_conditionDIS,
    compVal = 0, ROPE = c(-0.1, 0.1)
    )

plotPost(
    post$b_OOS_conditionRUM - post$b_OOS_conditionDIS,
    compVal = 0, ROPE = c(-0.1, 0.1)
    )

# priors for the constant-effects model
priors_varying <- c(
    prior(normal(0, 10), class = Intercept, resp = "FRO"),
    prior(normal(0, 1), class = b, resp = "FRO"),
    prior(exponential(1), class = sigma, resp = "FRO"),
    prior(exponential(1), class = sd, resp = "FRO"),
    prior(normal(0, 10), class = Intercept, resp = "OOS"),
    prior(normal(0, 1), class = b, resp = "OOS"),
    prior(exponential(1), class = sigma, resp = "OOS"),
    prior(exponential(1), class = sd, resp = "OOS"),
    prior(normal(0, 10), class = Intercept, resp = "OOI"),
    prior(normal(0, 1), class = b, resp = "OOI"),
    prior(exponential(1), class = sigma, resp = "OOI"),
    prior(exponential(1), class = sd, resp = "OOI")
    )

# fitting the model
varying_effects <- brm(
    mvbind(FRO, OOS, OOI) ~ 1 + condition + (1 | ID),
    family = gaussian(),
    prior = priors_varying,
    data = df2,
    chains = 4, cores = detectCores(),
    warmup = 2000, iter = 5000,
    control = list(adapt_delta = 0.95),
    sample_prior = TRUE,
    file = here("models/varying_effects")
    )

# comparing the models
waic(constant_effects, varying_effects)

# checking assumptions
plot(density(residuals(constant_effects) ) )
plot(density(residuals(varying_effects) ) )

# posterior predictive checking
pp_check(constant_effects, resp = "FRO")
pp_check(constant_effects, resp = "OOS")
pp_check(constant_effects, resp = "OOI")
pp_check(varying_effects, resp = "FRO")
pp_check(varying_effects, resp = "OOS")
pp_check(varying_effects, resp = "OOI")

########################################################################################################
# simulating data from the posterior
# (for new, that is, non-observed, participants from the same population)
# https://bookdown.org/content/4857/models-with-memory.html#multilevel-posterior-predictions
################################################################################################

# extracting posterior samples from varying-effects model
post <- posterior_samples(varying_effects)

# number of simulated participants
n_ppts <- 200

# what are the conditions?
nd <- distinct(df2, condition) %>% arrange(condition)

# simulating data
simulated_data <- fitted(
    varying_effects,
    newdata = nd,
    # probs = c(.1, .9),
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian",
    summary = FALSE,
    nsamples = n_ppts
    ) %>% 
    data.frame() %>% 
    # set_names(1:4) %>% 
    mutate(iter = 1:n_ppts) %>% 
    pivot_longer(-iter) %>%
    separate(col = name, into = c("condition", "muscle"), sep = "\\.") %>%
    mutate(condition = factor(condition, labels = nd$condition) ) %>%
    mutate(muscle = factor(muscle) )

# plotting these simulated data

simulated_data %>%
    ggplot(aes(x = condition, y = value) ) +
    # violin plots
    geom_violin(
        scale = "count",
        fill = "black",
        alpha = 0.2,
        colour = "white",
        position = pd,
        # adjust = 0.8,
        # draw_quantiles = 0.5,
        show.legend = FALSE
        ) +
    # plotting individual data points
    geom_quasirandom(alpha = 0.3) +
    # plitting the by-group mean
    stat_summary(
        fun.y = mean,
        geom = "line", size = 1,
        aes(group = muscle),
        position = pd,
        show.legend = FALSE
        ) +
    # plotting means
    stat_summary(
        fun.y = "mean",
        geom = "point", shape = 16, size = 3,
        position = pd,
        show.legend = TRUE
        ) +
    # plotting confidence intervals
    stat_summary(
        fun.data = mean_cl_normal,
        geom = "errorbar", size = 1, width = 0,
        fun.args = list(mult = 1.96),
        show.legend = FALSE,
        position = pd
        ) +
    # facetting by muscle
    facet_wrap(~muscle) +
    # axis labels
    labs(
        x = "Condition", y = "EMG log-amplitude",
        title = paste0("General plot for ", n_ppts, " participants (mean and 95% CI)")
        ) +
    # forces lower xlim to 0
    # coord_cartesian(xlim = c(0, 1) ) +
    theme_bw(base_size = 12) +
    scale_colour_brewer(palette = "Dark2", direction = 1) +
    scale_fill_brewer(palette = "Dark2", direction = 1)

######################################################################
# fitting a model on these data
############################################################

# reshaping the simulated data
simulated_data_wide <- simulated_data%>%
    pivot_wider(names_from = muscle, values_from = value) %>%
    rename(ID = iter)

# fitting the model
simulated_data_varying_effects <- brm(
    mvbind(FRO, OOS, OOI) ~ 1 + condition + (1 | ID),
    family = gaussian(),
    prior = priors_varying,
    data = simulated_data_wide,
    chains = 4, cores = detectCores(),
    warmup = 2000, iter = 5000,
    control = list(adapt_delta = 0.95),
    sample_prior = TRUE,
    file = here("models/simulated_data_varying_effects")
    )

# model summary
summary(simulated_data_varying_effects)
posterior_summary(varying_effects, pars = c("^b", "^sd") )
posterior_summary(simulated_data_varying_effects, pars = c("^b", "^sd") )

# BF testing
hyp_fro <- hypothesis(
    simulated_data_varying_effects,
    "FRO_conditionDIS = FRO_conditionRUM"
    )

plot(hyp_fro, plot = FALSE, theme = theme_bw(base_size = 12) )[[1]] +
    geom_vline(xintercept = 0, linetype = 2) +
    coord_cartesian(xlim = c(-2, 2) )

hyp_oos <- hypothesis(
    simulated_data_varying_effects,
    "OOS_conditionDIS = OOS_conditionRUM"
    )

plot(hyp_oos, plot = FALSE, theme = theme_bw(base_size = 12) )[[1]] +
    geom_vline(xintercept = 0, linetype = 2) +
    coord_cartesian(xlim = c(-2, 2) )

hyp_ooi <- hypothesis(
    simulated_data_varying_effects,
    "OOI_conditionDIS = OOI_conditionRUM"
    )

plot(hyp_ooi, plot = FALSE, theme = theme_bw(base_size = 12) )[[1]] +
    geom_vline(xintercept = 0, linetype = 2) +
    coord_cartesian(xlim = c(-2, 2) )

# extracts posterior samples
post <- posterior_samples(simulated_data_varying_effects)

plotPost(
    post$b_FRO_conditionRUM - post$b_FRO_conditionDIS
    # compVal = 0, ROPE = c(-0.1, 0.1)
    )

plotPost(
    post$b_OOI_conditionRUM - post$b_OOI_conditionDIS,
    # compVal = 0, ROPE = c(-0.1, 0.1)
    )

plotPost(
    post$b_OOS_conditionRUM - post$b_OOS_conditionDIS,
    # compVal = 0, ROPE = c(-0.1, 0.1)
    )

###############################################################
# Simulating effect of sample size on BF
################################################

simulating_bfs <- function (n_obs) {
    
    # extracting posterior samples from varying-effects model
    post <- posterior_samples(varying_effects)
    
    # number of simulated participants
    n_ppts <- n_obs
    
    # what are the conditions?
    nd <- distinct(df2, condition) %>% arrange(condition)
    
    # simulating data
    bf_simulated_data <- fitted(
        varying_effects,
        newdata = nd,
        # probs = c(.1, .9),
        allow_new_levels = TRUE,
        sample_new_levels = "gaussian",
        summary = FALSE,
        nsamples = n_ppts
        ) %>% 
        data.frame() %>% 
        mutate(iter = 1:n_ppts) %>% 
        pivot_longer(-iter) %>%
        separate(col = name, into = c("condition", "muscle"), sep = "\\.") %>%
        mutate(condition = factor(condition, labels = nd$condition) ) %>%
        mutate(muscle = factor(muscle) )
    
    # reshaping the simulated data
    bf_simulated_data_wide <- bf_simulated_data%>%
        pivot_wider(names_from = muscle, values_from = value) %>%
        rename(ID = iter)
    
    # fitting the model
    bf_simulated_data_varying_effects <- update(
        simulated_data_varying_effects,
        newdata = bf_simulated_data_wide,
        # mvbind(FRO, OOS, OOI) ~ 1 + condition + (1 | ID),
        # family = gaussian(),
        # prior = priors_varying,
        # data = simulated_data_wide,
        chains = 4, cores = detectCores(),
        warmup = 2000, iter = 5000,
        control = list(adapt_delta = 0.95),
        sample_prior = TRUE
        # file = here("models/simulated_data_varying_effects")
        )
    
    bf_hyp_ooi <- hypothesis(
        bf_simulated_data_varying_effects,
        "OOI_conditionRUM - OOI_conditionDIS = 0"
        )
    
    bf <- 1 / bf_hyp_ooi$hypothesis$Evid.Ratio # %>% as.numeric
    results <- data.frame(n_obs = n_obs, bf = bf)
    
    return(results)
    
    }

# simulating for some range of sample sizes
sample_size <- seq.int(from = 20, to = 200, by = 10)

# looping ove these sample sizes
for (i in 1:length(sample_size) ) {
    
    temp_results <- simulating_bfs(n_obs = sample_size[i])
    
    if (!exists("overall_results") ) {
        
        overall_results <- temp_results
        
    } else {
        
        overall_results <- rbind(overall_results, temp_results)
        
    }
    
    rm(temp_results)
    
}

# saving the results
save(overall_results, file = "results/overall_results.Rda")

# plotting the results
overall_results %>%
    ggplot(aes(x = n_obs, y = bf) ) +
    geom_line() +
    geom_point() +
    theme_bw(base_size = 12) +
    labs(x = "Number of participants", y = "Bayes factor")

###############################################################
# Does everyone?
################################################

# ... 
