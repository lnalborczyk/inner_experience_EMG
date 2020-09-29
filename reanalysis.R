#######################################################
# Reanalysing the data from Moffatt et al. (2020)     #
# --------------------------------------------------- #
# Ladislas Nalborczyk                                 #
# Last updated on 29.09.2020                          #
# https://github.com/lnalborczyk/inner_experience_EMG #
#######################################################

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

# plotting the baseline-normalised EMG-aplitude
df2 %>%
    pivot_longer(cols = FRO:OOI, names_to = "muscle") %>%
    pivot_wider(names_from = condition, values_from = value) %>%
    mutate(DIS_bas = DIS - BAS, RUM_bas = RUM - BAS) %>%
    select(ID, order, muscle, DIS_bas, RUM_bas) %>%
    pivot_longer(cols = DIS_bas:RUM_bas, names_to = "condition") %>%
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
    # splitting the by-group mean
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
        title = paste0("General plot for (mean and 95% CI)")
    ) +
    # forces lower xlim to 0
    # coord_cartesian(xlim = c(0, 1) ) +
    theme_bw(base_size = 12) +
    scale_colour_brewer(palette = "Dark2", direction = 1) +
    scale_fill_brewer(palette = "Dark2", direction = 1)

##############################
# Bayesian multilevel models #
##############################

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

# priors for the varying-effects model
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
# waic(constant_effects, varying_effects)

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

# baseline-normalised EMG amplitude
df3 <- df2 %>%
    pivot_longer(cols = FRO:OOI, names_to = "muscle") %>%
    pivot_wider(names_from = condition, values_from = value) %>%
    mutate(DIS_bas = DIS - BAS, RUM_bas = RUM - BAS) %>%
    select(ID, order, muscle, DIS_bas, RUM_bas) %>%
    pivot_longer(cols = DIS_bas:RUM_bas, names_to = "condition") %>%
    mutate(condition_contrast = ifelse(condition == "RUM_bas", 0.5, -0.5) ) %>%
    pivot_wider(names_from = muscle, values_from = value)

# priors for the varying-effects model
priors_varying_baseline <- c(
    # prior(normal(0, 10), class = Intercept, resp = "FRO"),
    # prior(normal(0, 1), class = b, resp = "FRO"),
    # prior(exponential(1), class = sigma, resp = "FRO"),
    # prior(exponential(1), class = sd, resp = "FRO"),
    # prior(normal(0, 10), class = Intercept, resp = "OOS"),
    # prior(normal(0, 1), class = b, resp = "OOS"),
    # prior(exponential(1), class = sigma, resp = "OOS"),
    # prior(exponential(1), class = sd, resp = "OOS"),
    prior(normal(0, 1), class = Intercept),
    prior(normal(0, 1), class = b),
    prior(exponential(1), class = sigma),
    prior(exponential(1), class = sd)
    # prior(lkj(2), class = cor)
    )

# fitting the model
varying_effects_baseline <- brm(
    OOI ~ 1 + condition_contrast + (1 | ID),
    family = gaussian(),
    prior = priors_varying_baseline,
    data = df3,
    chains = 4, cores = detectCores(),
    warmup = 2000, iter = 5000,
    control = list(adapt_delta = 0.99),
    sample_prior = TRUE,
    file = here("models/varying_effects_baseline_OOI")
    )

########################################################################################################
# simulating data from the posterior
# (for new, that is, non-observed, participants from the same population)
# https://bookdown.org/content/4857/models-with-memory.html#multilevel-posterior-predictions
# https://bookdown.org/content/4857/models-with-memory.html#posterior-prediction-for-new-clusters
###################################################################################################

# extracting posterior samples from varying-effects model
post <- posterior_samples(varying_effects)

# number of simulated participants
n_ppts <- 100

# for the 3 initials conditions and 200 new participants
# nd <- distinct(df2, condition) %>% arrange(condition)
# nd <- crossing(
#     distinct(df3, condition_contrast),
#     data.frame(ID = as.numeric(201:(201 + n_ppts - 1) ) )
#     )

nd <- crossing(
    distinct(df2, condition),
    data.frame(ID = as.numeric(201:(201 + n_ppts - 1) ) )
    )

# simulating data
# simulated_data <- fitted(
#     varying_effects,
#     newdata = nd,
#     # probs = c(.1, .9),
#     allow_new_levels = TRUE,
#     sample_new_levels = "gaussian",
#     summary = FALSE,
#     # nsamples = n_ppts
#     nsamples = 1
#     ) %>% 
#     data.frame() %>%
#     # set_names(1:4) %>%
#     # mutate(iter = 1:n_ppts) %>% 
#     # pivot_longer(-iter) %>%
#     pivot_longer(cols = 1:ncol(.) ) %>%
#     # mutate(ID = rep(101:200, 3) ) %>% head
#     separate(col = name, into = c("condition", "muscle"), sep = "\\.") %>%
#     mutate(condition = factor(condition, labels = nd$condition) ) %>%
#     mutate(muscle = factor(muscle) )

# simulating data
simulated_data <- posterior_predict(
    varying_effects,
    newdata = nd,
    # probs = c(.1, .9),
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian",
    summary = FALSE,
    # nsamples = n_ppts
    nsamples = 1
    )[1, , ] %>%
    data.frame() %>%
    bind_cols(nd) %>%
    # set_names(1:4) %>%
    # mutate(iter = 1:n_ppts) %>%
    # pivot_longer(-iter) %>%
    # mutate(ID = rep(101:200, 3) ) %>% head
    # separate(col = name, into = c("condition", "muscle"), sep = "\\.") %>%
    mutate(condition = factor(condition, labels = unique(nd$condition) ) ) %>%
    pivot_longer(cols = FRO:OOI, names_to = "muscle") %>%
    mutate(muscle = factor(muscle) )

# simulated_data <- posterior_predict(
#     varying_effects_baseline,
#     newdata = nd,
#     # probs = c(.1, .9),
#     allow_new_levels = TRUE,
#     sample_new_levels = "gaussian",
#     summary = FALSE,
#     nsamples = 1
#     )[1, ] %>% 
#     data.frame() %>%
#     bind_cols(nd) %>%
#     rename(OOI = ".") %>%
#     mutate(OOI = as.numeric(OOI), ID = as.factor(ID) ) %>%
#     ungroup

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
        # aes(x = condition, y = value),
        aes(group = muscle),
        position = pd,
        show.legend = FALSE
        # inherit.aes = FALSE
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

# reshaping the simulated data
simulated_data_wide <- simulated_data %>%
    pivot_wider(names_from = condition_contrast, values_from = OOI) %>%
    select(ID, distraction = "-0.5", rumination = "0.5") %>%
    mutate(difference = rumination - distraction)

library(BayesFactor)
ttestBF(x = simulated_data_wide$difference)

######################################################################
# fitting a model on these data
############################################################

# reshaping the simulated data
simulated_data_wide <- simulated_data %>%
    pivot_wider(names_from = muscle, values_from = value) %>%
    # rename(ID = iter)
    mutate(ID = as.numeric(ID), condition = as.character(condition) )

# priors for the constant-effects model
priors_simulated_data <- c(
    prior(normal(0, 10), class = Intercept, resp = "FRO"),
    prior(normal(0, 1), class = b, resp = "FRO"),
    prior(exponential(1), class = sigma, resp = "FRO"),
    prior(exponential(1), class = sd, resp = "FRO"),
    prior(normal(0, 10), class = Intercept, resp = "OOS"),
    prior(normal(0, 1), class = b, resp = "OOS"),
    prior(exponential(1), class = sigma, resp = "OOS"),
    prior(exponential(1), class = sd, resp = "OOS"),
    prior(normal(0, 1), class = Intercept),
    prior(normal(0, 1), class = b),
    prior(exponential(1), class = sigma),
    prior(exponential(1), class = sd)
    )

# fitting the model
# simulated_data_varying_effects <- brm(
#     mvbind(FRO, OOS, OOI) ~ 1 + condition + (1 | ID),
#     family = gaussian(),
#     prior = priors_varying,
#     # prior = priors_simulated_data,
#     data = simulated_data_wide,
#     chains = 4,
#     cores = parallel::detectCores(),
#     warmup = 2000, iter = 5000,
#     control = list(adapt_delta = 0.99),
#     sample_prior = TRUE,
#     file = here("models/simulated_data_varying_effects")
#     )

simulated_data_varying_effects <- update(
    varying_effects,
    # mvbind(FRO, OOS, OOI) ~ 1 + condition + (1 | ID),
    # family = gaussian(),
    # prior = priors_varying,
    # prior = priors_simulated_data,
    newdata = simulated_data_wide,
    chains = 4,
    cores = parallel::detectCores(),
    warmup = 2000, iter = 5000,
    control = list(adapt_delta = 0.99),
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

##########################################
# Simulating effect of sample size on BF #
##########################################

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
    
    # fitting the model
    # bf_simulated_data_varying_effects <- update(
    #     varying_effects,
    #     newdata = bf_simulated_data_wide,
    #     # mvbind(FRO, OOS, OOI) ~ 1 + condition + (1 | ID),
    #     # family = gaussian(),
    #     # prior = priors_varying,
    #     # data = simulated_data_wide,
    #     chains = 4, cores = detectCores(),
    #     warmup = 2000, iter = 5000,
    #     control = list(adapt_delta = 0.95),
    #     sample_prior = TRUE
    #     )
    
    # bf_hyp_fro <- hypothesis(
    #     bf_simulated_data_varying_effects,
    #     "FRO_conditionRUM - FRO_conditionDIS = 0"
    #     )
    # 
    # bf_fro <- 1 / bf_hyp_fro$hypothesis$Evid.Ratio
    # 
    # bf_hyp_ooi <- hypothesis(
    #     bf_simulated_data_varying_effects,
    #     "OOI_conditionRUM - OOI_conditionDIS = 0"
    #     )
    # 
    # bf_ooi <- 1 / bf_hyp_ooi$hypothesis$Evid.Ratio
    # 
    # bf_hyp_oos <- hypothesis(
    #     bf_simulated_data_varying_effects,
    #     "OOS_conditionRUM - OOS_conditionDIS = 0"
    #     )
    # 
    # bf_oos <- 1 / bf_hyp_oos$hypothesis$Evid.Ratio
    
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
    
    # results <- data.frame(
    #     n_obs = n_obs,
    #     bf_fro = bf_fro,
    #     bf_ooi = bf_ooi,
    #     bf_oos = bf_oos
    #     )
    
    return(c(bf_fro, bf_ooi, bf_oos) )
    
    }

# plotting the cauchy prior
# x <- seq(-5, 5, by = 0.01)
# plot(x = x, dcauchy(x, location = 0, scale = sqrt(2) / 2), col = "red", type = "l")
# lines(x = x, dcauchy(x, location = 0, scale = 1), col = "steelblue", type = "l")
# lines(x = x, dcauchy(x, location = 0, scale = sqrt(2)), col = "darkgreen", type = "l")

# simulating for some range of sample sizes
nsims <- 1e3
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
    labs(
        x = "Number of participants", y = "Natural logarithm of the Bayes factor",
        title = "Mean +/- SE (computed over 100 simulations)"
        )

# plotting the cauchy prior
x <- seq(-5, 5, by = 0.01)
plot(x = x, dcauchy(x, location = 0, scale = sqrt(2) / 2), col = "red", type = "l")
lines(x = x, dcauchy(x, location = 0, scale = 1), col = "steelblue", type = "l")
lines(x = x, dcauchy(x, location = 0, scale = sqrt(2)), col = "darkgreen", type = "l")

##################
# Does everyone? #
##################

# does everyone plot
df2 %>%
    pivot_longer(cols = FRO:OOI, names_to = "muscle") %>%
    pivot_wider(names_from = condition, values_from = value) %>%
    mutate(ruminator = ifelse(RUM > DIS, 1, 0) ) %>%
    mutate(ruminator = as.factor(ruminator) ) %>%
    # mutate(rum_dis_diff = RUM - DIS) %>%
    pivot_longer(cols = RUM:BAS, names_to = "condition") %>%
    ggplot(aes(x = condition, y = value, colour = ruminator, fill = ruminator) ) +
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
    # geom_quasirandom(alpha = 0.3) +
    geom_point(size = 2, alpha = 0.8, show.legend = FALSE) +
    geom_line(aes(group = ID), size = 1, alpha = 0.8, show.legend = FALSE) +
    # plitting the by-group mean
    # stat_summary(
    #     fun.y = mean,
    #     geom = "line", size = 2,
    #     aes(group = interaction(muscle, ruminator) ),
    #     position = pd,
    #     show.legend = FALSE
    #     ) +
    # plotting means
    # stat_summary(
    #     fun.y = "mean",
    #     geom = "point", shape = 16, size = 4,
    #     position = pd,
    #     show.legend = TRUE
    #     ) +
    # plotting confidence intervals
    # stat_summary(
    #     fun.data = mean_cl_normal,
    #     geom = "errorbar", size = 2, width = 0,
    #     fun.args = list(mult = 1.96),
    #     show.legend = FALSE,
    #     position = pd
    #     ) +
    # facetting by muscle
    facet_wrap(~muscle) +
    # axis labels
    labs(
        x = "Condition", y = "EMG log-amplitude",
        title = paste0("General plot for (mean and 95% CI)")
        ) +
    # forces lower xlim to 0
    # coord_cartesian(xlim = c(0, 1) ) +
    theme_bw(base_size = 12) +
    scale_colour_brewer(palette = "Dark2", direction = -1) +
    scale_fill_brewer(palette = "Dark2", direction = -1)

# priors for the unconstrained model
priors_unconstrained_model <- c(
    prior(normal(0, 1), class = Intercept),
    prior(normal(0, 1), class = b),
    prior(exponential(1), class = sigma)
    )

# fitting the model
unconstrained_model <- brm(
    OOI ~ 1 + condition,
    family = gaussian(),
    prior = priors_unconstrained_model,
    data = df2,
    chains = 4, cores = detectCores(),
    warmup = 2000, iter = 5000,
    control = list(adapt_delta = 0.95),
    sample_prior = TRUE,
    file = here("models/unconstrained_model")
    )

# priors for the positive-effects model
priors_positive_effects_model <- c(
    prior(normal(0, 1), class = Intercept),
    # with a lower bound on 0
    prior(normal(0, 1), class = b, lb = 0),
    prior(exponential(1), class = sigma)
    )

# fitting the model
positive_effects_model <- brm(
    OOI ~ 1 + condition,
    family = gaussian(),
    prior = priors_positive_effects_model,
    data = df2,
    chains = 4, cores = detectCores(),
    warmup = 2000, iter = 5000,
    control = list(adapt_delta = 0.95),
    sample_prior = TRUE,
    file = here("models/positive_effects_model")
    )

# priors for the common-effect model
priors_common_effect_model <- c(
    prior(normal(0, 1), class = Intercept),
    # with a lower bound on 0
    prior(normal(0, 1), class = b, lb = 0),
    prior(exponential(1), class = sigma)
    )

# fitting the model
common_effect_model  <- brm(
    OOI ~ 1 + condition,
    family = gaussian(),
    prior = priors_common_effect_model,
    data = df2,
    chains = 4, cores = detectCores(),
    warmup = 2000, iter = 5000,
    control = list(adapt_delta = 0.95),
    sample_prior = TRUE,
    file = here("models/common_effect_model")
    )

# priors for the common-effect model
priors_null_model <- c(
    prior(normal(0, 1), class = Intercept),
    # with a lower bound on 0
    prior(normal(0, 1), class = b, lb = 0),
    prior(exponential(1), class = sigma)
    )

# fitting the model
null_model  <- brm(
    OOI ~ 1 + condition,
    family = gaussian(),
    prior = priors_null_model,
    data = df2,
    chains = 4, cores = detectCores(),
    warmup = 2000, iter = 5000,
    control = list(adapt_delta = 0.95),
    sample_prior = TRUE,
    file = here("models/null_model")
    )
