# --------------------------
# Load Libraries
# --------------------------
library(Matrix)
library(lme4)
library(lmerTest)
library(dplyr)
library(broom.mixed)
library(ggplot2)
library(glmmTMB)
library(corrr)
library(MASS)
library(tidyr)
library(car)
library(modelsummary)
library(texreg)
library(LearnVizLMM)
library(Rcpp)
library(brms)
library(performance)
library(patchwork)
library(lattice)
library(sjPlot)
library(arm)
library(ggeffects)
library(forcats)
library(purrr)

#hist(model_data$prob_disagree)
summary(model_data$prob_disagree)
ggplot(model_data, aes(x=prob_disagree)) + 
  geom_histogram(binwidth=0.05, boundary = 0, fill="#FF9999", color="#e9ecef", alpha=0.9) +
  labs(title  = "Distribution of Predicted Probability of Disagreement",
       x = "Predicted Probability for Disagreement",
       y = "Frequency")


model_data <- read.csv("../../data/analysis_data/R_data/model_data.csv")


# ------------------------------------------------------------------------------
# H4: Higher submission and comment EMI --> lower disagreement
## coefficient of discussions EMI negative and significant
# ------------------------------------------------------------------------------

# duplicated submission and comment EMI --> control
model_data_h4_discussion <- model_data %>%
  group_by(submission_id, comment_id) %>%
  summarise(
    submission_EMI = first(submission_EMI),
    comment_EMI = first(comment_EMI),
    share_disagree = mean(disagreement_label, na.rm = TRUE),
    Mod_Dummy = first(Mod_Dummy),
    subreddit = first(subreddit),
    .groups = "drop"
  )

model_data_h4_discussion_sample <- model_data_h4_discussion %>%
  group_by(subreddit) %>%
  sample_frac(0.1) %>%
  ungroup()


# sample
model_h4_sample = lmer(share_disagree ~ submission_EMI + comment_EMI +
                         factor(Mod_Dummy) +
                         (1 + submission_EMI + comment_EMI | subreddit) +
                         (1 | submission_id), 
                       data = model_data_h4_discussion_sample)

summary(model_h4_sample)
VarCorr(model_h4_sample)
isSingular(model_h4_sample, tol = 1e-4) # TRUE: is singular


# reduced complexity
model_h4_sample_2 = lmer(share_disagree ~ submission_EMI + comment_EMI +
                           factor(Mod_Dummy) +
                           (1 | subreddit) +
                           (1 | submission_id), 
                         data = model_data_h4_discussion_sample)
summary(model_h4_sample_2)
anova(model_h4_sample, model_h4_sample_2)

VarCorr(model_h4_sample_2) 



# first model is singular, but a better fit
# means, one varinace/correlation parameter is estimated close to zero/zero
# too little data or no variation at group level

# fixed effects in v1 are not significant
# --> go with second, robust model

# submission higher negative coefficient, still only -0.02887









# ------------------------------------------------------------------------------
# H4.2: Effect especially prominent between ideological groups
## interaction of ideology with EMI coefficient?
# ------------------------------------------------------------------------------
# model with all subreddits and rand intercept per subreddit failed to converge
# causes: too many random effects: subreddit has random effect variance near zero -> drop


# -------------------------
# Stratify by Subreddit
# -------------------------

model_data_Ask_Politics <- model_data %>%
  dplyr:: filter(subreddit == "Ask_Politics") %>%
  mutate(
    ideology_group = factor(ideology_group, levels = c("PL & CL", "PR & CL", "PL & CR", "PR & CR")),
    Mod_Dummy = factor(Mod_Dummy, levels = c(0, 1))
  )

model_data_Askpolitics <- model_data %>%
  dplyr::filter(subreddit == "Askpolitics") %>%
  mutate(
    ideology_group = factor(ideology_group, levels = c("PL & CL", "PR & CL", "PL & CR", "PR & CR")),
    Mod_Dummy = factor(Mod_Dummy, levels = c(0, 1))
  )

model_data_PoliticalDebate <- model_data %>%
  dplyr::filter(subreddit == "PoliticalDebate") %>%
  mutate(
    ideology_group = factor(ideology_group, levels = c("PL & CL", "PR & CL", "PL & CR", "PR & CR")),
    Mod_Dummy = factor(Mod_Dummy, levels = c(0, 1))
  )

model_data_PoliticalDiscussion <- model_data %>%
  dplyr::filter(subreddit == "PoliticalDiscussion") %>%
  mutate(
    ideology_group = factor(ideology_group, levels = c("PL & CL", "PR & CL", "PL & CR", "PR & CR")),
    Mod_Dummy = factor(Mod_Dummy, levels = c(0, 1))
  )

model_data_politics <- model_data %>%
  dplyr::filter(subreddit == "politics") %>%
  mutate(
    ideology_group = factor(ideology_group, levels = c("PL & CL", "PR & CL", "PL & CR", "PR & CR")),
    Mod_Dummy = factor(Mod_Dummy, levels = c(0, 1))
  )


model_data_NeutralPolitics <- model_data %>%
  dplyr::filter(subreddit == "NeutralPolitics") %>%
  mutate(
    ideology_group = factor(ideology_group, levels = c("PL & CL", "PR & CL", "PL & CR", "PR & CR")),
    Mod_Dummy = factor(Mod_Dummy, levels = c(0, 1))
  )


# check correlation of submission and comment EMI --> if not too high, keep
# else, split into two models --> it's fine


# Define a function to fit the model to any given dataset
#--------------------------------------------------------
fit_model_h4 <- function(data) {
  lmer(
    prob_disagree ~ submission_EMI + comment_EMI * ideology_group + factor(Mod_Dummy) +
      (1 | submission_id) + (1 | comment_id),
    data = data,
    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
}

fit_model_h4_no_mod <- function(data) {
  lmer(
    prob_disagree ~ submission_EMI + comment_EMI * ideology_group  +
      (1 | submission_id) + (1 | comment_id),
    data = data,
    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
}

# Fit the models for each subreddit
model_h4_2_Ask_Politics          <- fit_model_h4(model_data_Ask_Politics)
model_h4_2_PoliticalDebate       <- fit_model_h4(model_data_PoliticalDebate)
model_h4_2_politics              <- fit_model_h4(model_data_politics)

summary(model_h4_2_Ask_Politics)
confint(model_h4_2_Ask_Politics, method = "Wald", level = 0.95)

summary(model_h4_2_PoliticalDebate)
confint(model_h4_2_PoliticalDebate, method = "Wald", level = 0.95)

summary(model_h4_2_politics)
confint(model_h4_2_politics, method = "Wald", level = 0.95)

# no mods
model_h4_2_Askpolitics           <- fit_model_h4_no_mod(model_data_Askpolitics)
model_h4_2_PoliticalDiscussion   <- fit_model_h4_no_mod(model_data_PoliticalDiscussion)
model_h4_2_NeutralPolitics       <- fit_model_h4_no_mod(model_data_NeutralPolitics)

summary(model_h4_2_Askpolitics)
confint(model_h4_2_Askpolitics, method = "Wald", level = 0.95)

summary(model_h4_2_PoliticalDiscussion)
confint(model_h4_2_PoliticalDiscussion, method = "Wald", level = 0.95)

summary(model_h4_2_NeutralPolitics)
confint(model_h4_2_NeutralPolitics, method = "Wald", level = 0.95)





# Function to sample and prepare predicted data
#----------------------------------------------
prepare_prediction_data <- function(model, data, predictor, n = 1000) {
  
  # Sample for plotting
  data_sample <- data %>% sample_n(min(n, nrow(data)))
  
  # Create grid for chosen predictor
  grid <- tibble(
    !!predictor := seq(
      min(data_sample[[predictor]], na.rm = TRUE),
      max(data_sample[[predictor]], na.rm = TRUE),
      length.out = 100
    )
  )
  
  # Add required covariates, using means or references
  if ("ideology_group" %in% names(data)) {
    grid$ideology_group <- factor("PL & CL", levels = c("PL & CL", "PR & CL", "PL & CR", "PR & CR"))
  }
  
  if (predictor != "submission_EMI" && "submission_EMI" %in% names(data)) {
    grid$submission_EMI <- mean(data$submission_EMI, na.rm = TRUE)
  }
  
  if (predictor != "comment_EMI" && "comment_EMI" %in% names(data)) {
    grid$comment_EMI <- mean(data$comment_EMI, na.rm = TRUE)
  }
  
  if ("Mod_Dummy" %in% names(data)) {
    grid$Mod_Dummy <- factor("0", levels = c("0", "1"))
  }
  
  # Add fake row with 2nd levels of factors to avoid contrast error
  grid_fake <- grid
  new_row <- grid[1, ]
  
  if ("ideology_group" %in% names(grid)) {
    new_row$ideology_group <- factor("PR & CL", levels = levels(grid$ideology_group))
  }
  
  if ("Mod_Dummy" %in% names(grid)) {
    new_row$Mod_Dummy <- factor("1", levels = levels(grid$Mod_Dummy))
  }
  
  grid_fake <- bind_rows(grid_fake, new_row)
  
  # Create design matrix with fixed effects only formula (drop response)
  formula_fixed <- formula(model, fixed.only = TRUE)[-2]
  
  X <- model.matrix(formula_fixed, data = grid_fake)
  
  # Remove fake row from model matrix and grid
  X <- X[-nrow(X), ]
  grid <- grid
  
  # Get fixed effects coefficients and variance-covariance matrix
  beta <- fixef(model)
  vcov_beta <- vcov(model)
  
  # Calculate predictions and SE
  grid$pred <- as.numeric(X %*% beta)
  pred_se <- sqrt(rowSums((X %*% vcov_beta) * X))
  
  grid <- grid %>%
    mutate(
      ci_low = pred - 1.96 * pred_se,
      ci_high = pred + 1.96 * pred_se
    )
  
  return(grid)
}



# Prepare list of models and corresponding data
models_list <- list(
  "Ask_Politics" = list(model = model_h4_2_Ask_Politics, data = model_data_Ask_Politics),
  "PoliticalDebate" = list(model = model_h4_2_PoliticalDebate, data = model_data_PoliticalDebate),
  "politics" = list(model = model_h4_2_politics, data = model_data_politics),
  "Askpolitics" = list(model = model_h4_2_Askpolitics, data = model_data_Askpolitics),
  "PoliticalDiscussion" = list(model = model_h4_2_PoliticalDiscussion, data = model_data_PoliticalDiscussion),
  "NeutralPolitics" = list(model = model_h4_2_NeutralPolitics, data = model_data_NeutralPolitics)
)


# Generate combined data for submission_EMI and comment_EMI
submission_plot_data <- map_dfr(names(models_list), function(name) {
  df <- prepare_prediction_data(models_list[[name]]$model, models_list[[name]]$data, "submission_EMI")
  df$subreddit <- name
  df$predictor <- "submission_EMI"
  df
})


comment_plot_data <- map_dfr(names(models_list), function(name) {
  df <- prepare_prediction_data(models_list[[name]]$model, models_list[[name]]$data, "comment_EMI")
  df$subreddit <- name
  df$predictor <- "comment_EMI"
  df
})

obs_data <- bind_rows(lapply(names(models_list), function(name) {
  df <- models_list[[name]]$data %>%
    dplyr::select(submission_EMI, prob_disagree, comment_EMI) %>%
    mutate(subreddit = name) %>%
    sample_n(min(10000, n()))
}))


# ---- Plotting ----
#----------------------------------------------

# Scatter + regression for submission_EMI
coef_labels_submission <- sapply(names(models_list), function(name) {
  model <- models_list[[name]]$model
  coef <- fixef(model)["submission_EMI"]
  label <- paste0(name, " (", round(coef, 3), ")")
  return(label)
})
names(coef_labels_submission) <- names(models_list)
submission_plot_data$subreddit <- coef_labels_submission[submission_plot_data$subreddit]
obs_data$subreddit_sub <- coef_labels_submission[obs_data$subreddit]

# Submission EMI Plot
p_submission <- ggplot() +
  geom_point(data = obs_data, aes(x = submission_EMI, y = prob_disagree, color = subreddit_sub), alpha = 0.1, size = 1.2) +
  geom_line(data = submission_plot_data, aes(x = submission_EMI, y = pred, color = subreddit), size = 1) +
  geom_ribbon(data = submission_plot_data, aes(x = submission_EMI, ymin = ci_low, ymax = ci_high, fill = subreddit), alpha = 0.2, color = NA) +
  theme_bw(base_size = 16) +
  labs(
    # title = "Predicted Disagreement by Submission EMI",
    x = "Submission EMI",
    y = "Probability of Disagreement",
    color = "Subreddit (Coef)",
    fill = "Subreddit (Coef)"
  ) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "right"
  )

# Scatter + regression for comment_EMI
coef_labels_comment <- sapply(names(models_list), function(name) {
  model <- models_list[[name]]$model
  coef <- fixef(model)["comment_EMI"]
  label <- paste0(name, " (", round(coef, 3), ")")
  return(label)
})
names(coef_labels_comment) <- names(models_list)
comment_plot_data$subreddit <- coef_labels_comment[comment_plot_data$subreddit]
obs_data$subreddit_com <- coef_labels_comment[obs_data$subreddit]

# Comment EMI Plot
p_comment <-  ggplot() +
  geom_point(data = obs_data, aes(x = comment_EMI, y = prob_disagree, color = subreddit_com), alpha = 0.1, size = 1.2) +
  geom_line(data = comment_plot_data, aes(x = comment_EMI, y = pred, color = subreddit), size = 1) +
  geom_ribbon(data = comment_plot_data, aes(x = comment_EMI, ymin = ci_low, ymax = ci_high, fill = subreddit), alpha = 0.2, color = NA) +
  theme_bw(base_size = 14) +
  labs(
    #title = "Predicted Disagreement by Comment EMI",
    x = "Comment EMI",
    y = "Probability of Disagreement",
    color = "Subreddit (Coef)",  # Legend title
    fill = "Subreddit (Coef)"
  ) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "right"
  )


# Save plots
ggsave("H4_plots_OLS/H4_bySubreddit_Submission_EMI_Disagreement.pdf", p_submission, width = 10, height = 6, dpi = 300)
ggsave("H4_plots_OLS/H4_bySubreddit_CommentEMI_Disagreement.pdf", p_comment, width = 10, height = 6, dpi = 300)

ggsave("H4_plots_OLS/H4_bySubreddit_Submission_EMI_Disagreement.png", p_submission, width = 10, height = 6, dpi = 300)
ggsave("H4_plots_OLS/H4_bySubreddit_CommentEMI_Disagreement.png", p_comment, width = 10, height = 6, dpi = 300)


# Optional: display side-by-side
library(patchwork)
combined_plot <- p_submission + p_comment + plot_layout(ncol = 1)
ggsave("H4_plots_OLS/H4_bySubreddit_Combined_Sub_vs_Comment.pdf", combined_plot, width = 14, height = 20, dpi = 300)



# ---------- Visualize Significant Interaction Terms --------------
#------------------------------------------------------------------

summary(model_h4_2_PoliticalDiscussion)

ideology_levels <- levels(model_data_PoliticalDiscussion$ideology_group)

grid_interact_PolDis <- expand.grid(
  comment_EMI = seq(min(model_data_PoliticalDiscussion$comment_EMI), max(model_data_PoliticalDiscussion$comment_EMI), length.out = 100),
  ideology_group = ideology_levels
)

# Set other variables to typical values
grid_interact_PolDis$submission_EMI <- mean(model_data_PoliticalDiscussion$submission_EMI, na.rm = TRUE)

X_PolDis <- model.matrix(~ submission_EMI + comment_EMI * ideology_group, data = grid_interact_PolDis)

beta_PolDis <- fixef(model_h4_2_PoliticalDiscussion)
vcov_beta_PolDis <- vcov(model_h4_2_PoliticalDiscussion)

grid_interact_PolDis$pred <- as.vector(X_PolDis %*% beta_PolDis)
se <- sqrt(rowSums((X_PolDis %*% vcov_beta_PolDis) * X_PolDis))

grid_interact_PolDis$ci_low <- grid_interact_PolDis$pred - 1.96 * se
grid_interact_PolDis$ci_high <- grid_interact_PolDis$pred + 1.96 * se


# Extract coefficients
coefs <- fixef(model_h4_2_PoliticalDiscussion)

# Get ideology group levels except the baseline
ideology_levels <- levels(model_data_PoliticalDiscussion$ideology_group)
ref_level <- ideology_levels[1]
other_levels <- ideology_levels[-1]

# Get slopes for each group
slopes <- sapply(ideology_levels, function(level) {
  if (level == ref_level) {
    coefs["comment_EMI"]
  } else {
    coefs["comment_EMI"] + coefs[paste0("comment_EMI:ideology_group", level)]
  }
})

# Format labels
group_labels <- paste0(ideology_levels, "(", round(slopes, 3), ")")
names(group_labels) <- ideology_levels

plot_PolDis <- ggplot(grid_interact_PolDis, aes(x = comment_EMI, y = pred, color = ideology_group, fill = ideology_group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2, color = NA) +
  theme_minimal(base_size = 14) +
  labs(
    title = "PoliticalDiscussion Interaction: Comment EMI × Ideology Group",
    x = "Comment EMI",
    y = "Predicted Probability of Disagreement",
    color = "Ideology Group",
    fill = "Ideology Group"
  )+
  scale_color_discrete(labels = group_labels) +
  scale_fill_discrete( labels = group_labels)+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "right"
  )



# Politics
summary(model_h4_2_politics)
ideology_levels <- levels(model_data_politics$ideology_group)

grid_interact_Pol <- expand.grid(
  comment_EMI = seq(min(model_data_politics$comment_EMI), max(model_data_politics$comment_EMI), length.out = 100),
  ideology_group = ideology_levels
)

# Set other variables to typical values
grid_interact_Pol$submission_EMI <- mean(model_data_politics$submission_EMI, na.rm = TRUE)
grid_interact_Pol$Mod_Dummy <- factor("0", levels = c("0", "1"))  # or leave out for `fit_model_h4_no_mod`

# Add fake row with 2nd levels of factors to avoid contrast error
grid_fake <- grid_interact_Pol
new_row <- grid_interact_Pol[1, ]


if ("Mod_Dummy" %in% names(grid_interact_Pol)) {
  new_row$Mod_Dummy <- factor("1", levels = levels(grid_interact_Pol$Mod_Dummy))
}

grid_fake <- bind_rows(grid_fake, new_row)

X_Pol <- model.matrix(~ submission_EMI + comment_EMI * ideology_group + Mod_Dummy, data = grid_fake)
# Remove fake row from model matrix and grid
X_Pol <- X_Pol[-nrow(X_Pol), ]

beta_Pol <- fixef(model_h4_2_politics)
vcov_beta_Pol <- vcov(model_h4_2_politics)

grid_interact_Pol$pred <- as.vector(X_Pol %*% beta_Pol)
se <- sqrt(rowSums((X_Pol %*% vcov_beta_Pol) * X_Pol))

grid_interact_Pol$ci_low <- grid_interact_Pol$pred - 1.96 * se
grid_interact_Pol$ci_high <- grid_interact_Pol$pred + 1.96 * se

# Extract coefficients
coefs <- fixef(model_h4_2_politics)

# Get ideology group levels except the baseline
ideology_levels <- levels(model_data_politics$ideology_group)
ref_level <- ideology_levels[1]
other_levels <- ideology_levels[-1]

# Get slopes for each group
slopes <- sapply(ideology_levels, function(level) {
  if (level == ref_level) {
    coefs["comment_EMI"]
  } else {
    coefs["comment_EMI"] + coefs[paste0("comment_EMI:ideology_group", level)]
  }
})

# Format labels
group_labels <- paste0(ideology_levels, "(", round(slopes, 3), ")")
names(group_labels) <- ideology_levels

plot_Pol <- ggplot(grid_interact_Pol, aes(x = comment_EMI, y = pred, color = ideology_group, fill = ideology_group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2, color = NA) +
  theme_minimal(base_size = 14) +
  labs(
    title = "politics: Interaction: Comment EMI × Ideology Group",
    x = "Comment EMI",
    y = "Predicted Probability of Disagreement",
    color = "Ideology Group",
    fill = "Ideology Group"
  )+
  scale_color_discrete(labels = group_labels) +
  scale_fill_discrete( labels = group_labels)+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "right"
  )

combined_plot_interactions <- plot_Pol + plot_PolDis + plot_layout(ncol = 2)
ggsave("H4_plots_OLS/H4_byIdeology_Interaction_Eff.pdf", combined_plot_interactions, width = 20, height = 10, dpi = 300)









# ------------------------------
# ------------------------------
# Ideology with sub and com, no grouping
# -------------------------------
# ------------------------------


# make stratified models for each ideological combination

model_data_renamed <- model_data %>%
  mutate(
    ideology_group = dplyr::recode(ideology_group,
                                   "PR & CR" = "CR & RR",
                                   "PR & CL" = "CR & RL",
                                   "PL & CR" = "CL & RR",
                                   "PL & CL" = "CL & RL"
    ),
    emi_group = dplyr::recode(emi_group,
                              "PE & CE" = "CE & RE",
                              "PE & CI" = "CE & RI",
                              "PI & CE" = "CI & RE",
                              "PI & CI" = "CI & RI"
    )
  )
model_data_renamed



model_data_PR_CR_clean <- model_data_renamed %>%
  filter(ideology_group == "CR & RR")

model_data_PR_CL_clean <- model_data_renamed %>%
  filter(ideology_group == "CR & RL")

model_data_PL_CL_clean <- model_data_renamed %>%
  filter(ideology_group == "CL & RL")

model_data_PL_CR_clean <- model_data_renamed %>%
  filter(ideology_group == "CL & RR")







fit_model_h4_ideology_clean <- function(data) {
  lmer(prob_disagree ~ submission_EMI + comment_EMI +
         factor(Mod_Dummy) + 
         (1 | submission_id) +
         (1 | comment_id), 
       data = data,
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
}

model_h4_2_PR_CR_all_clean <- fit_model_h4_ideology_clean(model_data_PR_CR_clean)
model_h4_2_PR_CL_all_clean <- fit_model_h4_ideology_clean(model_data_PR_CL_clean)
model_h4_2_PL_CR_all_clean <- fit_model_h4_ideology_clean(model_data_PL_CR_clean)
model_h4_2_PL_CL_all_clean <- fit_model_h4_ideology_clean(model_data_PL_CL_clean)




summary(model_h4_2_PR_CR_all_clean)
confint(model_h4_2_PR_CR_all_clean, method = "Wald", level = 0.95)


summary(model_h4_2_PR_CL_all_clean)
confint(model_h4_2_PR_CL_all_clean, method = "Wald", level = 0.95)


summary(model_h4_2_PL_CL_all_clean)
confint(model_h4_2_PL_CL_all_clean, method = "Wald", level = 0.95)


summary(model_h4_2_PL_CR_all_clean)
confint(model_h4_2_PL_CR_all_clean, method = "Wald", level = 0.95)






# Combine to Lists
# ----------------

# Prepare list of models and corresponding data
models_list_h4_all_clean <- list(
  "CR & RR" = list(model = model_h4_2_PR_CR_all_clean, data = model_data_PR_CR_clean),
  "CR & RL" = list(model = model_h4_2_PR_CL_all_clean, data = model_data_PR_CL_clean),
  "CL & RL" = list(model = model_h4_2_PL_CL_all_clean, data = model_data_PL_CL_clean),
  "CL & RR" = list(model = model_h4_2_PL_CR_all_clean, data = model_data_PL_CR_clean)
  
)


# Function to sample and prepare predicted data
#----------------------------------------------
prepare_prediction_data <- function(model, data, predictor, n = 1000) {
  
  # Sample for plotting
  data_sample <- data %>% sample_n(min(n, nrow(data)))
  
  # Create grid for chosen predictor
  grid <- tibble(
    !!predictor := seq(
      min(data_sample[[predictor]], na.rm = TRUE),
      max(data_sample[[predictor]], na.rm = TRUE),
      length.out = 100
    )
  )
  
  # Add required covariates, using means or references
  if (predictor != "submission_EMI" && "submission_EMI" %in% names(data)) {
    grid$submission_EMI <- mean(data$submission_EMI, na.rm = TRUE)
  }
  
  if (predictor != "comment_EMI" && "comment_EMI" %in% names(data)) {
    grid$comment_EMI <- mean(data$comment_EMI, na.rm = TRUE)
  }
  
  if ("Mod_Dummy" %in% names(data)) {
    grid$Mod_Dummy <- factor("0", levels = c("0", "1"))
  }
  
  # Add fake row with 2nd levels of factors to avoid contrast error
  grid_fake <- grid
  new_row <- grid[1, ]
  
  if ("Mod_Dummy" %in% names(grid)) {
    new_row$Mod_Dummy <- factor("1", levels = levels(grid$Mod_Dummy))
  }
  
  grid_fake <- bind_rows(grid_fake, new_row)
  
  # Create design matrix with fixed effects only formula (drop response)
  formula_fixed <- formula(model, fixed.only = TRUE)[-2]
  
  X <- model.matrix(formula_fixed, data = grid_fake)
  
  # Remove fake row from model matrix and grid
  X <- X[-nrow(X), ]
  grid <- grid
  
  # Get fixed effects coefficients and variance-covariance matrix
  beta <- fixef(model)
  vcov_beta <- vcov(model)
  
  # Calculate predictions and SE
  grid$pred <- as.numeric(X %*% beta)
  pred_se <- sqrt(rowSums((X %*% vcov_beta) * X))
  
  grid <- grid %>%
    mutate(
      ci_low = pred - 1.96 * pred_se,
      ci_high = pred + 1.96 * pred_se
    )
  
  return(grid)
}



# Generate combined data for submission_EMI and comment_EMI
submission_plot_data <- map_dfr(names(models_list_h4_all_clean), function(name) {
  df <- prepare_prediction_data(models_list_h4_all_clean[[name]]$model, models_list_h4_all_clean[[name]]$data, "submission_EMI")
  df$ideology <- name
  df$predictor <- "submission_EMI"
  df
})


comment_plot_data <- map_dfr(names(models_list_h4_all_clean), function(name) {
  df <- prepare_prediction_data(models_list_h4_all_clean[[name]]$model, models_list_h4_all_clean[[name]]$data, "comment_EMI")
  df$ideology <- name
  df$predictor <- "comment_EMI"
  df
})

obs_data <- bind_rows(lapply(names(models_list_h4_all_clean), function(name) {
  df <- models_list_h4_all_clean[[name]]$data %>%
    dplyr::select(submission_EMI, prob_disagree, comment_EMI) %>%
    mutate(ideology = name) %>%
    sample_n(min(10000, n()))
}))


# ---- Plotting ----
#----------------------------------------------

# Scatter + regression for submission_EMI
coef_labels_submission <- sapply(names(models_list_h4_all_clean), function(name) {
  model <- models_list_h4_all_clean[[name]]$model
  coef <- fixef(model)["submission_EMI"]
  label <- paste0(name, " (", round(coef, 3), ")")
  return(label)
})
names(coef_labels_submission) <- names(models_list_h4_all_clean)
submission_plot_data$ideology <- coef_labels_submission[submission_plot_data$ideology]
obs_data$ideology <- coef_labels_submission[obs_data$ideology]

# Submission EMI Plot
p_submission <- ggplot() +
  geom_point(data = obs_data, aes(x = submission_EMI, y = prob_disagree, color = ideology), alpha = 0.1, size = 1.2) +
  geom_line(data = submission_plot_data, aes(x = submission_EMI, y = pred, color = ideology), size = 1) +
  geom_ribbon(data = submission_plot_data, aes(x = submission_EMI, ymin = ci_low, ymax = ci_high, fill = ideology), alpha = 0.2, color = NA) +
  theme_bw(base_size = 16) +
  labs(
    # title = "Predicted Disagreement by Submission EMI",
    x = "Submission EMI",
    y = "Probability of Disagreement",
    color = "Ideology (Coef)",
    fill = "Ideology (Coef)"
  ) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "right"
  )



# Scatter + regression for comment_EMI
obs_data <- bind_rows(lapply(names(models_list_h4_all_clean), function(name) {
  df <- models_list_h4_all_clean[[name]]$data %>%
    dplyr::select(submission_EMI, prob_disagree, comment_EMI) %>%
    mutate(ideology = name) %>%
    sample_n(min(10000, n()))
}))
coef_labels_comment <- sapply(names(models_list_h4_all_clean), function(name) {
  model <- models_list_h4_all_clean[[name]]$model
  coef <- fixef(model)["comment_EMI"]
  label <- paste0(name, " (", round(coef, 3), ")")
  return(label)
})
names(coef_labels_comment) <- names(models_list_h4_all_clean)
comment_plot_data$ideology <- coef_labels_comment[comment_plot_data$ideology]
obs_data$ideology <- coef_labels_comment[obs_data$ideology]

# Comment EMI Plot
p_comment <-  ggplot() +
  geom_point(data = obs_data, aes(x = comment_EMI, y = prob_disagree, color = ideology), alpha = 0.1, size = 1.2) +
  geom_line(data = comment_plot_data, aes(x = comment_EMI, y = pred, color = ideology), size = 1) +
  geom_ribbon(data = comment_plot_data, aes(x = comment_EMI, ymin = ci_low, ymax = ci_high, fill = ideology), alpha = 0.2, color = NA) +
  theme_bw(base_size = 14) +
  labs(
    #title = "Predicted Disagreement by Comment EMI",
    x = "Comment EMI",
    y = "Probability of Disagreement",
    color = "Ideology (Coef)",  # Legend title
    fill = "Ideology (Coef)"
  ) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "right"
  )


# Save plots
ggsave("H4_plots_OLS/H4_byIdeology_all_Submission_EMI_Disagreement.pdf", p_submission, width = 10, height = 6, dpi = 300)
ggsave("H4_plots_OLS/H4_byIdeology_all_CommentEMI_Disagreement.pdf", p_comment, width = 10, height = 6, dpi = 300)

ggsave("H4_plots_OLS/H4_byIdeology_all_Submission_EMI_Disagreement.png", p_submission, width = 10, height = 6, dpi = 300)
ggsave("H4_plots_OLS/H4_byIdeology_all_CommentEMI_Disagreement.png", p_comment, width = 10, height = 6, dpi = 300)


# Optional: display side-by-side
library(patchwork)
combined_plot <- p_submission + p_comment + plot_layout(ncol = 1)
ggsave("H4_plots_OLS/H4_byIdeology_all_Combined_Sub_vs_Comment.pdf", combined_plot, width = 14, height = 20, dpi = 300)






























