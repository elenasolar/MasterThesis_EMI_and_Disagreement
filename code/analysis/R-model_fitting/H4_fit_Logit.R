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


logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}


model_data <- read.csv("../../data/analysis_data/R_data/model_data.csv")

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







# -------------------------
# LOGIT MODEL per subreddit
# -------------------------

# Fit Models Function
#--------------------
fit_model_h4_logit <- function(data) {
  glmer(
    disagree_label ~ submission_EMI + comment_EMI * ideology_group + factor(Mod_Dummy) +
      (1 | submission_id) + (1 | comment_id),
    data = data,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
}

fit_model_h4_no_mod_logit <- function(data) {
  glmer(
    disagree_label ~ submission_EMI + comment_EMI * ideology_group +
      (1 | submission_id) + (1 | comment_id),
    data = data,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
}

# Fit the models for each subreddit
model_h4_2_Ask_Politics_logit <- fit_model_h4_logit(model_data_Ask_Politics)
model_h4_2_PoliticalDebate_logit <- fit_model_h4_logit(model_data_PoliticalDebate)
model_h4_2_politics_logit <- fit_model_h4_logit(model_data_politics)

summary(model_h4_2_Ask_Politics_logit)
confint(model_h4_2_Ask_Politics_logit, method = "Wald", level = 0.95)

summary(model_h4_2_PoliticalDebate_logit)
confint(model_h4_2_PoliticalDebate_logit, method = "Wald", level = 0.95)

summary(model_h4_2_politics_logit)
confint(model_h4_2_politics_logit, method = "Wald", level = 0.95)

# no mods
model_h4_2_Askpolitics_logit <- fit_model_h4_no_mod_logit(model_data_Askpolitics)
model_h4_2_PoliticalDiscussion_logit <- fit_model_h4_no_mod_logit(model_data_PoliticalDiscussion)
model_h4_2_NeutralPolitics_logit <- fit_model_h4_no_mod_logit(model_data_NeutralPolitics)

summary(model_h4_2_Askpolitics_logit)
confint(model_h4_2_Askpolitics_logit, method = "Wald", level = 0.95)


summary(model_h4_2_PoliticalDiscussion_logit)
confint(model_h4_2_PoliticalDiscussion_logit, method = "Wald", level = 0.95)

summary(model_h4_2_NeutralPolitics_logit)
confint(model_h4_2_NeutralPolitics_logit, method = "Wald", level = 0.95)



prepare_prediction_data <- function(model, data, predictor, n = 1000) {
  # Sample for plotting
  data_sample <- data %>% dplyr::sample_n(min(n, nrow(data)))
  
  # Create grid for focal predictor
  grid <- tibble(
    !!predictor := seq(
      min(data_sample[[predictor]], na.rm = TRUE),
      max(data_sample[[predictor]], na.rm = TRUE),
      length.out = 100
    )
  )
  
  # Add required covariates
  if ("ideology_group" %in% names(data)) {
    grid$ideology_group <- factor("PL & CL", levels = levels(data$ideology_group))
  }
  
  if (predictor != "submission_EMI" && "submission_EMI" %in% names(data)) {
    grid$submission_EMI <- mean(data$submission_EMI, na.rm = TRUE)
  }
  
  if (predictor != "comment_EMI" && "comment_EMI" %in% names(data)) {
    grid$comment_EMI <- data %>%
      filter(ideology_group == "PL & CL") %>%
      summarize(mean_comment_EMI = mean(comment_EMI, na.rm = TRUE)) %>%
      pull(mean_comment_EMI)
  }
  
  if ("Mod_Dummy" %in% names(data)) {
    grid$Mod_Dummy <- factor("0", levels = levels(data$Mod_Dummy))
  }
  
  # Ensure all factor levels are included in design matrix construction
  grid_augmented <- grid
  if ("ideology_group" %in% names(grid)) {
    grid_augmented <- bind_rows(
      grid,
      grid %>% slice(1) %>% mutate(ideology_group = factor("PR & CL", levels = levels(data$ideology_group)))
    )
  }
  
  if ("Mod_Dummy" %in% names(grid)) {
    grid_augmented <- bind_rows(
      grid_augmented,
      grid %>% slice(1) %>% mutate(Mod_Dummy = factor("1", levels = levels(data$Mod_Dummy)))
    )
  }
  
  # Build design matrix from fixed effects
  formula_fixed <- formula(model, fixed.only = TRUE)[-2]
  X <- model.matrix(formula_fixed, data = grid_augmented)
  
  # Keep only the rows corresponding to original grid
  X <- X[1:nrow(grid), ]
  
  # Prediction on logit scale
  beta <- fixef(model)
  vcov_beta <- vcov(model)
  
  grid$logit_pred <- as.numeric(X %*% beta)
  pred_se <- sqrt(rowSums((X %*% vcov_beta) * X))
  
  # Convert to probabilities
  grid <- grid %>%
    mutate(
      prob_pred = plogis(logit_pred),
      ci_low = plogis(logit_pred - 1.96 * pred_se),
      ci_high = plogis(logit_pred + 1.96 * pred_se)
    )
  
  return(grid)
}



# Prepare list of models and corresponding data
models_list_logit <- list(
  "Ask_Politics" = list(model = model_h4_2_Ask_Politics_logit, data = model_data_Ask_Politics),
  "PoliticalDebate" = list(model = model_h4_2_PoliticalDebate_logit, data = model_data_PoliticalDebate),
  "politics" = list(model = model_h4_2_politics_logit, data = model_data_politics),
  "Askpolitics" = list(model = model_h4_2_Askpolitics_logit, data = model_data_Askpolitics),
  "PoliticalDiscussion" = list(model = model_h4_2_PoliticalDiscussion_logit, data = model_data_PoliticalDiscussion),
  "NeutralPolitics" = list(model = model_h4_2_NeutralPolitics_logit, data = model_data_NeutralPolitics)
)


# Generate combined data for submission_EMI and comment_EMI
submission_plot_data_logit <- map_dfr(names(models_list_logit), function(name) {
  df <- prepare_prediction_data(models_list_logit[[name]]$model, models_list_logit[[name]]$data, "submission_EMI")
  df$subreddit <- name
  df$predictor <- "submission_EMI"
  df
})
submission_plot_data_logit %>%
  filter(subreddit == "politics") %>%
  ggplot(aes(submission_EMI, prob_pred))+
  geom_line()

submission_plot_data_logit %>%
  filter(subreddit == "politics") %>%
  ggplot(aes(submission_EMI, logit_pred))+
  geom_line()


comment_plot_data_logit <- map_dfr(names(models_list_logit), function(name) {
  df <- prepare_prediction_data(models_list_logit[[name]]$model, models_list_logit[[name]]$data, "comment_EMI")
  df$subreddit <- name
  df$predictor <- "comment_EMI"
  df
})


obs_data_logit <- bind_rows(lapply(names(models_list_logit), function(name) {
  df <- models_list_logit[[name]]$data %>%
    dplyr::select(submission_EMI, prob_disagree, comment_EMI) %>%
    mutate(subreddit = name) %>%
    sample_n(min(10000, n()))
}))



######################
coef_labels_submission <- sapply(names(models_list_logit), function(name) {
  model <- models_list_logit[[name]]$model
  coef <- fixef(model)["submission_EMI"]
  label <- paste0(name, " (", round(coef, 3), ")")
  return(label)
})
names(coef_labels_submission) <- names(models_list_logit)
submission_plot_data_logit$subreddit <- coef_labels_submission[submission_plot_data_logit$subreddit]
obs_data_logit$subreddit_sub <- coef_labels_submission[obs_data_logit$subreddit]


p_submission_logit <- ggplot() +
  geom_point(data = obs_data_logit, aes(x = submission_EMI, y = prob_disagree, color = subreddit_sub), alpha = 0.1, size = 1.2) +
  geom_line(data = submission_plot_data_logit, aes(x = submission_EMI, y = prob_pred, color = subreddit), size = 1) +
  geom_ribbon(data = submission_plot_data_logit, aes(x = submission_EMI, ymin = ci_low, ymax = ci_high, fill = subreddit), alpha = 0.2, color = NA) +
  theme_bw(base_size = 14) +
  labs(
    #title = "Predicted Disagreement by Submission EMI",
    x = "Submission EMI",
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


# Scatter + regression for comment_EMI
coef_labels_comment <- sapply(names(models_list_logit), function(name) {
  model <- models_list_logit[[name]]$model
  coef <- fixef(model)["comment_EMI"]
  label <- paste0(name, " (", round(coef, 3), ")")
  return(label)
})
names(coef_labels_comment) <- names(models_list_logit)
comment_plot_data_logit$subreddit <- coef_labels_comment[comment_plot_data_logit$subreddit]
obs_data_logit$subreddit_com <- coef_labels_comment[obs_data_logit$subreddit]



p_comment_logit <-  ggplot() +
  geom_point(data = obs_data_logit, aes(x = comment_EMI, y = prob_disagree, color = subreddit_com), alpha = 0.1, size = 1.2) +
  geom_line(data = comment_plot_data_logit, aes(x = comment_EMI, y = prob_pred, color = subreddit), size = 1) +
  geom_ribbon(data = comment_plot_data_logit, aes(x = comment_EMI, ymin = ci_low, ymax = ci_high, fill = subreddit), alpha = 0.2, color = NA) +
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


p_submission_logit
p_comment_logit

# Save plots
ggsave("H4_plots_Logit/H4_bySubreddit_Submission_EMI_Disagreement.pdf", p_submission_logit, width = 10, height = 6, dpi = 300)
ggsave("H4_plots_Logit/H4_bySubreddit_CommentEMI_Disagreement.pdf", p_comment_logit, width = 10, height = 6, dpi = 300)

ggsave("H4_plots_Logit/H4_bySubreddit_Submission_EMI_Disagreement.png", p_submission_logit, width = 10, height = 6, dpi = 300)
ggsave("H4_plots_Logit/H4_bySubreddit_CommentEMI_Disagreement.png", p_comment_logit, width = 10, height = 6, dpi = 300)

# Optional: display side-by-side
library(patchwork)
combined_plot_logit <- p_submission_logit + p_comment_logit + plot_layout(ncol = 1)
ggsave("H4_plots_Logit/H4_bySubreddit_Combined_Sub_vs_Comment.pdf", combined_plot_logit, width = 14, height = 20, dpi = 300)



# sainity checks on the politics subreddit

submission_plot_data_logit %>%
  filter(subreddit == "politics (-0.281)") %>%
  ggplot(aes(submission_EMI, prob_pred))+
  geom_line()

submission_plot_data_logit %>%
  filter(subreddit == "politics (-0.281)")%>%
  ggplot(aes(submission_EMI, logit_pred))+
  geom_line()



levels(model_data_Ask_Politics$Mod_Dummy)
table(model_data_PoliticalDebate$Mod_Dummy)







# ------------------------------
# ------------------------------
# Stratified Models by Ideology
# -------------------------------
# ------------------------------


# make stratified models for each ideological combination

model_data_PR_CR <- model_data %>%
  filter(ideology_group == "PR & CR")

model_data_PR_CL <- model_data %>%
  filter(ideology_group == "PR & CL")

model_data_PL_CL <- model_data %>%
  filter(ideology_group == "PL & CL")

model_data_PL_CR <- model_data %>%
  filter(ideology_group == "PL & CR")


fit_model_h4_ideology_logit <- function(data) {
  glmer(
    disagree_label ~ submission_EMI + comment_EMI + factor(Mod_Dummy) +
      (1 | submission_id) + (1 | comment_id),
    data = data,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
}

fit_model_h4_ideology_logit_com <- function(data) {
  glmer(
    disagree_label ~ comment_EMI + factor(Mod_Dummy) +
      (1 | submission_id) + (1 | comment_id),
    data = data,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
}

fit_model_h4_ideology_logit_sub <- function(data) {
  glmer(
    disagree_label ~ submission_EMI + factor(Mod_Dummy) +
      (1 | submission_id),
    data = data,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
}



# Fit the models for each ideology
model_h4_2_PR_CR_all <- fit_model_h4_ideology_logit(model_data_PR_CR)
model_h4_2_PR_CR_com <- fit_model_h4_ideology_logit_com(model_data_PR_CR)
model_h4_2_PR_CR_sub <- fit_model_h4_ideology_logit_sub(model_data_PR_CR)

summary(model_h4_2_PR_CR_all)
confint(model_h4_2_PR_CR_all, method = "Wald", level = 0.95)

#summary(model_h4_2_PR_CR_com)
#confint(model_h4_2_PR_CR_com, method = "Wald", level = 0.95)

#summary(model_h4_2_PR_CR_sub)
#confint(model_h4_2_PR_CR_sub, method = "Wald", level = 0.95)



model_h4_2_PR_CL_all <- fit_model_h4_ideology_logit(model_data_PR_CL)
model_h4_2_PR_CL_com <- fit_model_h4_ideology_logit_com(model_data_PR_CL)
model_h4_2_PR_CL_sub <- fit_model_h4_ideology_logit_sub(model_data_PR_CL)

summary(model_h4_2_PR_CL_all)
confint(model_h4_2_PR_CL_all, method = "Wald", level = 0.95)

#summary(model_h4_2_PR_CL_com)
#confint(model_h4_2_PR_CL_com, method = "Wald", level = 0.95)

#summary(model_h4_2_PR_CL_sub)
#confint(model_h4_2_PR_CL_sub, method = "Wald", level = 0.95)



model_h4_2_PL_CR_all <- fit_model_h4_ideology_logit(model_data_PL_CR)
model_h4_2_PL_CR_com <- fit_model_h4_ideology_logit_com(model_data_PL_CR)
model_h4_2_PL_CR_sub <- fit_model_h4_ideology_logit_sub(model_data_PL_CR)

summary(model_h4_2_PL_CR_all)
confint(model_h4_2_PL_CR_all, method = "Wald", level = 0.95)

#summary(model_h4_2_PL_CR_com)
#confint(model_h4_2_PL_CR_com, method = "Wald", level = 0.95)

#summary(model_h4_2_PL_CR_sub)
#confint(model_h4_2_PL_CR_sub, method = "Wald", level = 0.95)



model_h4_2_PL_CL_all <- fit_model_h4_ideology_logit(model_data_PL_CL)
model_h4_2_PL_CL_com <- fit_model_h4_ideology_logit_com(model_data_PL_CL)
model_h4_2_PL_CL_sub <- fit_model_h4_ideology_logit_sub(model_data_PL_CL)

summary(model_h4_2_PL_CL_all)
confint(model_h4_2_PL_CL_all, method = "Wald", level = 0.95)

#summary(model_h4_2_PL_CL_com)
#confint(model_h4_2_PL_CL_com, method = "Wald", level = 0.95)

#summary(model_h4_2_PL_CL_sub)
#confint(model_h4_2_PL_CL_sub, method = "Wald", level = 0.95)


#-----------------
# Combine to Lists
# ----------------

# Prepare list of models and corresponding data

# rename to match terminology of paper: comment and reply (rather than parent and child)

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


model_data_CR_RR <- model_data_PR_CR %>%
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
  
model_data_CR_RL <- model_data_PR_CL%>%
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
model_data_CL_RL <- model_data_PL_CL %>%
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
model_data_CL_RR <- model_data_PL_CR %>%
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



models_list_h4_all <- list(
  "CR & RR" = list(model = model_h4_2_PR_CR_all, data = model_data_CR_RR),
  "CR & RL" = list(model = model_h4_2_PR_CL_all, data = model_data_CR_RL),
  "CL & RL" = list(model = model_h4_2_PL_CL_all, data = model_data_CL_RL),
  "CL & RR" = list(model = model_h4_2_PL_CR_all, data = model_data_CL_RR)
  
)


# not changed
models_list_h4_com <- list(
  "PR & CR" = list(model = model_h4_2_PR_CR_com, data = model_data_PR_CR),
  "PR & CL" = list(model = model_h4_2_PR_CL_com, data = model_data_PR_CL),
  "PL & CL" = list(model = model_h4_2_PL_CL_com, data = model_data_PL_CL),
  "PL & CR" = list(model = model_h4_2_PL_CR_com, data = model_data_PL_CR)
  
)
models_list_h4_sub <- list(
  "PR & CR" = list(model = model_h4_2_PR_CR_sub, data = model_data_PR_CR),
  "PR & CL" = list(model = model_h4_2_PR_CL_sub, data = model_data_PR_CL),
  "PL & CL" = list(model = model_h4_2_PL_CL_sub, data = model_data_PL_CL),
  "PL & CR" = list(model = model_h4_2_PL_CR_sub, data = model_data_PL_CR)
  
)


prepare_prediction_data_ideology <- function(model, data, predictor, n = 1000) {
  # Sample for plotting
  data_sample <- data %>% dplyr::sample_n(min(n, nrow(data)))
  
  # Create grid for focal predictor
  grid <- tibble(
    !!predictor := seq(
      min(data_sample[[predictor]], na.rm = TRUE),
      max(data_sample[[predictor]], na.rm = TRUE),
      length.out = 100
    )
  )
  
  # Add required covariates
  
  if (predictor != "submission_EMI" && "submission_EMI" %in% names(data)) {
    grid$submission_EMI <- mean(data$submission_EMI, na.rm = TRUE)
  }
  
  if (predictor != "comment_EMI" && "comment_EMI" %in% names(data)) {
    grid$comment_EMI <- mean(data$comment_EMI, na.rm = TRUE)
  }
  
  if ("Mod_Dummy" %in% names(data)) {
    grid$Mod_Dummy <- factor("0", levels = c("0", "1"))
  }
  
  # Ensure all factor levels are included in design matrix construction
  grid_augmented <- grid
  
  if ("Mod_Dummy" %in% names(grid)) {
    grid_augmented <- bind_rows(
      grid_augmented,
      grid %>% slice(1) %>% mutate(Mod_Dummy = factor("1", levels = c("0", "1")))
    )
  }
  
  # Build design matrix from fixed effects
  formula_fixed <- formula(model, fixed.only = TRUE)[-2]
  X <- model.matrix(formula_fixed, data = grid_augmented)
  
  # Keep only the rows corresponding to original grid
  X <- X[1:nrow(grid), ]
  
  # Prediction on logit scale
  beta <- fixef(model)
  vcov_beta <- vcov(model)
  
  grid$logit_pred <- as.numeric(X %*% beta)
  pred_se <- sqrt(rowSums((X %*% vcov_beta) * X))
  
  # Convert to probabilities
  grid <- grid %>%
    mutate(
      prob_pred = plogis(logit_pred),
      ci_low = plogis(logit_pred - 1.96 * pred_se),
      ci_high = plogis(logit_pred + 1.96 * pred_se)
    )
  
  return(grid)
}


# Generate combined data for submission_EMI and comment_EMI
submission_plot_data_ideology <- map_dfr(names(models_list_h4_all), function(name) {
  df <- prepare_prediction_data_ideology(models_list_h4_all[[name]]$model, models_list_h4_all[[name]]$data, "submission_EMI")
  df$ideology <- name
  df$predictor <- "submission_EMI"
  df
})



comment_plot_data_ideology <- map_dfr(names(models_list_h4_all), function(name) {
  df <- prepare_prediction_data_ideology(models_list_h4_all[[name]]$model, models_list_h4_all[[name]]$data, "comment_EMI")
  df$ideology <- name
  df$predictor <- "comment_EMI"
  df
})


obs_data_ideology <- bind_rows(lapply(names(models_list_h4_all), function(name) {
  df <- models_list_h4_all[[name]]$data %>%
    dplyr::select(submission_EMI, prob_disagree, comment_EMI) %>%
    mutate(ideology = name) %>%
    sample_n(min(10000, n()))
}))



######################
coef_labels_submission <- sapply(names(models_list_h4_all), function(name) {
  model <- models_list_h4_all[[name]]$model
  coef <- fixef(model)["submission_EMI"]
  label <- paste0(name, " (", round(coef, 3), ")")
  return(label)
})
names(coef_labels_submission) <- names(models_list_h4_all)
submission_plot_data_ideology$ideology <- coef_labels_submission[submission_plot_data_ideology$ideology]
obs_data_ideology$ideology_sub <- coef_labels_submission[obs_data_ideology$ideology]


p_submission_ideology <- ggplot() +
  geom_point(data = obs_data_ideology, aes(x = submission_EMI, y = prob_disagree, color = ideology_sub), alpha = 0.1, size = 1.2) +
  geom_line(data = submission_plot_data_ideology, aes(x = submission_EMI, y = prob_pred, color = ideology), size = 1) +
  geom_ribbon(data = submission_plot_data_ideology, aes(x = submission_EMI, ymin = ci_low, ymax = ci_high, fill = ideology), alpha = 0.2, color = NA) +
  theme_bw(base_size = 14) +
  labs(
    #title = "Predicted Disagreement by Submission EMI",
    x = "Submission EMI",
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


# Scatter + regression for comment_EMI
coef_labels_comment <- sapply(names(models_list_h4_all), function(name) {
  model <- models_list_h4_all[[name]]$model
  coef <- fixef(model)["comment_EMI"]
  label <- paste0(name, " (", round(coef, 3), ")")
  return(label)
})
names(coef_labels_comment) <- names(models_list_h4_all)
comment_plot_data_ideology$ideology <- coef_labels_comment[comment_plot_data_ideology$ideology]
obs_data_ideology$ideology_com <- coef_labels_comment[obs_data_ideology$ideology]

obs_data_ideology
comment_plot_data_ideology

p_comment_ideology <-  ggplot() +
  geom_point(data = obs_data_ideology, aes(x = comment_EMI, y = prob_disagree, color = ideology_com), alpha = 0.1, size = 1.2) +
  geom_line(data = comment_plot_data_ideology, aes(x = comment_EMI, y = prob_pred, color = ideology), size = 1) +
  geom_ribbon(data = comment_plot_data_ideology, aes(x = comment_EMI, ymin = ci_low, ymax = ci_high, fill = ideology), alpha = 0.2, color = NA) +
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


p_submission_ideology
p_comment_ideology

# Save plots
ggsave("H4_plots_Logit/H4_byIdeology_Submission_EMI_Disagreement.pdf", p_submission_ideology, width = 10, height = 6, dpi = 300)
ggsave("H4_plots_Logit/H4_byIdeology_CommentEMI_Disagreement.pdf", p_comment_ideology, width = 10, height = 6, dpi = 300)

ggsave("H4_plots_Logit/H4_byIdeology_Submission_EMI_Disagreement.png", p_submission_ideology, width = 10, height = 6, dpi = 300)
ggsave("H4_plots_Logit/H4_byIdeology_CommentEMI_Disagreement.png", p_comment_ideology, width = 10, height = 6, dpi = 300)


# Optional: display side-by-side
library(patchwork)
combined_plot_ideology <- p_submission_ideology + p_comment_ideology + plot_layout(ncol = 1)
ggsave("H4_plots_Logit/H4_byIdeology_Combined_Sub_vs_Comment.pdf", combined_plot_ideology, width = 14, height = 20, dpi = 300)



