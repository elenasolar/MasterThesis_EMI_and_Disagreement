
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




model_data <- read.csv("../../data/analysis_data/R_data/model_data.csv")


# -------------------------------------------------------------------------------
# H2: Higher submission and comment EMI leads to higher reply EMI
## coefficient of comment EMI (more) positive and significant
# ------------------------------------------------------------------------------

# ----------------
# Sample the Data
# ----------------

set.seed(123)
model_data_sample <- model_data %>%
  group_by(subreddit) %>%
  sample_frac(0.1) %>%
  ungroup()

# --------------
# Null Models H2
# --------------
null_model_h2 <- lmer(
  reply_EMI ~ factor(Mod_Dummy) +
    (1 | subreddit) +
    (1 | comment_id) +
    (1 | submission_id),
  data = model_data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(null_model_h2) # failed to converge, max|grad| = 0.004 > 0.002
# model nearly unidentifyable, large eigenvalue



# Model v1 (Full Random Effects): Random intercepts & slopes for subreddit
model_h2_full_1 <- lmer(
  reply_EMI ~ submission_EMI + comment_EMI + factor(Mod_Dummy) +
    (1 + submission_EMI + comment_EMI | subreddit) +  # random intercept & slopes
    (1 | comment_id) +                                # comment appears multiple times
    (1 | submission_id),                              # submission appears multiple times
  data = model_data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model_h2_full_1)
isSingular(model_h2_full_1) # True -> problematic
summary(model_h2_full_1)$varcor # highly correlated comment and submission EMI 

coef(model_h2_full_1)

# Notes:
# - Warning: Singular fit (likely due to near-zero variance or high correlation)
# - Correlation between intercept and slopes is very high (-0.98)
# - This suggests overfitting or overparameterization → try simpler model



# Model v2 (Simplified Random Effects): Random intercept only for subreddit
model_h2_full_2 <- lmer(reply_EMI ~ submission_EMI + comment_EMI + factor(Mod_Dummy) +
                          (1 | subreddit) +
                          (1 | comment_id) +
                          (1 | submission_id),
                        data = model_data,
                        control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# checked nesting the random effects: could not converge


isSingular(model_h2_full_2) # no
summary(model_h2_full_2)# -> convergence issue! 0.04 > 0.002, nearly unidentifybale

confint(model_h2_full_2, method = "Wald", level = 0.95)



# - `comment_EMI`: Strongest and significant effect (supports H2)
# - `submission_EMI`: Smaller but also significant
# - `Mod_Dummy`: Also significant
# - Most variance at comment and submission level; subreddit contributes little


# zero variance in subreddit interceot --> remove
# Model 3 (Simplified Random Effects): Random intercept only for submission and comment
model_h2_full_3 <- lmer(reply_EMI ~ submission_EMI + comment_EMI + factor(Mod_Dummy) +
                          (1 | comment_id) +
                          (1 | submission_id),
                        data = model_data,
                        control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
# -> 
isSingular(model_h2_full_3) # no
summary(model_h2_full_3) # -> convergence issue! 0.005 > 0.002, nearly unidentifybale


# split by comment and submission
model_h2_full_4_comment <- lmer(reply_EMI ~ comment_EMI + factor(Mod_Dummy) +
                                  (1 | comment_id) +
                                  (1 | submission_id), # because using Mod Dummy
                                data = model_data,
                                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
# -> 
isSingular(model_h2_full_4_comment) # no
summary(model_h2_full_4_comment) # still very large eigenvalue, 0.004 > 0.002


# submission
model_h2_full_4_submission <- lmer(reply_EMI ~ submission_EMI + factor(Mod_Dummy) +
                                     (1 | submission_id),
                                   data = model_data,
                                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
isSingular(model_h2_full_4_submission) # no
summary(model_h2_full_4_submission) # fine


model_h2_full_4_submission_interact <- lmer(reply_EMI ~ submission_EMI * factor(Mod_Dummy) +
                                              (1 | submission_id),
                                            data = model_data,
                                            control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
) #

isSingular(model_h2_full_4_submission_interact) # no
summary(model_h2_full_4_submission_interact) # apparently fine








# Model Comparison: Which model fits better?
anova_h2 <- anova(model_h2_full_1, model_h2_full_2, model_h2_full_3)
anova_h2
# - model 1 better, but singular
# 

anova_h2_1 <- anova(model_h2_full_4_comment, model_h2_full_4_submission, model_h2_full_4_submission_interact)
anova_h2_1




# Save model results
# ------------------
# Extract data
model_data_h2 <- model.frame(model_h2_full_2)
model_data_h2$fitted_h2 <- fitted(model_h2_full_2)
model_data_h2$residuals_h2 <- resid(model_h2_full_2)

# Extract random effects by subreddit
ranef_df_h2 <- coef(model_h2_full_2)$subreddit
ranef_df_h2$subreddit <- rownames(ranef_df_h2)
ranef_df_h2


write.csv(model_data_h2, "../../data/analysis_data/R_data/model_data_h2.csv", row.names = FALSE)
write.csv(ranef_df_h2, "../../data/analysis_data/R_data/ranef_df_h2.csv", row.names = FALSE)


screenreg(list(
  Null = null_model_h2,
  Full_1 = model_h2_full_1,
  Full_2 = model_h2_full_2,
  Full_3 = model_h2_full_3, 
  comment = model_h2_full_4_comment,
  submission = model_h2_full_4_submission,
  submission_inter = model_h2_full_4_submission_interact
))

screenreg(list(
  Null = null_model_h2,
  Full_1 = model_h2_full_1,
  Full_2 = model_h2_full_2,
  Full_3 = model_h2_full_3
))



# --------------------------------------------------------------------------------
# Model Diagnostics & Visualization
# --------------------------------------------------------------------------------
set.seed(123)  # for reproducibility
model_data_h2_sample <- model_data_h2[sample(nrow(model_data_h2), 10000), ]  # sample 10,000 rows

title_size <- 12  

# Plot 1: Fitted vs. Residuals
p1_h2 <- ggplot(model_data_h2_sample, aes(x = fitted_h2, y = residuals_h2)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_bw(base_size = 12) +
  theme(aspect.ratio = 1, plot.title = element_text(size = title_size)) +
  labs(title = "Fitted vs. Residuals", x = "Fitted", y = "Residuals")

# Q-Q Plot
qq_data_h2 <- data.frame(
  theoretical = qqnorm(model_data_h2_sample$residuals_h2, plot.it = FALSE)$x,
  sample = qqnorm(model_data_h2_sample$residuals_h2, plot.it = FALSE)$y
)

p2_h2 <- ggplot(qq_data_h2, aes(sample = sample)) +
  stat_qq(alpha = 0.4) +
  stat_qq_line(color = "red") +
  theme_bw(base_size = 12) +
  theme(aspect.ratio = 1, plot.title = element_text(size = title_size)) +
  labs(title = "Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles")

# Histogram
p3_h2 <- ggplot(model_data_h2_sample, aes(x = residuals_h2)) +
  geom_histogram(binwidth = 0.25, fill = "#0072B2", color = "white", alpha = 0.8) +
  theme_bw(base_size = 12) +
  theme(aspect.ratio = 1, plot.title = element_text(size = title_size)) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency")


# Combine into one row
model_h2_diagnostic_plot = p1_h2 + p2_h2 + p3_h2 + plot_layout(ncol = 3)
model_h2_diagnostic_plot
# Save with custom size to preserve square shape (adjust height if necessary)
ggsave("H2_plots_OLS/H2_Diagnostic.pdf", model_h2_diagnostic_plot, width = 12, height = 4, dpi = 600)
ggsave("H2_plots_OLS/H2_Diagnostic.png", model_h2_diagnostic_plot, width = 12, height = 4, dpi = 300)




confint(model_h2_full_2, method = "Wald")



model_data_h2_sample <- model_data_h2_sample %>%
  rename(Mod_Dummy = 'factor(Mod_Dummy)') %>%
  mutate(Mod_Dummy = factor(Mod_Dummy, levels = c("0", "1"), labels = c("Non-Moderator", "Moderator")))






# Fixed effect coefficients and variance-covariance matrix
fixef_beta <- fixef(model_h2_full_2)
vcov_beta <- vcov(model_h2_full_2)

# Define sequences
submission_seq <- seq(min(model_data$submission_EMI, na.rm = TRUE),
                      max(model_data$submission_EMI, na.rm = TRUE),
                      length.out = 100)

comment_seq <- seq(min(model_data$comment_EMI, na.rm = TRUE),
                   max(model_data$comment_EMI, na.rm = TRUE),
                   length.out = 100)

# Define levels for moderator dummy
mod_levels <- c("0", "1")

# ---------------------------
# Prediction grid: Submission
# ---------------------------
submission_pred_df <- expand_grid(
  submission_EMI = submission_seq,
  Mod_Dummy = factor(mod_levels, levels = c("0", "1"))
) %>%
  mutate(
    comment_EMI = mean(model_data$comment_EMI, na.rm = TRUE)  # control for other variable
  )

# Add fake row to avoid contrast issues
fake_row <- submission_pred_df[1, ]
fake_row$Mod_Dummy <- factor(ifelse(fake_row$Mod_Dummy == "0", "1", "0"), levels = c("0", "1"))
submission_pred_df_fake <- bind_rows(submission_pred_df, fake_row)

# Design matrix for submission
X_sub <- model.matrix(~ submission_EMI + comment_EMI + factor(Mod_Dummy), data = submission_pred_df_fake)
X_sub <- X_sub[-nrow(X_sub), ]  # remove fake row
submission_pred_df <- submission_pred_df_fake[-nrow(submission_pred_df_fake), ]

# Compute predictions
submission_pred_df$pred <- as.numeric(X_sub %*% fixef_beta)
pred_se <- sqrt(rowSums((X_sub %*% vcov_beta) * X_sub))
submission_pred_df <- submission_pred_df %>%
  mutate(
    ci_low = pred - 1.96 * pred_se,
    ci_high = pred + 1.96 * pred_se,
    Mod_Dummy = factor(Mod_Dummy, levels = c("0", "1"), labels = c("Non-Moderator", "Moderator"))
  )

# ---------------------------
# Prediction grid: Comment
# ---------------------------
comment_pred_df <- expand_grid(
  comment_EMI = comment_seq,
  Mod_Dummy = factor(mod_levels, levels = c("0", "1"))
) %>%
  mutate(
    submission_EMI = mean(model_data$submission_EMI, na.rm = TRUE)
  )

# Add fake row
fake_row <- comment_pred_df[1, ]
fake_row$Mod_Dummy <- factor(ifelse(fake_row$Mod_Dummy == "0", "1", "0"), levels = c("0", "1"))
comment_pred_df_fake <- bind_rows(comment_pred_df, fake_row)

# Design matrix
X_com <- model.matrix(~ submission_EMI + comment_EMI + factor(Mod_Dummy), data = comment_pred_df_fake)
X_com <- X_com[-nrow(X_com), ]
comment_pred_df <- comment_pred_df_fake[-nrow(comment_pred_df_fake), ]

# Compute predictions
comment_pred_df$pred <- as.numeric(X_com %*% fixef_beta)
pred_se <- sqrt(rowSums((X_com %*% vcov_beta) * X_com))
comment_pred_df <- comment_pred_df %>%
  mutate(
    ci_low = pred - 1.96 * pred_se,
    ci_high = pred + 1.96 * pred_se,
    Mod_Dummy = factor(Mod_Dummy, levels = c("0", "1"), labels = c("Non-Moderator", "Moderator"))
  )

# ---------------------------
# Plotting
# ---------------------------
p1 <- ggplot(data = submission_pred_df, aes(x = submission_EMI, y = pred, color = Mod_Dummy, fill = Mod_Dummy)) +
  geom_point(data = model_data_h2_sample,
             aes(x = submission_EMI, y = reply_EMI, color = Mod_Dummy), alpha = 0.1, show.legend = FALSE) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.3, color = NA) +
  scale_color_manual(values = c("Non-Moderator" = "#1b9e77", "Moderator" = "#d95f02")) +
  scale_fill_manual(values = c("Non-Moderator" = "#1b9e77", "Moderator" = "#d95f02")) +
  labs(x = "Submission EMI", y = "Predicted Reply EMI") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")


p2 <- ggplot(data = comment_pred_df, aes(x = comment_EMI, y = pred, color = Mod_Dummy, fill = Mod_Dummy)) +
  geom_point(data = model_data_h2_sample,
             aes(x = comment_EMI, y = reply_EMI, color = Mod_Dummy), alpha = 0.1, show.legend = FALSE) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.3, color = NA) +
  scale_color_manual(values = c("Non-Moderator" = "#1b9e77", "Moderator" = "#d95f02")) +
  scale_fill_manual(values = c("Non-Moderator" = "#1b9e77", "Moderator" = "#d95f02")) +
  labs(x = "Comment EMI", y = "Predicted Reply EMI",
     color = "Mod_Dummy", fill = "Mod_Dummy") +
  theme_bw(base_size = 14) 


# Combine
combined_plot <- p1 + p2 + plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

combined_plot

# Save
ggsave("H2_FixedEffect_combined.pdf", combined_plot, width = 12, height = 6, dpi = 600)

p1 <- p1 +
  theme(legend.position = "right")

ggsave("H2_plots_OLS/H2_FixedEffect_submissions.pdf", p1, width = 12, height = 6, dpi = 600)
ggsave("H2_plots_OLS/H2_FixedEffect_comments.pdf", p2, width = 12, height = 6, dpi = 600)

ggsave("H2_plots_OLS/H2_FixedEffect_submissions.png", p1, width = 12, height = 6, dpi = 300)
ggsave("H2_plots_OLS/H2_FixedEffect_comments.png", p2, width = 12, height = 6, dpi = 300)






