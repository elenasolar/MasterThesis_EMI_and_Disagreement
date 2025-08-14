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



# load data
model_data <- read.csv("../../data/analysis_data/R_data/model_data.csv")


# ------------------------------------------------------------------------------
# H1: Higher submission EMI --> higher document EMI
## coefficient of submission EMI positive and significant
# ------------------------------------------------------------------------------


# Format Document-Level Dataset
# -----------------------------
doc_level_data_com <- model_data %>%
  dplyr::select(comment_id, submission_id, submission_EMI, 
                comment_EMI, Mod_Dummy, subreddit) %>%
  distinct(comment_id, .keep_all = TRUE) %>%
  rename(text_id = comment_id,
         text_EMI = comment_EMI,
         submission_EMI = submission_EMI) %>%
  mutate(
    comment_type = 1
  )

doc_level_data_rep <- model_data %>%
  dplyr::select(reply_id, submission_id, submission_EMI, 
                reply_EMI, Mod_Dummy, subreddit) %>%
  distinct(reply_EMI, .keep_all = TRUE) %>%
  rename(text_id = reply_id,
         text_EMI = reply_EMI) %>%
  mutate(
    comment_type = 0
  )


doc_level_data <- bind_rows(doc_level_data_com, doc_level_data_rep)

write.csv(doc_level_data, "../../data/analysis_data/R_data/doc_level_data.csv", row.names = FALSE)

doc_level_data <- read.csv("../../data/analysis_data/R_data/doc_level_data.csv")






# Pre-Model Checks
# -----------------
# Check correlations and multicollinearity

cor_matrix <- doc_level_data %>%
  dplyr::select(text_EMI, submission_EMI, comment_type, Mod_Dummy) %>%
  cor(use = "complete.obs")
print(cor_matrix)

vif_model_h1 <- lm(text_EMI ~ submission_EMI + factor(comment_type) + factor(Mod_Dummy), data = doc_level_data)
vif(vif_model_h1)


# NULL MODEL
# -----------

null_model_h1_full <- lmer(
  text_EMI ~ 1 + (1 | subreddit) + (1 | submission_id),
  data = doc_level_data
)


# FIXED EFFECTS MODEL VARIANTS
# -----------------------------

# Model 1: Random slope (submission_EMI) per subreddit
model_h1_full_1 <- lmer(text_EMI ~ submission_EMI + factor(comment_type) + factor(Mod_Dummy) +
                          (1 + submission_EMI | subreddit) + 
                          (1 | submission_id),
                        data = doc_level_data,
                        control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(model_h1_full_1)
# Model failed to converge with max|grad| = 0.00459611 (tol = 0.002, component 1)
# changed optimizer: now converged!

coef(model_h1_full_1)$subreddit

confint(model_h1_full_1, method = "Wald", level = 0.95)




# Model 2: Check: No random slope better?
model_h1_full_2 <- lmer(text_EMI ~ submission_EMI + factor(comment_type) + factor(Mod_Dummy) +
                          (1 | subreddit) +
                          (1 | submission_id),
                        data = doc_level_data,
                        control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(model_h1_full_2)





# Model Comparison 
# -----------------
anova_h1 = anova(model_h1_full_1, model_h1_full_2)

AIC(model_h1_full_1, model_h1_full_2)


# Model summary
screenreg(list(
  Null = null_model_h1_full,
  Full_1 = model_h1_full_1,
  Full_2 = model_h1_full_2
))




# Save model results
# ------------------
# Extract data
model_data_h1 <- model.frame(model_h1_full_1)
model_data_h1$fitted_h1 <- fitted(model_h1_full_1) # full, random and fixed effect
model_data_h1$residuals_h1 <- resid(model_h1_full_1)

# Extract random effects
ranef_df_h1 <- coef(model_h1_full_1)$subreddit
ranef_df_h1$subreddit <- rownames(ranef_df_h1)
ranef_df_h1

# fixed, random and residuals effect prediction
model_data_h1 <- model_data_h1 %>%
  mutate(fit_fixed = predict(model_h1_full_1, re.form = NA),
         fit_rand = predict(model_h1_full_1, re.form = NULL),
         resid = resid(model_h1_full_1))


# save
write.csv(model_data_h1, "../../data/analysis_data/R_data/model_data_h1.csv", row.names = FALSE)
write.csv(ranef_df_h1, "../../data/analysis_data/R_data/ranef_df_h1.csv", row.names = FALSE)

# reimport
model_data_h1 <- read.csv("../../data/analysis_data/R_data/model_data_h1.csv")
ranef_df_h1 <- read.csv("../../data/analysis_data/R_data/ranef_df_h1.csv")




# --------------------------------------------------------------------------------
# Model Diagnostics & Visualization
# --------------------------------------------------------------------------------
set.seed(123)  # for reproducibility
model_data_h1_sample <- model_data_h1[sample(nrow(model_data_h1), 10000), ]  # sample 10,000 rows

title_size <- 12  

# Plot 1: Fitted vs. Residuals
p1_h1 <- ggplot(model_data_h1_sample, aes(x = fitted_h1, y = residuals_h1)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_bw(base_size = 12) +
  theme(aspect.ratio = 1, plot.title = element_text(size = title_size)) +
  labs(title = "Fitted vs. Residuals", x = "Fitted", y = "Residuals")

# Q-Q Plot
qq_data_h1 <- data.frame(
  theoretical = qqnorm(model_data_h1_sample$residuals_h1, plot.it = FALSE)$x,
  sample = qqnorm(model_data_h1_sample$residuals_h1, plot.it = FALSE)$y
)

p2_h1 <- ggplot(qq_data_h1, aes(sample = sample)) +
  stat_qq(alpha = 0.4) +
  stat_qq_line(color = "red") +
  theme_bw(base_size = 12) +
  theme(aspect.ratio = 1, plot.title = element_text(size = title_size)) +
  labs(title = "Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles")

# Histogram
p3_h1 <- ggplot(model_data_h1_sample, aes(x = residuals_h1)) +
  geom_histogram(binwidth = 0.25, fill = "#0072B2", color = "white", alpha = 0.8) +
  theme_bw(base_size = 12) +
  theme(aspect.ratio = 1, plot.title = element_text(size = title_size)) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency")


# Combine into one row
model_h1_diagnostic_plot = p1_h1 + p2_h1 + p3_h1 + plot_layout(ncol = 3)
model_h1_diagnostic_plot
# Save with custom size to preserve square shape (adjust height if necessary)
ggsave("H1_plots_OLS/H1_Diagnostic.pdf", model_h1_diagnostic_plot, width = 12, height = 4, dpi = 600)
ggsave("H1_plots_OLS/H1_Diagnostic.png", model_h1_diagnostic_plot, width = 12, height = 4, dpi = 300)




# Scatter plot with random effects and CI's
# -----------------------------------------

# Get subreddits
subreddits <- unique(doc_level_data$subreddit)

# Create grid: 100 points along submission_EMI range
submission_seq <- seq(
  min(doc_level_data$submission_EMI, na.rm = TRUE),
  max(doc_level_data$submission_EMI, na.rm = TRUE),
  length.out = 100
)

# Get fixed effects and random effects
fixef_beta <- fixef(model_h1_full_1)
re_subreddit <- ranef(model_h1_full_1)$subreddit  # intercept + slope per subreddit
vcov_beta <- vcov(model_h1_full_1)

# Prepare grid for each subreddit
pred_grid <- bind_rows(lapply(subreddits, function(sr) {
  # Build grid
  df <- tibble(
    submission_EMI = submission_seq,
    comment_type = factor("1", levels = c("0", "1")),  # or whatever baseline
    Mod_Dummy = factor("0", levels = c("0", "1")),
    subreddit = sr
  )
  
  # Add a fake row with alternate factor levels to avoid contrast errors
  df_fake <- df
  new_row <- df[1, ]
  
  if ("comment_type" %in% names(df)) {
    new_row$comment_type <- factor("0", levels = c("0", "1"))
  }
  
  if ("Mod_Dummy" %in% names(df)) {
    new_row$Mod_Dummy <- factor("1", levels = c("0", "1"))
  }
  
  df_fake <- bind_rows(df_fake, new_row)
  
  # Construct design matrix
  X <- model.matrix(~ submission_EMI + comment_type + Mod_Dummy, data = df_fake)
  X <- X[-nrow(X), ]  # Remove fake row
  
  # Add fixed + random effects
  ranef_intercept <- re_subreddit[sr, "(Intercept)"]
  ranef_slope <- re_subreddit[sr, "submission_EMI"]
  
  fixed_pred <- as.numeric(X %*% fixef_beta)
  df$pred <- fixed_pred + ranef_intercept + ranef_slope * df$submission_EMI
  
  # Standard error (only from fixed effects)
  pred_se <- sqrt(rowSums((X %*% vcov_beta) * X))
  df$ci_low <- df$pred - 1.96 * pred_se
  df$ci_high <- df$pred + 1.96 * pred_se
  
  df
}))

# fixed effect
# Create grid for fixed effect line (no subreddit-specific random effects)
fixed_grid <- tibble(
  submission_EMI = submission_seq,
  comment_type = factor("1", levels = c("0", "1")),  # baseline
  Mod_Dummy = factor("0", levels = c("0", "1"))      # baseline
)

# Design matrix for fixed effects only
X_fixed <- model.matrix(~ submission_EMI + comment_type + Mod_Dummy, data = fixed_grid)

# Predicted values (fixed effect only)
fixed_grid$pred <- as.numeric(X_fixed %*% fixef_beta)

# Confidence intervals (fixed effect uncertainty only)
fixed_grid$ci_low <- fixed_grid$pred - 1.96 * sqrt(rowSums((X_fixed %*% vcov_beta) * X_fixed))
fixed_grid$ci_high <- fixed_grid$pred + 1.96 * sqrt(rowSums((X_fixed %*% vcov_beta) * X_fixed))
fixed_grid$Effect <- "Fixed Effect"

obs_sample <- doc_level_data %>%
  sample_n(min(10000, n()))


# add slopes
subreddit_slopes <- sapply(subreddits, function(sr) {
  slope <- coef(model_h1_full_1)$subreddit[sr, "submission_EMI"]
  sprintf("%s (%.2f)", sr, slope)
})
names(subreddit_slopes) <- subreddits

pred_grid$subreddit_label <- subreddit_slopes[pred_grid$subreddit]
obs_sample$subreddit_label <- subreddit_slopes[obs_sample$subreddit]



p_random <- ggplot() +
  geom_point(data = obs_sample, aes(x = submission_EMI, y = text_EMI, color = subreddit_label), alpha = 0.2) +
  geom_line(data = pred_grid, aes(x = submission_EMI, y = pred, color = subreddit_label), size = 1) +
  geom_ribbon(data = pred_grid, aes(x = submission_EMI, ymin = ci_low, ymax = ci_high, fill = subreddit_label), alpha = 0.1, color = NA) +
  
  geom_line(data = fixed_grid, aes(x = submission_EMI, y = pred, linetype = Effect), 
            color = "black", size = 1.2) +
  
  geom_ribbon(data = fixed_grid, aes(x = submission_EMI, ymin = ci_low, ymax = ci_high), 
              fill = "grey40", alpha = 0.2, inherit.aes = FALSE) +
  theme_bw(base_size = 14) +
  labs(
    #title = "Predicted Text EMI by Submission EMI (per Subreddit)",
    x = "Submission EMI",
    y = "Text EMI",
    color = "Subreddit",
    fill = "Subreddit"
  ) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "right"
  )

p_random

ggsave("H1_plots_OLS/H1_bySubreddit_and_fixed.pdf", p_random, width = 10, height = 6, dpi = 600)

ggsave("H1_plots_OLS/H1_bySubreddit_and_fixed.png", p_random, width = 10, height = 6, dpi = 300)





