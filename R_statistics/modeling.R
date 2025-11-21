# Script to model the data

# Source data -------------------------------------------------------------

source("1_data_cleaning.r")

# Loading libraries -------------------------------------------------------

library(modelsummary)
library(gt)
library(lme4)

# Logistic regression -----------------------------------------------------

# Fit models

logit1 <- glm(<FORMULA>, data = data_modeling, family = "binomial")
logit2 <- glm(<FORMULA>, data = data_modeling, family = "binomial")
logit3 <- glm(<FORMULA>, data = data_modeling, family = "binomial")

logit4 <- glm(<FORMULA>, data = data_modeling, family = "binomial")
logit5 <- glm(<FORMULA>, data = data_modeling, family = "binomial")
logit6 <- glm(<FORMULA>, data = data_modeling, family = "binomial")

# Tabulate results

coef_map_vector <- c(<MAPPING>)
alignment <- "lcccccc"
notes_list <- list("<VALUE> coded as 1. Coefficients presented in log-odds. Standard errors in parentheses.")
gof_omit_string <- "RMSE|F|Std.Errors|R2 Marg.|R2 Cond."
stars_boolean <- TRUE
include_reference_boolean <- TRUE

table_logit <- modelsummary(
  list("Model 1" = logit1, "Model 2" = logit2, "Model 3" = logit3, "Model 4" = logit4, "Model 5" = logit5, "Model 6" = logit6),
  gof_omit = gof_omit_string,
  stars = stars_boolean,
  include_reference = include_reference_boolean,
  coef_map = coef_map_vector,
  align = alignment,
  notes = notes_list,
  output = "gt") %>% 
  tab_header(title = md("**Results from logistic regressions**")) %>% 
  tab_style(style = cell_text(style = "italic"), locations = cells_column_labels(columns = everything()))

gtsave(table_logit, filename = "table_logistic_regressions.html")
gtsave(table_logit, filename = "table_logistic_regressions.docx")

# The AIC, BIC, and log likelihood are pretty similar. Theoretical/substantive selection is appropriate
# <VAR> and <VAR> are positive and significant
# <VAR>, <VAR>, and <VAR> are positive and significant

# It doesn't make sense to check for linearity because all the predictors are categorical

# Check for multicollinearity

car::vif(logit2)
car::vif(logit3)
car::vif(logit5)
car::vif(logit6)
# No collinearity issues

# Residual diagnostics

plot(residuals(logit1, type = "deviance"))
plot(residuals(logit1, type = "pearson"))
plot(residuals(logit2, type = "deviance"))
plot(residuals(logit2, type = "pearson"))
plot(residuals(logit3, type = "deviance"))
plot(residuals(logit3, type = "pearson"))
plot(residuals(logit4, type = "deviance"))
plot(residuals(logit4, type = "pearson"))
plot(residuals(logit5, type = "deviance"))
plot(residuals(logit5, type = "pearson"))
plot(residuals(logit6, type = "deviance"))
plot(residuals(logit6, type = "pearson"))
# No major systematic problems
# Most residuals fall within +-2
# Some show 1-2 more extreme observations

# Influence measures are not going to add much in addition to just looking at counts (2_descriptive_statistics.R)
# Particularly, leverage is totally a function of the X matrix, which is just a function of the indicators

# There are only 3 observations where <VAR> is <VALUE> and <VAR> is <VALUE>
# For <VAR> == <VALUE>, 3 vs 12 is strong evidence
summary(logistf::logistf(<FORMULA>, data = <DATA_SUBSET>))
summary(logistf::logistf(<FORMULA>, data = <DATA_SUBSET>))
# results hold

# There are only 2 observations where <VAR> is "VALUE" and <VAR> is "VALUE"
# For <VAR> == "VALUE", 2 vs 10 is strong evidence
summary(logistf::logistf(<FORMULA>, data = <DATA_SUBSET>))
# <VAR> only marginally significant, otherwise results hold
summary(logistf::logistf(<FORMULA>, data = <DATA_SUBSET>))
# results hold
summary(logistf::logistf(<FORMULA>, data = <DATA_SUBSET>))
summary(logistf::logistf(<FORMULA>, data = <DATA_SUBSET>))
# results hold

# Overall, the results seem robust

# Logistic regression with cluster robust standard errors -----------------

table_logit_crse <- modelsummary(
  list("Model 1" = logit1, "Model 2" = logit2, "Model 3" = logit3, "Model 4" = logit4, "Model 5" = logit5, "Model 6" = logit6),
  # Gives the same numbers than vcov = map(list(logit1, logit2, logit3, logit4, logit5, logit6), ~ sandwich::vcovCL(.x, cluster = ~ participant_id))
  vcov = ~ participant_id,
  gof_omit = gof_omit_string,
  stars = stars_boolean,
  include_reference = include_reference_boolean,
  coef_map = coef_map_vector,
  align = alignment,
  notes = notes_list,
  output = "gt") %>% 
  tab_header(title = md("**Results from logistic regressions with cluster robust standard errors**")) %>% 
  tab_style(style = cell_text(style = "italic"), locations = cells_column_labels(columns = everything()))

gtsave(table_logit_crse, filename = "table_logistic_regressions_crse.html")
gtsave(table_logit_crse, filename = "table_logistic_regressions_crse.docx")

# <VAR> and <VAR> are positive and significant
# <VAR>, <VAR>, and <VAR> are positive and significant

# Results from logistic regressions seem robust

# Logistic mixed model ----------------------------------------------------

# Fit models

# https://www.stat.cmu.edu/~brian/valerie/617-2022/617-2021/0%20-%20lmer%20%26%20glmer%20performance/%282%29%20Mixed%20Effects%20Modeling%20Tips_%20Use%20a%20Fast%20Optimizer%2C%20but%20Perform%20Optimizer%20Checks%20_%20Steven%20V.%20Miller.pdf
# https://hlplab.wordpress.com/2014/06/24/more-on-old-and-new-lme4/
# recommend bobyqa for logistic mixed effects model

# + (1 | participant_id) -- allowing the intercept to vary across participants, meaning allowing each participant to have their own baseline probability for the outcome

glmm1 <- glmer(<FORMULA> + (1 | participant_id), data = data_modeling, family = binomial, control = glmerControl(optimizer = "bobyqa"))
glmm2 <- glmer(<FORMULA> + (1 | participant_id), data = data_modeling, family = binomial, control = glmerControl(optimizer = "bobyqa"))
glmm3 <- glmer(<FORMULA> + (1 | participant_id), data = data_modeling, family = binomial, control = glmerControl(optimizer = "bobyqa"))

glmm4 <- glmer(<FORMULA> + (1 | participant_id), data = data_modeling, family = binomial, control = glmerControl(optimizer = "bobyqa"))
glmm5 <- glmer(<FORMULA> + (1 | participant_id), data = data_modeling, family = binomial, control = glmerControl(optimizer = "bobyqa"))
glmm6 <- glmer(<FORMULA> + (1 | participant_id), data = data_modeling, family = binomial, control = glmerControl(optimizer = "bobyqa"))

# No convergence warnings

# Tabulate results

coef_map_vector_glmm <- c(coef_map_vector, "SD (Intercept participant_id)" = "Standard deviation of the random intercepts")

table_glmm <- modelsummary(
  list("Model 1" = glmm1, "Model 2" = glmm2, "Model 3" = glmm3, "Model 4" = glmm4, "Model 5" = glmm5, "Model 6" = glmm6),
  gof_omit = gof_omit_string,
  stars = stars_boolean,
  include_reference = include_reference_boolean,
  coef_map = coef_map_vector_glmm,
  align = alignment,
  notes = notes_list,
  output = "gt") %>% 
  tab_header(title = md("**Results from logistic mixed models**")) %>% 
  tab_style(style = cell_text(style = "italic"), locations = cells_column_labels(columns = everything()))

gtsave(table_glmm, filename = "table_logistic_mixed_models.html")
gtsave(table_glmm, filename = "table_logistic_mixed_models.docx")

# These models fit a bit better than the logistic regressions
# ICC between 0.3 and 0.4. That's not negligeable, suggesting it's important to use mixed models.
# SD (Intercept participant_id) is pretty high compared to the magnitude of the coefficients, suggesting that there is a fair amount of variation across subjects.

# <VAR> and <VAR> are positive and significant
# <VAR>, <VAR>, and <VAR> are positive and significant
# The coefficients are a bit different than the logistic regression, but not wildly different

# Check for singularity

isSingular(glmm1)
isSingular(glmm2)
isSingular(glmm3)
isSingular(glmm4)
isSingular(glmm5)
isSingular(glmm6)
# No issues with singularity

# Checking for overdispersion

performance::check_overdispersion(glmm1)
performance::check_overdispersion(glmm2)
performance::check_overdispersion(glmm3)
performance::check_overdispersion(glmm4)
performance::check_overdispersion(glmm5)
performance::check_overdispersion(glmm6)
# No issues with overdispersion

# Residual diagnostics

plot(DHARMa::simulateResiduals(glmm1))
plot(DHARMa::simulateResiduals(glmm2))
plot(DHARMa::simulateResiduals(glmm3))
plot(DHARMa::simulateResiduals(glmm4))
plot(DHARMa::simulateResiduals(glmm5))
plot(DHARMa::simulateResiduals(glmm6))
# No issues

# Check for multicollinearity

performance::check_collinearity(glmm2)
performance::check_collinearity(glmm3)
performance::check_collinearity(glmm5)
performance::check_collinearity(glmm6)
# No collinearity issues
