# Note: I wrote this code for a consult on 12/21/23.
# This code is not meant to be the final code for the analyses.
# It is meant to explain some options and show the client how to do certain analyses themselves.

# Loading libraries -------------------------------------------------------

library(simr)

# Setting parameters ------------------------------------------------------

# Seed for random number generation
# Simulation results depend on this
# It's good practice to re-run simulations with different seeds
seed <- 12823
set.seed(seed)

# Number of simulations/replicates to run
# Please note nsim should be higher. Default in simr is 1000 but increasing it improves precision
nsim <- 10

# Number of respondents per group (video/script)
# Total number of subjects will be twice this number since there are two groups
# You'll need to change this number to whatever you want to try
n_per_group <- 5

# Reading data ------------------------------------------------------------

data <- read.csv("<client_data>.csv")

# Cleaning data -----------------------------------------------------------

# Creating DV
data$incorrect <- ifelse(ifelse(data$testcondition == "A", TRUE, FALSE) == data$measurement, 0, 1)

# Converting DV and IVs to factor
data$incorrect <- factor(data$incorrect)
data$exposurecondition <- factor(data$exposurecondition)
data$testcondition <- factor(data$testcondition)
data$ResponseId <- factor(data$ResponseId)

# Estimating model for experiment 1 ---------------------------------------

# Generalized linear model
logit <- glm(incorrect ~ exposurecondition + testcondition + exposurecondition*testcondition, data = data, family = "binomial")
# Please note that data from within-subjects experiments violates the assumption of independence
# https://discovery.ucl.ac.uk/id/eprint/10107874/1/singmann_kellen-introduction-mixed-models%281%29.pdf
# Reviewers may note this point and object to analyzing the data with logistic regression
# Could potentially include ResponseId as a fixed effect, but length(unique(data$ResponseId)) = 193 participants is a lot
# Could potentially use cluster-robust standard errors

# Generalized mixed effects model
# https://stats.oarc.ucla.edu/r/dae/mixed-effects-logistic-regression/
logit_me <- glmer(
  incorrect ~ exposurecondition + testcondition + exposurecondition*testcondition + (1 | ResponseId),
  data = data,
  family = "binomial"
  )

# Comparing the two models

summary(logit) 
summary(logit_me)
# similar

logLik(logit)
logLik(logit_me)
# logit_me better

AIC(logit)
AIC(logit_me)
# logit_me better

BIC(logit)
BIC(logit_me)
# logit_me better

anova(logit_me, logit, test = "Chisq")
# https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faqhow-are-the-likelihood-ratio-wald-and-lagrange-multiplier-score-tests-different-andor-similar/
# https://stackoverflow.com/questions/49103318/anova-function-when-comparing-logistic-models-has-no-p-value-for-deviance
# equivalent to anova(logit_me, logit, test = "LRT")
# Prefers logit_me

# overall, the models seem similar. logit_me is probably best practices
# reviewers may object to logit. You could at least have logit_me as robustness check in appendix

# Generalized linear mixed effects model without interaction
logit_me_no_interaction <- glmer(
  incorrect ~ exposurecondition + testcondition + (1 | ResponseId),
  data = data,
  family = "binomial"
)

# Comparing with generalized linear mixed effets model with interaction
anova(logit_me, logit_me_no_interaction, test = "Chisq")
# equivalent to: doTest(logit_me, fcompare(~ exposurecondition + testcondition))
# https://cran.r-project.org/web/packages/simr/vignettes/examples.html
# prefers logit_me_no_interaction
# Could also visualize results to assess whether there's an interaction

# Power analysis for experiment 1 -----------------------------------------
# Please note that post-hoc power analyses are problematic
# https://www.tandfonline.com/doi/abs/10.1198/000313001300339897
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12504
# Also, post-hoc power analyses don't add any information beyond the reported p value
# pp. 17-18 of this article: https://online.ucpress.edu/collabra/article/8/1/33267/120491/Sample-Size-Justification
# See also: https://library.virginia.edu/data/articles/post-hoc-power-calculations-are-not-useful#:~:text=The%20problem%20with%20this%20idea,will%20always%20have%20low%20power.

# Please note that simr is designed for generalized linear mixed models
# https://cran.r-project.org/web/packages/simr/index.html 
# That said, generalized linear models are also supported
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12504

# Generalized linear mixed effects model

# Fixed effects for the model: fixef(logit_me)
# Please note: Main effects should not be tested when they appear in an interaction term
# https://cran.r-project.org/web/packages/simr/vignettes/examples.html
# Please note each of these considers one fixed effect at the time (not all or several at the same time)
powerSim(logit_me, fixed("exposureconditionI"), nsim = nsim, seed = seed)
powerSim(logit_me, fixed("testconditioni"), nsim = nsim, seed = seed)
powerSim(logit_me, fixed("exposureconditionI:testconditioni"), nsim = nsim, seed = seed)
# z test seems to work well
# https://cran.r-project.org/web/packages/simr/simr.pdf
# https://cran.r-project.org/web/packages/simr/vignettes/examples.html
# Not sure if set.seed() works with simr, so I opted for repeating each time, which can be error prone
# If there are warnings or errors, check:
# lastResult()$warnings
# lastResult()$errors

# powerCurve
# This can take a while to run, but you can parallelize the code
# https://pablobernabeu.github.io/2021/parallelizing-simr-powercurve/
pc_exposurecondition <- powerCurve(
  logit_me,
  fixed("exposureconditionI"),
  along = "ResponseId",
  # You can change this to whatever you want
  # The more levels, the longer the code takes to run
  # breaks can go as high as length(unique(data$ResponseId))
  breaks = c(1, 10, 193),
  seed = seed,
  nsim = nsim
  )
plot(pc_exposurecondition)

pc_testcondition <- powerCurve(
  logit_me,
  fixed("testconditioni"),
  along = "ResponseId",
  # You can change this to whatever you want
  # The more levels, the longer the code takes to run
  # breaks can go as high as length(unique(data$ResponseId))
  breaks = c(1, 75, 193),
  seed = seed,
  nsim = nsim
) 
plot(pc_testcondition)

pc_interaction <- powerCurve(
  logit_me,
  fixed("exposureconditionI:testconditioni"),
  along = "ResponseId",
  # You can change this to whatever you want
  # The more levels, the longer the code takes to run
  # breaks can go as high as length(unique(data$ResponseId))
  breaks = c(1, 150, 193),
  seed = seed,
  nsim = nsim
) 
plot(pc_interaction)

# You can do the same with the logit model here

# Power analysis for experiment 2 -----------------------------------------
# https://cran.r-project.org/web/packages/simr/vignettes/fromscratch.html
# https://lkumle.github.io/power_notebooks/Scenario3_notebook.html

# Creating data for explanatory variables
# Please note that there are multiple ways to implement this

# Creating data for group exposed to video
# I'm using the same names and values than in the data for clarity
X1 <- expand.grid(
  exposurecondition = c('A', 'I'),
  testcondition = c('A', 'i'),
  # New variable for group
  group = "video",
  # This is to simulate the fact that each subject has 16 exposures (8 accurate/8 inaccurate)
  # Based on what we discussed over email, sounds like this shouldn't matter
  stimuli = letters[1:4],
  ResponseId = 1:n_per_group
)

# Creating data for group exposed to script
# Same as above
X2 <- expand.grid(
  exposurecondition = c('A', 'I'),
  testcondition = c('A', 'i'),
  # Different group
  group = "script",
  stimuli = letters[1:4],
  ResponseId = 1:n_per_group
)

# Combining data for both groups into a single data frame
X <- rbind(X1, X2)

# Generalized mixed effects model

# Fixed effects
# You have to decide what these numbers are
# https://online.ucpress.edu/collabra/article/8/1/33267/120491/Sample-Size-Justification
# On effect size, see: https://www.frontiersin.org/articles/10.3389/fpsyg.2019.00813/full
fixed_effects <- c(-1.7, 1.4, 0.3, 0, 0.1)
# c(intercept, exposureconditionI, testconditioni, groupscript, exposureconditionI:testconditioni)
# check they're in the "right" order: fixef(logit_me_2)

# Variance per subject
# You also have to decide what this number is
subject_variance <- 0.3

# Creating an artificial lme4 object
logit_me_2 <- makeGlmer(
  # Similar to logit_me, but adding a fixed effect for group
  y ~ exposurecondition + testcondition + exposurecondition*testcondition + group + (1 | ResponseId),
  # do you want to interact group with factors?
  family = "binomial",
  fixef = fixed_effects,
  VarCorr = subject_variance,
  data = X
)
# You can check summary(logit_me_2)

# You can use powerSim and powerCurve in the same way as above

# Generalized linear model
# simr doesn't include a "makeGlm" function
# pwrss includes pwrss.z.logreg, but it seems to handle only one explanatory variable
# https://cran.r-project.org/web/packages/pwrss/index.html
# the same seems to be true for wp.logistic in WebPower
# https://cran.r-project.org/web/packages/WebPower/index.html
# same for powerLogisticBin in powerMediation
# https://cran.r-project.org/web/packages/powerMediation/index.html
# however, as it's always the case, you can do it from scratch
# https://stats.stackexchange.com/questions/604914/r-power-analysis-for-a-logistic-regression
# Again, there are many ways to implement this. This is just a quick one. The code can be improved
# You could also do a linear probability model: https://psycnet.apa.org/record/2020-71596-001

# Changing explanatory variables to numeric for calculations
X$exposurecondition <- ifelse(X$exposurecondition == "I", 1, 0)
X$testcondition <- ifelse(X$testcondition == "i", 1, 0)
X$group <- ifelse(X$group == "script", 1, 0)

# Setting coefficients (log odds ratios)
# https://en.wikipedia.org/wiki/Logistic_regression 
# https://stats.oarc.ucla.edu/r/dae/logit-regression/
# Again, these are numbers that you need to decide
# I'm just using the ones from model above for this script
betas <- c(-1.6, 1.3, 0.4, 0, 0.1)
# Please make sure they're in the correct order

# Vector to store results
results <- c()

# Linear predictions
linear_predictions <- 
  betas[1] + 
  X$exposurecondition * betas[2] + 
  X$testcondition * betas[3] + 
  X$group * betas[4] + 
  X$exposurecondition * X$testcondition * betas[5]
# do you want to interact group with factors?

# Predicted probabilities
probabilities <- exp(linear_predictions) / (1 + exp(linear_predictions))

# Simulation
for (replicate in 1:nsim) {
  
  # Predicted class
  class_predictions <- rbinom(length(probabilities), 1, probabilities)
  # You can see this changes every time:
  # print(mean(class_predictions))
  
  # Estimating model
  logit_2 <- glm(
    class_predictions ~ X$exposurecondition + X$testcondition + X$exposurecondition * X$testcondition + X$group,
    # do you want to interact group with factors?
    family = "binomial"
  )
  
  # Getting p-value(s)
  # You need to decide which one(s) you want
  # Here I'm just taking the one for exposurecondition as an example
  p_value <- summary(logit_2)$coefficients[2, 4]
  # You can see this changes every time:
  # print(p_value)
  
  # Storing whether result is significant or not
  # Picking 0.05 as significance level. You can change that
  results <- c(results, p_value < 0.05)
  
}

# Power
# If you re-run the for loop without re-running results <- c() you can incorrectly get power > 1
sum(results) / nsim

# You could write code from scratch to do an equivalent to powerCurve
