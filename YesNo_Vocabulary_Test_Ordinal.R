---
  title: ""
date: "`r Sys.Date()`"
output:
  rmdformats::readthedown:
  highlight: kate
---
  
  ```{r setup, include=FALSE}
## Global options
knitr::opts_chunk$set(cache = TRUE)
```

# Preparation

load packages

```{r}
library(brms)
library(tidyverse)
library(sjPlot)
library(performance)
library(rstan)
library(bayesplot)
library(loo)
library(bayestestR)
library(psych)
library(reshape2)
library(GGally)
library(ggridges)
library(DT)
library(knitr)
library(kableExtra)
library(reshape2)
library(gridExtra)
library(purrr)
```

```{r}
sessionInfo()
```

Set the theme

```{r}
theme_set(theme_bw())
set.seed(123)
rstan_options(auto_write = T)
options(mc.cores=parallel::detectCores())
```

load data

```{r}
dat <- read_csv("YesNo.csv")
```

convert to long format

```{r}
data_long <- dat %>% 
  pivot_longer(
    cols = -Participant,
    names_to = "Item",
    values_to = "Response") %>%
  mutate(
    Participant = factor(Participant))
```

make correct answers dataset

```{r}
item_list <- c("accumulation",	"accurate",	"adaptation",	"adjacent",	"allocation",	"alter",	"amendment",	"arbitrary",	"assembly",	"assurance",	"aware",	"behalf",	"capable",	"clarity",	"coincide",	"colleagues",	"comprise",	"concurrent",	"consultation",	"contradiction",	"conversely",	"crucial",	"denote",	"depression",	"deviation",	"discretion",	"discrimination",	"diversity",	"domain",	"duration",	"edition",	"eliminate",	"enable",	"energy",	"enforcement",	"enormous",	"equipment",	"equivalent",	"erosion",	"estate",	"ethical",	"exceed",	"explicit",	"exploitation",	"exposure",	"external",	"flexibility",	"forthcoming",	"foundation",	"gender",	"guidelines",	"hierarchical",	"incentive",	"incidence",	"inclination",	"incompatible",	"inherent",	"inhibition",	"initiatives",	"innovation",	"insights",	"inspection",	"instructions",	"integrity",	"intelligence",	"intensity",	"intervention",	"intrinsic",	"invoke",	"likewise",	"logic",	"manipulation",	"mediation",	"minimal",	"mode",	"nonetheless",	"nuclear",	"odd",	"ongoing",	"orientation",	"persistent",	"practitioners",	"protocol",	"publication",	"pursue",	"quotation",	"reluctant",	"restore",	"scenario",	"simulation",	"sphere",	"stability",	"straightforward",	"submitted",	"substitution",	"supplementary",	"symbolic",	"thereby",	"thesis",	"topic",	"transmission",	"undergo",	"unique",	"vehicle",	"visible",	"vision",	"visual",	"welfare",	"absolvention",	"ackery",	"almanical",	"amagran",	"amphlett",	"annobile",	"ashment",	"asslam",	"atribus",	"attard",	"bastionate",	"benevolate",	"berrow",	"cambule",	"captivise",	"carpin",	"causticate",	"charactal",	"coath",	"coppard",	"crucialate",	"cymballic",	"decorite",	"defunctionary",	"deliction",	"descript",	"doole",	"dring",	"dyment",	"ebullible",	"eluctant",	"expostulant",	"galpin",	"hapgood",	"hawther",	"hegedoxy",	"hermantic",	"hignall",	"hislop",	"hoult",	"interisation",	"jemmett",	"joice",	"keable",	"kearle",	"loveridge",	"majury",	"mastaphitis",	"neutration",	"nichee",	"oestrogeny",	"paralogue",	"perceptacle",	"peritonic",	"prowt",	"rainish",	"remonic",	"samphirate",	"savery",	"savourite",	"stattock",	"shuddery",	"tamnifest",	"tearle",	"tindle",	"transcendiary",	"vardy",	"viggers",	"warman",	"waygood",	"whitrow",	"wintle")
correct_answer_list <- c("Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"Yes",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No",	"No")
```

```{r}
correct_answers <- data.frame(
  Item = item_list,
  Correct_Answer = correct_answer_list)
```

combine data

```{r}
combined_data <- merge(data_long, correct_answers, by = "Item")
```

# Scoring Methods

- The Hit column is 1 when Response is "Yes" and Correct_Answer is "Yes"

- The Miss column is 1 when Response is "No" and Correct_Answer is "Yes"

- The False_Alarm column is 1 when Response is "Yes" and Correct_Answer is "No"

- The Correct_Rejection column is 1 when Response is "No" and Correct_Answer is "No"

- Scoring_1: 1 point if Hit is 1, 0 points for Correct_Rejection, Miss, and False_Alarm

- Scoring_2: 1 point if Hit is 1, 1 point if Correct_Rejection is 1, 0 points for Miss and False_Alarm

- Scoring_3: 2 points if Hit is 1, 1 point if Correct_Rejection is 1, 0 points for Miss and False_Alarm

- Scoring_4: 3 points if Hit is 1, 2 points if Correct_Rejection is 1, 1 point if Miss is 1, 0 points if False_Alarm is 1

```{r}
scoring_data <- combined_data %>%
  mutate(
    Hit = ifelse(Response == "Yes" & Correct_Answer == "Yes", 1, 0),
    Miss = ifelse(Response == "No" & Correct_Answer == "Yes", 1, 0),
    False_Alarm = ifelse(Response == "Yes" & Correct_Answer == "No", 1, 0),
    Correct_Rejection = ifelse(Response == "No" & Correct_Answer == "No", 1, 0),
    Scoring_1 = ifelse(Hit == 1, 1, 0),
    Scoring_2 = ifelse(Hit == 1 | Correct_Rejection == 1, 1, 0),
    Scoring_3 = ifelse(Hit == 1, 2, ifelse(Correct_Rejection == 1, 1, 0)),
    Scoring_4 = ifelse(Hit == 1, 2, ifelse(Correct_Rejection == 1, 1, ifelse(False_Alarm == 1, 0, 1))),
    Scoring_5 = ifelse(Hit == 1, 3, ifelse(Correct_Rejection == 1, 2, ifelse(False_Alarm == 1, 0, 1))))
```

convert to long format

```{r}
long_format_data <- scoring_data %>%
  pivot_longer(
    cols = c(Scoring_1, Scoring_2, Scoring_3, Scoring_4, Scoring_5),
    names_to = "Scoring_Method",
    values_to = "Score")

long_format_data <- mutate(
  long_format_data,
  Participant = factor(Participant),
  Scoring_Method = factor(Scoring_Method))
```

# Descriptive Statistics

```{r}
participant_scores <- long_format_data %>%
  group_by(Participant, Scoring_Method) %>%
  summarise(Total_Score = sum(Score, na.rm = T)) %>%
  pivot_wider(names_from = Scoring_Method, values_from = Total_Score)
```

```{r}
participant_scores %>%
  dplyr::select(Scoring_1, Scoring_2, Scoring_3, Scoring_4, Scoring_5) %>% 
  describe() %>% 
  kable(format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
```

```{r}
long_data <- participant_scores %>%
  pivot_longer(cols = starts_with("Scoring"), names_to = "Scoring", values_to = "Score")

ggplot(long_data, aes(x = Score, y = ..density..)) +
  geom_histogram(binwidth = 10, alpha = 0.6) +
  geom_density(linewidth = 1) +
  facet_wrap(~ Scoring, scales = "free") +
  labs(title = "Histograms with Density Plots of Scoring Methods", x = "Score", y = "Density")
```

make data separate

```{r}
scoring_1_data <- long_format_data %>%
  filter(Scoring_Method == "Scoring_1") %>% 
  filter(Correct_Answer == "Yes")

scoring_2_data <- long_format_data %>%
  filter(Scoring_Method == "Scoring_2") %>%
  mutate(Score = as.ordered(Score))

scoring_3_data <- long_format_data %>%
  filter(Scoring_Method == "Scoring_3") %>%
  mutate(Score = as.ordered(Score))

scoring_4_data <- long_format_data %>%
  filter(Scoring_Method == "Scoring_4") %>%
  mutate(Score = as.ordered(Score))

scoring_5_data <- long_format_data %>%
  filter(Scoring_Method == "Scoring_5") %>%
  mutate(Score = as.ordered(Score))
```

```{r}
datatable(
  scoring_1_data,
  filter='top', extensions = 'Scroller', class="compact",
  options = list(scrollY = 400, searching = T, paging = F,
                 columnDefs = list(list(className = 'dt-left', targets = "_all"))))
```

```{r}
datatable(
  scoring_2_data,
  filter='top', extensions = 'Scroller', class="compact",
  options = list(scrollY = 400, searching = T, paging = F,
                 columnDefs = list(list(className = 'dt-left', targets = "_all"))))
```

```{r}
datatable(
  scoring_3_data,
  filter='top', extensions = 'Scroller', class="compact",
  options = list(scrollY = 400, searching = T, paging = F,
                 columnDefs = list(list(className = 'dt-left', targets = "_all"))))
```

```{r}
datatable(
  scoring_4_data,
  filter='top', extensions = 'Scroller', class="compact",
  options = list(scrollY = 400, searching = T, paging = F,
                 columnDefs = list(list(className = 'dt-left', targets = "_all"))))
```

```{r}
datatable(
  scoring_5_data,
  filter='top', extensions = 'Scroller', class="compact",
  options = list(scrollY = 400, searching = T, paging = F,
                 columnDefs = list(list(className = 'dt-left', targets = "_all"))))
```

## Meara's (1996) Δm and Huibregtse et al.'s (2002) `I_{SDT}`

Below are the equations for `Δm` and `I_{SDT}`:
  
  $$
  \Delta m = \frac{h - f}{1 - f} - \frac{f}{h}
$$
  
  $$
  I_{SDT} = 1 - \frac{4h (1 - f) - 2 (h - f) (1 + h - f)}{4h (1 - f) - (h - f) (1 + h - f)}
$$
  
  prepare data

```{r}
previous_scoring_data <- combined_data %>%
  mutate(
    Hit = ifelse(Response == "Yes" & Correct_Answer == "Yes", 1, 0),
    Miss = ifelse(Response == "No" & Correct_Answer == "Yes", 1, 0),
    False_Alarm = ifelse(Response == "Yes" & Correct_Answer == "No", 1, 0),
    Correct_Rejection = ifelse(Response == "No" & Correct_Answer == "No", 1, 0))

participant_rates <- previous_scoring_data %>%
  group_by(Participant) %>%
  summarise(
    h = sum(Hit) / sum(Correct_Answer == "Yes"),
    f = sum(False_Alarm) / sum(Correct_Answer == "No"))

participant_rates <- participant_rates %>%
  mutate(
    delta_m = (h - f) / (1 - f) - (f / h),
    I_SDT = 1 - (4 * h * (1 - f) - 2 * (h - f) * (1 + h - f)) / (4 * h * (1 - f) - (h - f) * (1 + h - f)))
```

descriptive statistics

```{r}
describe(participant_rates) %>%
  kable(format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
```

## Correlation

```{r}
combined_data <- participant_rates %>%
  inner_join(participant_scores, by = "Participant")
```

```{r}
cor_matrix <- combined_data %>%
  select(h, f, delta_m, I_SDT, Scoring_1, Scoring_2, Scoring_3, Scoring_4, Scoring_5) %>%
  cor()
```

```{r}
sjPlot::tab_corr(
  cor_matrix, triangle = "lower", 
  title = "Correlation Matrix",
  var.labels = c("Hit Rate", "False Alarm Rate", "Delta_m", "I_SDT", "Scoring_1", "Scoring_2", "Scoring_3", "Scoring_4", "Scoring_5"))
```

```{r}
ggpairs(
  combined_data, columns = 2:10, 
  title = "Correlation Matrix",
  upper = list(continuous = "cor"),
  lower = list(continuous = "smooth"))
```

# Bayesian Estimation

## set prior distribution and model formula

The prior distribution is set as a normal distribution with a mean of 0 and a standard deviation of 3, based on Levy and Miskevy (2017) and Bürkner (2017). Additionally, separate prior distributions are set for Participant and Item, and partial pooling is employed.

```{r}
prior <- set_prior("normal(0, 3)", class = "sd", group = "Participant") +
  set_prior("normal(0, 3)", class = "sd", group = "Item")
```

## model specification

The model is specified as a multilevel logistic regression model with a random intercept for Participant and Item. The outcome variable is Score, which are dichotomous test data in the YesNo vocabulary test.

The cumulative model assumes

$$
  P(y = c) = F(\tau_c - \psi) - F(\tau_{c-1} - \psi)
$$
  
  where $F$ is the logistic function, $\tau_c$ is the threshold parameter, and $\psi$ is the person ability parameter. $F$ is called the cumulative distribution function (CDF) of continuous unbounded distribution, and $\tau_c$ has $C-1$ ordered thresholds.

The adjacent category model is called the partial credit model (PCM; Rasch, 1961). The PCM is a generalization of the 1PLM, and the PCM is represented as follows:
  
  $$
  P(y = c) = \frac{\exp\left(\sum_{j=1}^{c-1}(\psi - \tau_j)\right)}{\sum_{r=1}^{C}\exp\left(\sum_{j=1}^{r-1}(\psi - \tau_j)\right)}
$$
  
  # Model Fitting
  
  ```{r}
fit_scoring_3 <- brm(
  Score ~ 1 + (1 | Item) + (1 | Participant),
  data = scoring_3_data,
  family = cumulative(link = "logit"),
  prior = prior,
  save_pars = save_pars(all = TRUE),
  seed = 1234)

fit_scoring_4 <- brm(
  Score ~ 1 + (1 | Item) + (1 | Participant),
  data = scoring_4_data,
  family = cumulative(link = "logit"),
  prior = prior,
  save_pars = save_pars(all = TRUE),
  seed = 1234)

fit_scoring_5 <- brm(
  Score ~ 1 + (1 | Item) + (1 | Participant),
  data = scoring_5_data,
  family = cumulative(link = "logit"),
  prior = prior,
  save_pars = save_pars(all = TRUE),
  seed = 1234)
```

# Stan Code

```{r}
make_stancode(fit_scoring_3)
```

```{r}
make_stancode(fit_scoring_4)
```

```{r}
make_stancode(fit_scoring_5)
```

# Model Results

```{r}
summary(fit_scoring_3)
```

```{r}
summary(fit_scoring_4)
```

```{r}
summary(fit_scoring_5)
```

# Model Assumptions and Convergence

## trace plot

```{r}
plot(fit_scoring_3, combo = c("trace", "dens_overlay"), ask = F)
plot(fit_scoring_4, combo = c("trace", "dens_overlay"), ask = F)
plot(fit_scoring_5, combo = c("trace", "dens_overlay"), ask = F)
```

## posterior predictive checks

```{r}
pp_check(
  object = fit_scoring_3,
  ndraws = 1000,
  type = "stat",
  stat = "mean") +
  theme_test() +
  ylab("Frequency")

pp_check(
  object = fit_scoring_3,
  ndraws = 100) +
  theme_test() +
  ylab("Density")

pp_check(
  object = fit_scoring_4,
  ndraws = 1000,
  type = "stat",
  stat = "mean") +
  theme_test() +
  ylab("Frequency")

pp_check(
  object = fit_scoring_4,
  ndraws = 100) +
  theme_test() +
  ylab("Density")

pp_check(
  object = fit_scoring_5,
  ndraws = 1000,
  type = "stat",
  stat = "mean") +
  theme_test() +
  ylab("Frequency")

pp_check(
  object = fit_scoring_5,
  ndraws = 100) +
  theme_test() +
  ylab("Density")
```

# Model Comparison

The Leave-One-Out Cross-Validation (LOO-CV) method involves training a model on a dataset excluding one data point, then making a prediction on the excluded data point. This process is repeated for each data point in the dataset.

$$
  D = \{(x₁, y₁), \dots, (xₙ, yₙ)\} 
$$
  
  The LOO-CV estimate is expressed as follows:
  
  $$
  \text{LOO-CV} = -2 \sum_{i=1}^n \log p(y_i | x_i, D_{(-i)})
$$
  
  ```{r}
loo_cv_scoring_3 <- brms::loo(fit_scoring_3)
loo_cv_scoring_4 <- brms::loo(fit_scoring_4)
loo_cv_scoring_5 <- brms::loo(fit_scoring_5)
```

```{r}
print(loo_cv_scoring_3, simplify = FALSE)
print(loo_cv_scoring_4, simplify = FALSE)
print(loo_cv_scoring_5, simplify = FALSE)
```

The Widely Applicable Information Criterion (WAIC) is one of the information criteria used to evaluate the predictive performance of Bayesian models. It is asymptotically equivalent to Leave-One-Out Cross-Validation (LOO-CV) and is widely used due to its relatively easy computation.

WAIC is defined by the following formula:
  
  $$
  \text{WAIC} = -2(\text{lppd} - p_{\text{WAIC}})
$$
  
  - lppd is the log pointwise predictive density.

- p_WAIC is the effective number of parameters (a penalty term for model complexity).

```{r}
waic_3 <- brms::waic(fit_scoring_3)
waic_4 <- brms::waic(fit_scoring_4)
waic_5 <- brms::waic(fit_scoring_5)
```

```{r}
print(waic_3$estimates)
print(waic_4$estimates)
print(waic_5$estimates)
```

# Fit Statistics

extract item and person parameters

```{r}
ranef_scoring_3 <- ranef(fit_scoring_3)
ranef_scoring_4 <- ranef(fit_scoring_4)
ranef_scoring_5 <- ranef(fit_scoring_5)
```

item parameter

```{r}
item_pars_scoring_3 <- ranef_scoring_3$Item
item_pars_scoring_4 <- ranef_scoring_4$Item
item_pars_scoring_5 <- ranef_scoring_5$Item
```

person parameter

```{r}
person_pars_scoring_3 <- ranef_scoring_3$Participant
person_pars_scoring_4 <- ranef_scoring_4$Participant
person_pars_scoring_5 <- ranef_scoring_5$Participant

coef_scoring_3 <- coef(fit_scoring_3)$Participant
coef_scoring_4 <- coef(fit_scoring_4)$Participant
coef_scoring_5 <- coef(fit_scoring_5)$Participant
```

predict the probability of correct response

```{r}
predicted_probs_3 <- predict(fit_scoring_3, type = "response")
predicted_probs_4 <- predict(fit_scoring_4, type = "response")
predicted_probs_5 <- predict(fit_scoring_5, type = "response")
```

calculate residuals

```{r}
numeric_score_3 <- as.numeric(as.character(fit_scoring_3$data$Score))
numeric_score_4 <- as.numeric(as.character(fit_scoring_4$data$Score))
numeric_score_5 <- as.numeric(as.character(fit_scoring_5$data$Score))
```

```{r}
expected_category_score_3 <- apply(predicted_probs_3, 1, which.max) - 1
expected_category_score_4 <- apply(predicted_probs_4, 1, which.max) - 1
expected_category_score_5 <- apply(predicted_probs_5, 1, which.max) - 1
```

```{r}
residuals_3 <- numeric_score_3 - expected_category_score_3
residuals_4 <- numeric_score_4 - expected_category_score_4
residuals_5 <- numeric_score_5 - expected_category_score_5
```

```{r}
weighted_average_score_3 <- apply(predicted_probs_3, 1, function(x) sum((0:2) * x))
weighted_average_score_4 <- apply(predicted_probs_4, 1, function(x) sum((0:2) * x))
weighted_average_score_5 <- apply(predicted_probs_5, 1, function(x) sum((0:2) * x))
```

## Distribution of Infit Mean Square

outfit and infit statistics

$$
  Outfit=\frac{1}{N}\sum_{i=1}^{N} \frac{(U_i - P_{ij})^2}{P_{ij}(1 - P_{ij})}
$$
  
  $$
  Infit= \frac{\sum_{i=1}^{N} (U_{ij} - P_{ij})^2}{\sum_{i=1}^{N} P_{ij}(1 - P_{ij})}
$$
  
  ### Scoring_3
  
  ```{r}
N_scoring_3 <- nrow(predicted_probs_3)
actual_category_scoring_3 <- as.numeric(as.character(expected_category_score_3)) - 1

squared_residuals_scoring_3 <- residuals_3^2

p_ij_1_minus_p_ij_scoring_3 <- sapply(1:N_scoring_3, function(i) {
  p <- predicted_probs_3[i, actual_category_scoring_3[i] + 1]
  if (length(p) == 0 || is.na(p)) return(NA)
  p * (1 - p)
})

p_ij_1_minus_p_ij_scoring_3[p_ij_1_minus_p_ij_scoring_3 == 0 | !is.finite(p_ij_1_minus_p_ij_scoring_3)] <- NA
outfit_scoring_3 <- mean(squared_residuals_scoring_3 / p_ij_1_minus_p_ij_scoring_3, na.rm = TRUE)
infit_scoring_3 <- sum(squared_residuals_scoring_3, na.rm = TRUE) / sum(p_ij_1_minus_p_ij_scoring_3, na.rm = TRUE)

cat("Outfit Scoring_3:", outfit, "\n")
cat("Infit Scoring_3:", infit, "\n")

cat("Number of NA or Inf values in p_ij_1_minus_p_ij:", sum(is.na(p_ij_1_minus_p_ij_scoring_3)), "\n")
cat("Range of p_ij_1_minus_p_ij:", range(p_ij_1_minus_p_ij_scoring_3, na.rm = TRUE), "\n")
cat("Range of squared_residuals:", range(squared_residuals_scoring_3), "\n")
```

```{r}
participant_infit_scoring_3 <- tapply(1:N_scoring_3, scoring_3_data$Participant, function(idx) {
  sum(squared_residuals_scoring_3[idx], na.rm = TRUE) / sum(p_ij_1_minus_p_ij_scoring_3[idx], na.rm = TRUE)
})

item_infit_scoring_3 <- tapply(1:N_scoring_3, scoring_3_data$Item, function(idx) {
  sum(squared_residuals_scoring_3[idx], na.rm = TRUE) / sum(p_ij_1_minus_p_ij_scoring_3[idx], na.rm = TRUE)
})

item_fit_scoring_3 <- tapply(1:N_scoring_3, scoring_3_data$Item, function(idx) {
  infit <- sum(squared_residuals_scoring_3[idx], na.rm = TRUE) / sum(p_ij_1_minus_p_ij_scoring_3[idx], na.rm = TRUE)
  outfit <- mean(squared_residuals_scoring_3[idx] / p_ij_1_minus_p_ij_scoring_3[idx], na.rm = TRUE)
  c(infit = infit, outfit = outfit)
})

item_fit_df_scoring_3 <- do.call(rbind, item_fit_scoring_3) %>%
  as.data.frame() %>%
  rownames_to_column("Item")

ggplot(data.frame(infit = participant_infit_scoring_3), aes(x = infit)) +
  geom_histogram(binwidth = 0.05, fill = "grey", color = "black") +
  geom_vline(xintercept = c(0.5, 1.5), color = "red", linetype = "dashed") +
  labs(title = "Distribution of Infit Mean Square (Persons): Scoring_3",
       x = "Infit Mean Square", y = "Count")

ggplot(data.frame(infit = item_infit_scoring_3), aes(x = infit)) +
  geom_histogram(binwidth = 0.05, fill = "grey", color = "black") +
  geom_vline(xintercept = c(0.5, 1.5), color = "red", linetype = "dashed") +
  labs(title = "Distribution of Infit Mean Square (Items): Scoring_3",
       x = "Infit Mean Square", y = "Count")

ggplot(item_fit_df_scoring_3, aes(x = infit, y = outfit)) +
  geom_point() +
  geom_text(aes(label = Item), vjust = -0.5, hjust = 0.5, size = 3) +
  geom_hline(yintercept = c(0.5, 1.5), color = "red", linetype = "dashed") +
  geom_vline(xintercept = c(0.5, 1.5), color = "red", linetype = "dashed") +
  labs(title = "Infit vs Outfit Mean Square (Items): Scoring_3",
       x = "Infit Mean Square", y = "Outfit Mean Square")
```

```{r}
item_fit_sorted_scoring_3 <- item_fit_df_scoring_3 %>%
  arrange(desc(infit)) %>% 
  na.omit()

print("Top 10 items with highest Infit Mean Square:")
print(head(item_fit_sorted_scoring_3, 10))

high_infit_items_scoring_3 <- item_fit_sorted_scoring_3 %>%
  filter(infit > 10)

print("Items with Infit Mean Square greater than 10:")
print(high_infit_items_scoring_3)
```

### Scoring_4

```{r}
N_scoring_4 <- nrow(predicted_probs_4)
actual_category_scoring_4 <- as.numeric(as.character(expected_category_score_4)) - 1

squared_residuals_scoring_4 <- residuals_4^2

p_ij_1_minus_p_ij_scoring_4 <- sapply(1:N_scoring_4, function(i) {
  p <- predicted_probs_4[i, actual_category_scoring_4[i] + 1]
  if (length(p) == 0 || is.na(p)) return(NA)
  p * (1 - p)
})

p_ij_1_minus_p_ij_scoring_4[p_ij_1_minus_p_ij_scoring_4 == 0 | !is.finite(p_ij_1_minus_p_ij_scoring_4)] <- NA
outfit_scoring_4 <- mean(squared_residuals_scoring_4 / p_ij_1_minus_p_ij_scoring_4, na.rm = TRUE)
infit_scoring_4 <- sum(squared_residuals_scoring_4, na.rm = TRUE) / sum(p_ij_1_minus_p_ij_scoring_4, na.rm = TRUE)

cat("Outfit Scoring_4:", outfit, "\n")
cat("Infit Scoring_4:", infit, "\n")

cat("Number of NA or Inf values in p_ij_1_minus_p_ij:", sum(is.na(p_ij_1_minus_p_ij_scoring_4)), "\n")
cat("Range of p_ij_1_minus_p_ij:", range(p_ij_1_minus_p_ij_scoring_4, na.rm = TRUE), "\n")
cat("Range of squared_residuals:", range(squared_residuals_scoring_4), "\n")
```

```{r}
participant_infit_scoring_4 <- tapply(1:N_scoring_4, scoring_4_data$Participant, function(idx) {
  sum(squared_residuals_scoring_4[idx], na.rm = TRUE) / sum(p_ij_1_minus_p_ij_scoring_4[idx], na.rm = TRUE)
})

item_infit_scoring_4 <- tapply(1:N_scoring_4, scoring_4_data$Item, function(idx) {
  sum(squared_residuals_scoring_4[idx], na.rm = TRUE) / sum(p_ij_1_minus_p_ij_scoring_4[idx], na.rm = TRUE)
})

item_fit_scoring_4 <- tapply(1:N_scoring_4, scoring_4_data$Item, function(idx) {
  infit <- sum(squared_residuals_scoring_4[idx], na.rm = TRUE) / sum(p_ij_1_minus_p_ij_scoring_4[idx], na.rm = TRUE)
  outfit <- mean(squared_residuals_scoring_4[idx] / p_ij_1_minus_p_ij_scoring_4[idx], na.rm = TRUE)
  c(infit = infit, outfit = outfit)
})

item_fit_df_scoring_4 <- do.call(rbind, item_fit_scoring_4) %>%
  as.data.frame() %>%
  rownames_to_column("Item")

ggplot(data.frame(infit = participant_infit_scoring_4), aes(x = infit)) +
  geom_histogram(binwidth = 0.05, fill = "grey", color = "black") +
  geom_vline(xintercept = c(0.5, 1.5), color = "red", linetype = "dashed") +
  labs(title = "Distribution of Infit Mean Square (Persons): Scoring_4",
       x = "Infit Mean Square", y = "Count")

ggplot(data.frame(infit = item_infit_scoring_4), aes(x = infit)) +
  geom_histogram(binwidth = 0.05, fill = "grey", color = "black") +
  geom_vline(xintercept = c(0.5, 1.5), color = "red", linetype = "dashed") +
  labs(title = "Distribution of Infit Mean Square (Items): Scoring_4",
       x = "Infit Mean Square", y = "Count")

ggplot(item_fit_df_scoring_4, aes(x = infit, y = outfit)) +
  geom_point() +
  geom_text(aes(label = Item), vjust = -0.5, hjust = 0.5, size = 3) +
  geom_hline(yintercept = c(0.5, 1.5), color = "red", linetype = "dashed") +
  geom_vline(xintercept = c(0.5, 1.5), color = "red", linetype = "dashed") +
  labs(title = "Infit vs Outfit Mean Square (Items): Scoring_4",
       x = "Infit Mean Square", y = "Outfit Mean Square")
```

```{r}
item_fit_sorted_scoring_4 <- item_fit_df_scoring_4 %>%
  arrange(desc(infit)) %>% 
  na.omit()

print("Top 10 items with highest Infit Mean Square:")
print(head(item_fit_sorted_scoring_4, 10))

high_infit_items_scoring_4 <- item_fit_sorted_scoring_4 %>%
  filter(infit > 10)

print("Items with Infit Mean Square greater than 10:")
print(high_infit_items_scoring_4)
```

### Scoring_5

```{r}
N_scoring_5 <- nrow(predicted_probs_5)
actual_category_scoring_5 <- as.numeric(as.character(expected_category_score_5)) - 1

squared_residuals_scoring_5 <- residuals_5^2

p_ij_1_minus_p_ij_scoring_5 <- sapply(1:N_scoring_5, function(i) {
  p <- predicted_probs_5[i, actual_category_scoring_5[i] + 1]
  if (length(p) == 0 || is.na(p)) return(NA)
  p * (1 - p)
})

p_ij_1_minus_p_ij_scoring_5[p_ij_1_minus_p_ij_scoring_5 == 0 | !is.finite(p_ij_1_minus_p_ij_scoring_5)] <- NA
outfit_scoring_5 <- mean(squared_residuals_scoring_5 / p_ij_1_minus_p_ij_scoring_5, na.rm = TRUE)
infit_scoring_5 <- sum(squared_residuals_scoring_5, na.rm = TRUE) / sum(p_ij_1_minus_p_ij_scoring_5, na.rm = TRUE)

cat("Outfit Scoring_5:", outfit, "\n")
cat("Infit Scoring_5:", infit, "\n")

cat("Number of NA or Inf values in p_ij_1_minus_p_ij:", sum(is.na(p_ij_1_minus_p_ij_scoring_5)), "\n")
cat("Range of p_ij_1_minus_p_ij:", range(p_ij_1_minus_p_ij_scoring_5, na.rm = TRUE), "\n")
cat("Range of squared_residuals:", range(squared_residuals_scoring_5), "\n")
```

```{r}
participant_infit_scoring_5 <- tapply(1:N_scoring_5, scoring_5_data$Participant, function(idx) {
  sum(squared_residuals_scoring_5[idx], na.rm = TRUE) / sum(p_ij_1_minus_p_ij_scoring_5[idx], na.rm = TRUE)
})

item_infit_scoring_5 <- tapply(1:N_scoring_5, scoring_5_data$Item, function(idx) {
  sum(squared_residuals_scoring_5[idx], na.rm = TRUE) / sum(p_ij_1_minus_p_ij_scoring_5[idx], na.rm = TRUE)
})

item_fit_scoring_5 <- tapply(1:N_scoring_5, scoring_5_data$Item, function(idx) {
  infit <- sum(squared_residuals_scoring_5[idx], na.rm = TRUE) / sum(p_ij_1_minus_p_ij_scoring_5[idx], na.rm = TRUE)
  outfit <- mean(squared_residuals_scoring_5[idx] / p_ij_1_minus_p_ij_scoring_5[idx], na.rm = TRUE)
  c(infit = infit, outfit = outfit)
})

item_fit_df_scoring_5 <- do.call(rbind, item_fit_scoring_5) %>%
  as.data.frame() %>%
  rownames_to_column("Item")

ggplot(data.frame(infit = participant_infit_scoring_5), aes(x = infit)) +
  geom_histogram(binwidth = 0.05, fill = "grey", color = "black") +
  geom_vline(xintercept = c(0.5, 1.5), color = "red", linetype = "dashed") +
  labs(title = "Distribution of Infit Mean Square (Persons): Scoring_5",
       x = "Infit Mean Square", y = "Count")

ggplot(data.frame(infit = item_infit_scoring_5), aes(x = infit)) +
  geom_histogram(binwidth = 0.05, fill = "grey", color = "black") +
  geom_vline(xintercept = c(0.5, 1.5), color = "red", linetype = "dashed") +
  labs(title = "Distribution of Infit Mean Square (Items): Scoring_5",
       x = "Infit Mean Square", y = "Count")

ggplot(item_fit_df_scoring_5, aes(x = infit, y = outfit)) +
  geom_point() +
  geom_text(aes(label = Item), vjust = -0.5, hjust = 0.5, size = 3) +
  geom_hline(yintercept = c(0.5, 1.5), color = "red", linetype = "dashed") +
  geom_vline(xintercept = c(0.5, 1.5), color = "red", linetype = "dashed") +
  labs(title = "Infit vs Outfit Mean Square (Items): Scoring_5",
       x = "Infit Mean Square", y = "Outfit Mean Square")
```

```{r}
item_fit_sorted_scoring_5 <- item_fit_df_scoring_5 %>%
  arrange(desc(infit)) %>% 
  na.omit()

print("Top 10 items with highest Infit Mean Square:")
print(head(item_fit_sorted_scoring_5, 10))

high_infit_items_scoring_5 <- item_fit_sorted_scoring_5 %>%
  filter(infit > 10)

print("Items with Infit Mean Square greater than 10:")
print(high_infit_items_scoring_5)
```


## Person and Item Parameters

#### Person Parameters

Scoring_3

```{r}
ranef_scoring_3$Participant[, , "Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  arrange(Estimate) %>%
  mutate(rowname = seq_len(n())) %>%
  ggplot(aes(rowname, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Person Number (Sorted)")
```

Scoring_4

```{r}
ranef_scoring_4$Participant[, , "Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  arrange(Estimate) %>%
  mutate(rowname = seq_len(n())) %>%
  ggplot(aes(rowname, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Person Number (Sorted)")
```

Scoring_5

```{r}
ranef_scoring_5$Participant[, , "Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  arrange(Estimate) %>%
  mutate(rowname = seq_len(n())) %>%
  ggplot(aes(rowname, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Person Number (Sorted)")
```

#### Item Parameters

Scoring_3

```{r}
ranef_scoring_3$Item[, , "Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  arrange(Estimate) %>%
  mutate(rowname = seq_len(n())) %>%
  ggplot(aes(rowname, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Item Number (Sorted)")
```

Scoring_4

```{r}
ranef_scoring_4$Item[, , "Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  arrange(Estimate) %>%
  mutate(rowname = seq_len(n())) %>%
  ggplot(aes(rowname, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Item Number (Sorted)")
```

Scoring_5

```{r}
ranef_scoring_5$Item[, , "Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  arrange(Estimate) %>%
  mutate(rowname = seq_len(n())) %>%
  ggplot(aes(rowname, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Item Number (Sorted)")
```

```{r}
person_data <- function(ranef_data, scoring_method) {
  ranef_data$Participant[, , "Intercept"] %>%
    as_tibble() %>%
    rownames_to_column("Participant") %>%
    mutate(ScoringMethod = scoring_method)
}

item_data <- function(ranef_data, scoring_method) {
  ranef_data$Item[, , "Intercept"] %>%
    as_tibble() %>%
    rownames_to_column("Item") %>%
    mutate(ScoringMethod = scoring_method)
}

person_combined_data <- bind_rows(
  person_data(ranef_scoring_3, "Scoring_3"),
  person_data(ranef_scoring_4, "Scoring_4"),
  person_data(ranef_scoring_5, "Scoring_5"))

item_combined_data <- bind_rows(
  item_data(ranef_scoring_3, "Scoring_3"),
  item_data(ranef_scoring_4, "Scoring_4"),
  item_data(ranef_scoring_5, "Scoring_5"))

person_order <- person_combined_data %>%
  group_by(Participant) %>%
  summarize(MeanEstimate = mean(Estimate)) %>%
  arrange(MeanEstimate) %>%
  mutate(Order = row_number())

item_order <- item_combined_data %>%
  group_by(Item) %>%
  summarize(MeanEstimate = mean(Estimate)) %>%
  arrange(MeanEstimate) %>%
  mutate(Order = row_number())
```

### plot

```{r}
ggplot(person_combined_data %>% left_join(person_order, by = "Participant"),
       aes(x = Order, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = ScoringMethod)) +
  geom_pointrange(alpha = 0.7, position = position_dodge(width = 0.5)) +
  coord_flip() +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Person Number (Sorted by Mean Estimate)",
       y = "Estimate",
       color = "Scoring Method") +
  theme(legend.position = "bottom")

ggplot(item_combined_data %>% left_join(item_order, by = "Item"),
       aes(x = Order, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = ScoringMethod)) +
  geom_pointrange(alpha = 0.7, position = position_dodge(width = 0.5)) +
  coord_flip() +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Item Number (Sorted by Mean Estimate)",
       y = "Estimate",
       color = "Scoring Method") +
  theme(legend.position = "bottom")
```

### item information function

```{r}
calculate_item_info <- function(theta, item_params, thresholds) {
  n_items <- length(item_params)
  n_theta <- length(theta)
  n_cat <- ncol(thresholds) + 1
  
  info <- array(0, dim = c(n_items, n_theta))
  
  for (i in 1:n_items) {
    for (t in 1:n_theta) {
      probs <- category_probs(theta[t], item_params[i], thresholds[i,])
      d_probs <- category_probs_deriv(theta[t], item_params[i], thresholds[i,])
      
      info[i, t] <- sum((d_probs^2) / probs)
    }
  }
  
  return(info)
}

category_probs <- function(theta, a, b) {
  n_cat <- length(b) + 1
  z <- c(-Inf, b, Inf)
  p <- numeric(n_cat)
  
  for (k in 1:n_cat) {
    p[k] <- plogis(a * (theta - z[k])) - plogis(a * (theta - z[k+1]))
  }
  
  return(p)
}

category_probs_deriv <- function(theta, a, b) {
  n_cat <- length(b) + 1
  z <- c(-Inf, b, Inf)
  d_p <- numeric(n_cat)
  
  for (k in 1:n_cat) {
    d_p[k] <- a * (dlogis(a * (theta - z[k])) - dlogis(a * (theta - z[k+1])))
  }
  
  return(d_p)
}

calculate_item_info <- function(theta, item_params, thresholds) {
  n_items <- length(item_params)
  n_theta <- length(theta)
  
  info <- array(0, dim = c(n_items, n_theta))
  
  for (i in 1:n_items) {
    for (t in 1:n_theta) {
      probs <- category_probs(theta[t], item_params[i], thresholds[i,])
      d_probs <- category_probs_deriv(theta[t], item_params[i], thresholds[i,])
      
      info[i, t] <- sum((d_probs^2) / probs)
    }
  }
  
  return(info)
}

category_probs <- function(theta, a, b) {
  n_cat <- length(b) + 1
  z <- c(-Inf, b, Inf)
  p <- numeric(n_cat)
  
  for (k in 1:n_cat) {
    p[k] <- plogis(a * (theta - z[k])) - plogis(a * (theta - z[k+1]))
  }
  
  return(p)
}

category_probs_deriv <- function(theta, a, b) {
  n_cat <- length(b) + 1
  z <- c(-Inf, b, Inf)
  d_p <- numeric(n_cat)
  
  for (k in 1:n_cat) {
    d_p[k] <- a * (dlogis(a * (theta - z[k])) - dlogis(a * (theta - z[k+1])))
  }
  
  return(d_p)
}

extract_params <- function(fit) {
  fixef_output <- fixef(fit)
  ranef <- ranef(fit)$Item
  
  print("Fixed effects:")
  print(fixef_output)
  
  print("Random effects dimensions:")
  print(dim(ranef))
  
  fixed_thresholds <- fixef_output[, "Estimate"]
  
  print("Fixed thresholds:")
  print(fixed_thresholds)
  
  thresholds <- matrix(rep(fixed_thresholds, each = nrow(ranef)), 
                       nrow = nrow(ranef), 
                       ncol = length(fixed_thresholds))
  
  print("Thresholds dimensions:")
  print(dim(thresholds))
  
  print("First few rows of thresholds before adding random effects:")
  print(head(thresholds))
  
  # ランダム効果を加算（ランダム効果は1列目のみ使用）
  random_effects <- ranef[, "Estimate", "Intercept"]
  print("First few random effects:")
  print(head(random_effects))
  
  thresholds[,1] <- thresholds[,1] + random_effects
  
  print("First few rows of thresholds after adding random effects:")
  print(head(thresholds))
  
  item_params <- rep(1, nrow(ranef))
  
  return(list(thresholds = thresholds, item_params = item_params))
}
```

### Estimated Results

```{r}
theta <- seq(-6, 6, length.out = 100)
params_score_3 <- extract_params(fit_scoring_3)
item_info_score_3 <- calculate_item_info(theta, params_score_3$item_params, params_score_3$thresholds)
total_info_3 <- colSums(item_info_score_3, na.rm = TRUE)

params_score_4 <- extract_params(fit_scoring_4)
item_info_score_4 <- calculate_item_info(theta, params_score_4$item_params, params_score_4$thresholds)
total_info_4 <- colSums(item_info_score_4, na.rm = TRUE)

params_score_5 <- extract_params(fit_scoring_5)
item_info_score_5 <- calculate_item_info(theta, params_score_5$item_params, params_score_5$thresholds)
total_info_5 <- colSums(item_info_score_5, na.rm = TRUE)
```

### Plot

```{r}
item_info_df_scoring_3 <- as.data.frame(t(item_info_score_3))
item_info_df_scoring_4 <- as.data.frame(t(item_info_score_4))
item_info_df_scoring_5 <- as.data.frame(t(item_info_score_5))

colnames(item_info_df_scoring_3) <- paste0("Item_", 1:ncol(item_info_df_scoring_3))
colnames(item_info_df_scoring_4) <- paste0("Item_", 1:ncol(item_info_df_scoring_4))
colnames(item_info_df_scoring_5) <- paste0("Item_", 1:ncol(item_info_df_scoring_5))

item_info_df_scoring_3$theta <- theta
item_info_df_scoring_4$theta <- theta
item_info_df_scoring_5$theta <- theta

item_info_long_scoring_3 <- melt(item_info_df_scoring_3, id.vars = "theta", variable.name = "Item", value.name = "Information")
item_info_long_scoring_4 <- melt(item_info_df_scoring_4, id.vars = "theta", variable.name = "Item", value.name = "Information")
item_info_long_scoring_5 <- melt(item_info_df_scoring_5, id.vars = "theta", variable.name = "Item", value.name = "Information")

ggplot(item_info_long_scoring_3, aes(x = theta, y = Information, color = Item)) +
  geom_line() +
  labs(title = "Item Information Functions Scoring_3", x = "Theta", y = "Information") +
  theme(legend.position = "none")
ggplot(item_info_long_scoring_4, aes(x = theta, y = Information, color = Item)) +
  geom_line() +
  labs(title = "Item Information Functions Scoring_4", x = "Theta", y = "Information") +
  theme(legend.position = "none")
ggplot(item_info_long_scoring_5, aes(x = theta, y = Information, color = Item)) +
  geom_line() +
  labs(title = "Item Information Functions Scoring_5", x = "Theta", y = "Information") +
  theme(legend.position = "none")

total_info_scoring_3 <- rowSums(item_info_df_scoring_3[, -ncol(item_info_df_scoring_3)])
total_info_scoring_4 <- rowSums(item_info_df_scoring_4[, -ncol(item_info_df_scoring_4)])
total_info_scoring_5 <- rowSums(item_info_df_scoring_5[, -ncol(item_info_df_scoring_5)])

total_info_df_scoring_3 <- data.frame(theta = theta, Information = total_info_scoring_3)
total_info_df_scoring_4 <- data.frame(theta = theta, Information = total_info_scoring_4)
total_info_df_scoring_5 <- data.frame(theta = theta, Information = total_info_scoring_5)

ggplot(total_info_df_scoring_3, aes(x = theta, y = Information)) +
  geom_line() +
  labs(title = "Test Information Function Scoring_3", x = "Theta", y = "Total Information")
ggplot(total_info_df_scoring_4, aes(x = theta, y = Information)) +
  geom_line() +
  labs(title = "Test Information Function Scoring_4", x = "Theta", y = "Total Information")
ggplot(total_info_df_scoring_5, aes(x = theta, y = Information)) +
  geom_line() +
  labs(title = "Test Information Function Scoring_5", x = "Theta", y = "Total Information")
```

```{r}
calculate_info_with_cri <- function(fit, theta, n_samples = 100) {
  posterior_samples <- as.matrix(fit)
  n_items <- length(unique(fit$data$Item))
  
  info_samples <- array(0, dim = c(n_samples, length(theta)))
  
  for (i in 1:n_samples) {
    sample_idx <- sample(1:nrow(posterior_samples), 1)
    
    fixed_thresholds <- posterior_samples[sample_idx, grep("^b_Intercept", colnames(posterior_samples))]
    random_effects <- posterior_samples[sample_idx, grep("^r_Item", colnames(posterior_samples))]
    
    thresholds <- matrix(rep(fixed_thresholds, each = n_items), nrow = n_items, ncol = length(fixed_thresholds))
    thresholds[,1] <- thresholds[,1] + random_effects
    
    item_params <- rep(1, n_items)
    
    item_info <- calculate_item_info(theta, item_params, thresholds)
    info_samples[i,] <- colSums(item_info)
  }
  
  mean_info <- colMeans(info_samples)
  lower_cri <- apply(info_samples, 2, quantile, probs = 0.025)
  upper_cri <- apply(info_samples, 2, quantile, probs = 0.975)
  
  return(list(mean = mean_info, lower = lower_cri, upper = upper_cri))
}

theta <- seq(-6, 6, length.out = 100)

info_3 <- calculate_info_with_cri(fit_scoring_3, theta)
info_4 <- calculate_info_with_cri(fit_scoring_4, theta)
info_5 <- calculate_info_with_cri(fit_scoring_5, theta)

comparison_df <- data.frame(
  theta = rep(theta, 3),
  total_info = c(info_3$mean, info_4$mean, info_5$mean),
  lower = c(info_3$lower, info_4$lower, info_5$lower),
  upper = c(info_3$upper, info_4$upper, info_5$upper),
  model = rep(c("Model 3", "Model 4", "Model 5"), each = length(theta)))
```

```{r}
ggplot(comparison_df, aes(x = theta, y = total_info, color = model, fill = model)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(title = "Comparison of Test Information Functions with 95% CrI",
       x = "Theta",
       y = "Total Information",
       color = "Model",
       fill = "Model") +
  scale_color_manual(values = c("Model 3" = "blue", "Model 4" = "red", "Model 5" = "green")) +
  scale_fill_manual(values = c("Model 3" = "blue", "Model 4" = "red", "Model 5" = "green"))
```
