# load packages
library(tidyverse)
library(tidymodels)
library(modelr)
library(rsample)
library(yardstick)

# read data
url <- 'https://raw.githubusercontent.com/pstat197/pstat197a/main/materials/labs/lab4-logistic/data/biomarker_clean.csv'

s_star <- c("DERM", "RELT", "IgD", "PTN", "FSTL1")
biomarker <- read_csv(url) %>%
  # subset to proteins of interest and group
  select(group, any_of(s_star)) %>%
  # convert group (chr) to binary (lgl)
  mutate(class = (group == 'ASD')) %>%
  select(-group)

# for reproducibility
set.seed(102022)

# partition data
partitions <- biomarker %>%
  initial_split(prop = 0.8)

# examine
partitions


# training set
training(partitions) %>% head(4)
# testing set
testing(partitions) %>% head(4)


# fit glm
fit <- glm(class ~ ., 
           data = biomarker, 
           family = binomial(link = "logit"))
tidy(fit)

# compute predictions on the test set
testing(partitions) %>%
  add_predictions(fit)

# manually transform to probabilities
testing(partitions) %>%
  add_predictions(fit) %>%
  mutate(probs = 1/(1 + exp(-pred))) %>%
  select(class, pred, probs) %>%
  head(5)


# predict on scale of response
testing(partitions) %>%
  add_predictions(fit, type = 'response') %>%
  select(class, pred) %>%
  head(5)

# predict classes
testing(partitions) %>%
  add_predictions(fit, type = 'response') %>%
  mutate(pred.class = (pred > 0.5)) %>%
  select(class, pred, pred.class) %>%
  head(5)

# tabulate
testing(partitions) %>%
  add_predictions(fit, type = 'response') %>%
  mutate(pred.class = (pred > 0.5)) %>%
  select(class, pred.class) %>%
  table()
# store predictions as factors
pred_df <- testing(partitions) %>%
  add_predictions(fit, type = 'response') %>%
  mutate(pred.class = (pred > 0.5),
         group = factor(class, labels = c('TD', 'ASD')),
         pred.group = factor(pred.class, labels = c('TD', 'ASD'))) 

# check order of factor levels
pred_df %>% pull(group) %>% levels()

# compute specificity
pred_df %>%
  specificity(truth = group, 
              estimate = pred.group,
              event_level = 'second')

# sensitivity
pred_df %>%
  sensitivity(truth = group,
              estimate = pred.group,
              event_level = 'second')

# define panel (arguments must be yardstick metric function names)
panel_fn <- metric_set(sensitivity, specificity)

# compute
pred_df %>%
  panel_fn(truth = group,
           estimate = pred.group,
           event_level = 'second')

pred_df %>% conf_mat(truth = group, estimate = pred.group)

pred_df %>%
  roc_curve(truth = group, estimate = pred)

pred_df %>%
  roc_curve(truth = group, 
            estimate = pred,
            event_level = 'second') %>%
  ggplot(aes(y = sensitivity, x = 1 - specificity)) +
  geom_path() +
  geom_abline(slope = 1, intercept = 0)

pred_df %>% roc_auc(truth = group, 
                    estimate = pred,
                    event_level = 'second')

panel <- metric_set(roc_auc, accuracy) 

pred_df %>% panel(truth = group,
                  estimate = pred.group,
                  pred,
                  event_level = 'second')

# define some helper functions
fit_fn <- function(.df){
  glm(class ~ ., family = 'binomial', data = .df)
}

pred_fn <- function(.df, .mdl){
  .df %>% add_predictions(.mdl, type = 'response')
}

panel <- metric_set(sensitivity, specificity, accuracy, roc_auc)

eval_fn <- function(.df){
  .df %>%
    mutate(group = factor(class, labels = c('TD', 'ASD')),
           pred.group = factor(pred > 0.5, labels = c('TD', 'ASD'))) %>%
    panel(truth = group,
          estimate = pred.group,
          pred,
          event_level = 'second')
  
}


set.seed(101922)
n_splits <- 400
out <- tibble(partition = 1:n_splits,
              split = map(partition, ~ initial_split(biomarker, prop = 0.8)),
              train = map(split, training),
              test = map(split, testing),
              fit = map(train, fit_fn),
              pred_test = map2(test, fit, pred_fn),
              pred_train = map2(train, fit, pred_fn),
              eval_test = map(pred_test, eval_fn),
              eval_train = map(pred_train, eval_fn))

out %>% head(4)

test_accuracy <- out %>% 
  select(partition, contains('eval')) %>%
  unnest(eval_test) %>%
  select(partition, .metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)

train_accuracy <- out %>% 
  select(partition, contains('eval')) %>%
  unnest(eval_train) %>%
  select(partition, .metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)

test_accuracy %>% head(4)

train_accuracy %>% head(4)

train_summaries <- train_accuracy %>%
  rename(roc.auc = roc_auc) %>%
  select(-partition) %>%
  summarize_all(.funs = list(mean = mean, sd = sd)) %>%
  gather() %>%
  separate(key, into = c('metric', 'stat'), sep = '_') %>%
  spread(stat, value)

test_summaries <- test_accuracy %>%
  rename(roc.auc = roc_auc) %>%
  select(-partition) %>%
  summarize_all(.funs = list(mean = mean, sd = sd)) %>%
  gather() %>%
  separate(key, into = c('metric', 'stat'), sep = '_') %>%
  spread(stat, value)

left_join(train_summaries, 
          test_summaries,
          by = 'metric',
          suffix = c('.train', '.test')) %>%
  select(metric, contains('mean'), contains('sd')) %>%
  knitr::kable()



