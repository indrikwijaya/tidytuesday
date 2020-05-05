# goal: models for NFL attendance dataset from tidytuesday

library(tidyverse)

attendance <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')

attendance_joined <- attendance %>% 
  left_join(standings,
            by = c("year", "team_name", "team"))

attendance_joined

# we have info on weekly attendance at NFL games, along w characteristics of team 
# records per season such as SRS (Simple Rating System), how many pts were scored
# for/against teams, whether a team made the playoffs, and more.
# We'll build a model for weekly attendance

attendance_joined %>% 
  filter(!is.na(weekly_attendance)) %>% 
  ggplot(aes(fct_reorder(team_name, weekly_attendance),
             weekly_attendance,
             fill = playoffs)) + 
  geom_boxplot(outlier.alpha = 0.5) +
  coord_flip() + 
  labs(fill = NULL, x = NULL, y = "Weekly NFL game attendance")

# Notice that for the 32 teams in the NFL, we have years they all did and didnt make
# the playoffs, which will be nice for modeling.

# how much does margin_of_victory, a measure of pts scored relative to pts allowed,
# measure the same thing as getting to the playoffs?
attendance_joined %>% 
  distinct(team_name, year, margin_of_victory, playoffs) %>% 
  ggplot(aes(margin_of_victory, fill = playoffs)) + 
  geom_histogram(position = 'identity', alpha = 0.7) + 
  labs(x = 'Margin of victory', y = 'Number of teams', fill = NULL)

# are there changes w the week of thea season?
attendance_joined %>% 
  mutate(week = factor(week)) %>% 
  ggplot(aes(week, weekly_attendance, fill = week)) + 
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.5) + 
  labs(x = "Week of NFL season", y = "Weekly NFL game attendance")

# now, create a dataset for modeling
# 1) remove the weeks that each team didnt play (weekly attendance = NA)
# 2) only keep columns for modeling that we want to use for modeling
# for eg, we keep margin_of_victory and strength_of_schedule, but not simple_rating
# which is the sum of those 1st two quantities

attendance_df <- attendance_joined %>% 
  filter(!is.na(weekly_attendance)) %>% 
  dplyr::select(weekly_attendance, team_name, year, week, margin_of_victory, strength_of_schedule,
         playoffs)
attendance_df

## Build simple models
library(tidymodels)

set.seed(1234)
attendance_split <- attendance_df %>% 
  initial_split(strata = playoffs)

nfl_train <- training(attendance_split)
nfl_test <- testing(attendance_split)

# one of the significant problems that tidymodels solves is how so many modeling packages
# and functions in R have diff inputs, calling sequences, and outputs.
lm_spec <- linear_reg() %>% 
  set_engine(engine = "lm")
lm_spec

lm_fit <- lm_spec %>% 
  fit(weekly_attendance ~ ., data = nfl_train)
lm_fit

rf_spec <- rand_forest(mode = 'regression') %>% 
  set_engine('ranger')
rf_spec

rf_fit <- rf_spec %>% 
  fit(weekly_attendance ~., data = nfl_train)
rf_fit

## Evaluate models
results_train <- lm_fit %>% 
  predict(new_data = nfl_train) %>% 
  mutate(truth = nfl_train$weekly_attendance, model = 'lm') %>% 
  bind_rows(rf_fit %>% 
              predict(new_data = nfl_train) %>% 
              mutate(truth = nfl_train$weekly_attendance,
                     model = 'rf'))

results_test <- lm_fit %>% 
  predict(new_data = nfl_test) %>% 
  mutate(truth = nfl_test$weekly_attendance, model = 'lm') %>% 
  bind_rows(rf_fit %>% 
              predict(new_data = nfl_test) %>% 
              mutate(truth = nfl_test$weekly_attendance, 
                     model = 'rf'))

results_train %>% 
  group_by(model) %>% 
  rmse(truth = truth, estimate = .pred)

results_test %>% 
  group_by(model) %>% 
  rmse(truth = truth, estimate = .pred)

# if we look at the training data, the rf model performed much better than the linear model;
# the rmse is much lower. yet, the same cannot be said for the testing data! 
# the metric for training and testing for the lm is about the same, meaning that we have not overfit
# for the rf model, the rmse is higher for the testing data than for the training data, by quite a lot
# out training data is not giving us a good idea of how our model is going to perform,
# and this powerful ML algo has overfit to this dataset

results_test %>% 
  mutate(train = "testing") %>% 
  bind_rows(results_train %>% 
              mutate(train = "training")) %>% 
  ggplot(aes(truth, .pred, color = model)) + 
  geom_abline(lty = 2, color = "gray80", size = 1.5) + 
  geom_point(alpha = 0.5) + 
  facet_wrap(~train) +
  labs(x = "Truth", y = "Predicted attendance", color = "Type of model")

# can resample the training set to produce an estimate of how the model will perform

set.seed(1234)
nfl_folds <- vfold_cv(nfl_train, strata = playoffs)

rf_res <- fit_resamples(weekly_attendance ~ .,
                        rf_spec,
                        nfl_folds,
                        control = control_resamples(save_pred = TRUE))