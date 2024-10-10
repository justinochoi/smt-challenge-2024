library(tidymodels) 
library(vip)
library(janitor)
tidymodels_prefer()

# Change response variable to a factor 
final_df$ss_got_ball <- as.factor(final_df$ss_got_ball) 

# Split the data into training and testing sets 
set.seed(131)
split <- initial_split(final_df, prop = 0.75, strata = ss_got_ball) 
train <- training(split) 
test <- testing(split) 

# Five repeats of ten-fold cross-validation 
set.seed(132) 
folds <- vfold_cv(train, v = 10, repeats = 5, strata = ss_got_ball) 

# We'll build two models and compare them  
# First we need create some model recipes 
standard_rec <- 
  recipe(ss_got_ball ~ euclid_dist + angle_between + distance_covered + similarity, 
         data = train) 

complex_rec <- standard_rec %>% 
  step_normalize(all_predictors()) %>% 
  step_interact(~ all_predictors():all_predictors()) 

# Provide specifications for each model type 
logistic_spec <- 
  logistic_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet") 
  
rf_spec <- 
  rand_forest(min_n = tune(), mtry = tune(), trees = 500) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification") 

# Create a workflow for each model 
logistic_wflow <- workflow_set(
  preproc = list(complex_rec), models = list(logistic_spec)) 
    
rf_wflow <- workflow_set(
  preproc = list(standard_rec), models = list(rf_spec)) 

all_workflows <- bind_rows(logistic_wflow, rf_wflow) %>% 
  mutate(wflow_id = gsub("(recipe_)", "", wflow_id)) 

# Searching for best tuning parameters to optimize log-loss 
grid_results <- all_workflows %>% 
  workflow_map(
    seed = 133, resamples = folds, grid = 20, 
    control = control_grid(
      save_pred = T, parallel_over = "everything", save_workflow = T), 
    metrics = metric_set(mn_log_loss), 
    verbose = T
  ) 

# Let's look at the tuning results and visualize them 
grid_results %>% rank_results() 
autoplot(grid_results, select_best = T) 
# Confidence intervals overlap, but logistic regression is clearly better  
# Should be less prone to over-fitting, too 

# Finalizing the parameters  
logistic_params <- grid_results %>% 
  extract_workflow_set_result("logistic_reg") %>% 
  select_best(metric = "mn_log_loss") 

rf_params <- grid_results %>% 
  extract_workflow_set_result("rand_forest") %>% 
  select_best(metric = "mn_log_loss") 

# Finalizing the models 
logistic_results <- grid_results %>% 
  extract_workflow("logistic_reg") %>% 
  finalize_workflow(logistic_params) %>% 
  last_fit(split, metrics = metric_set(mn_log_loss))  

rf_results <- grid_results %>%
  extract_workflow("rand_forest") %>% 
  finalize_workflow(rf_params) %>% 
  last_fit(split, metrics = metric_set(mn_log_loss)) 

collect_metrics(logistic_results) # 0.162 log-loss
collect_metrics(rf_results) # 0.190 log-loss
# As expected, a tree-based model does much worse on testing data 

# What variables contribute how much? 
logistic_coeffs <- 
  tidy(extract_fit_parsnip(logistic_results)) %>% 
  filter(abs(estimate) > 0) %>% 
  select(term, estimate) 

# vip: variable importance 
rf_vip <-
  extract_fit_parsnip(rf_results) %>% 
  vip() 

rf_vip$data$Variable <- 
  c("Angle Between", "Similarity", "Distance Covered", "Initial Distance")

rf_vip$data %>% 
  ggplot(aes(x=Importance, 
             y=reorder(Variable, Importance), 
             fill=Variable)) + 
  geom_bar(stat = "identity") + 
  labs(
    y = "Variable", 
    title = "The Relative Importance of Each Variable", 
    subtitle = "Importance is a measure of accuracy gained", 
    fill = NA
  ) + 
  theme_bw() + 
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold")) 
# Both models think that the angle between ball and player is crucial 

# Let's run the model on our entire data and save the results 
logistic_preds <- logistic_results %>% 
  extract_workflow() %>% 
  predict(new_data = final_df, type = "prob") %>% 
  select(.pred_Yes) %>% 
  clean_names() %>% 
  mutate(pred_yes = round(pred_yes, 3)) 

final_df$est_prob <- logistic_preds$pred_yes 

