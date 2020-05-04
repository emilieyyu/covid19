library(lubridate)
library(tidyverse)
library(tidyquant)
library(ggplot2)
library(recipes)
library(timetk)
library(workflows)
library(parsnip)
library(yardstick)

setwd("#setyourpathtofiles")
total <- "canada_total.csv"
#fullpath <- file.path(filename)
tot <- read.csv(total)
tot$Date <- ymd(as.character(tot$Date))
tot %>%
  ggplot(aes(Date, Total))+
  geom_rect(xmin = as.numeric(ymd("2020-05-01")),
            xmax = as.numeric(ymd("2020-05-01")),
            ymin = 0, ymax = 8000)+
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  labs(title = "Covid19-Trend: Canada Total") +
  ylab("Total Cases (by Day)")+
  theme_tq()


filename <- "canada_diff.csv"
#fullpath <- file.path(filename)
dat <- read.csv(filename)
dat$Date <- ymd(as.character(dat$Date))

dat %>%
  ggplot(aes(Date, Diff))+
  geom_rect(xmin = as.numeric(ymd("2020-04-01")),
            xmax = as.numeric(ymd("2020-04-22")),
            ymin = 0, ymax = 8000,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("2020-03-10"), y = 2000,
           color = palette_light()[[1]], label = "Training Region") +
  annotate("text", x = ymd("2020-04-15"), y = 5000,
           color = palette_light()[[1]], label = "Test Region") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  labs(title = "Covid19-Trend: Canada") +
  ylab("New Cases Per Day")+
  theme_tq()

train <- dat %>% filter(Date < ymd("2020-04-01"))
test <- dat %>% filter(Date >= ymd("2020-04-01"))
recipe_spec_timeseries <- recipe(Diff ~ ., data = train) %>%
  step_timeseries_signature(Date) 
bake(prep(recipe_spec_timeseries), new_data = train)
recipe_spec_final <- recipe_spec_timeseries %>%
  step_rm(Date) %>%
  step_rm(contains("iso"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts")) %>%
  #step_normalize(contains("index.num"), Date_year) %>%
  step_dummy(contains("lbl"), one_hot = TRUE) 
bake(prep(recipe_spec_final), new_data = train)


model_spec_glmnet <- linear_reg(mode = "regression") %>%
  set_engine("lm")


workflow_glmnet <- workflow() %>%
  add_recipe(recipe_spec_final) %>%
  add_model(model_spec_glmnet)

workflow_glmnet

workflow_trained <- workflow_glmnet %>% fit(data = train)

prediction_tbl <- workflow_trained %>% 
  predict(test) %>%
  bind_cols(test) 

prediction_tbl

dat %>%
  ggplot(aes(Date, Diff))+
  geom_rect(xmin = as.numeric(ymd("2020-04-01")),
            xmax = as.numeric(ymd("2020-04-22")),
            ymin = 0, ymax = 8000,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("2020-03-10"), y = 2000,
           color = palette_light()[[1]], label = "Training Region") +
  annotate("text", x = ymd("2020-04-15"), y = 5000,
           color = palette_light()[[1]], label = "Test Region") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  labs(title = "Covid19-Trend: Canada Testing Trained Data") +
  ylab("New Cases Per Day")+
  theme_tq()+
  # Add predictions
  geom_point(aes(x = Date, y = .pred), data = prediction_tbl, 
             alpha = 0.5, color = palette_light()[[2]])

prediction_tbl %>% metrics(Diff, .pred)

prediction_tbl %>%
  ggplot(aes(x = Date, y = Diff - .pred)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth() +
  theme_tq() +
  labs(title = "Test Set: GLM Model Residuals", x = "Date", y="Actual - Predicted") +
  scale_y_continuous(limits = c(-2000, 2500))

##Future Data
idx <- dat %>% tk_index()
summary <- idx %>% tk_get_timeseries_summary()
summary[1:6]
summary[7:12]
idx_future <- idx %>% tk_make_future_timeseries(n_future = 60)
future_tbl <- tibble(Date = idx_future) 
future_tbl

future_predictions_tbl <- workflow_glmnet %>% 
  fit(data = dat) %>%
  predict(future_tbl) %>%
  bind_cols(future_tbl)

dat %>%
  ggplot(aes(Date, Diff))+
  geom_rect(xmin = as.numeric(ymd("2020-04-01")),
            xmax = as.numeric(ymd("2020-04-22")),
            ymin = 0, ymax = 6000,
            fill = palette_light()[[4]], alpha = 0.01) +
  geom_rect(xmin = as.numeric(ymd("2020-04-22")),
            xmax = as.numeric(ymd("2021-06-30")),
            ymin = 0, ymax = 6000,
            fill = palette_light()[[3]], alpha = 0.01) +
  annotate("text", x = ymd("2020-03-15"), y = 4000,
           color = palette_light()[[1]], label = "Training Region") +
  annotate("text", x = ymd("2020-04-15"), y = 4500,
           color = palette_light()[[1]], label = "Test Region") +
  annotate("text", x = ymd("2020-06-01"), y = 5000,
           color = palette_light()[[1]], label = "Forecast Region") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  # future data
  geom_point(aes(x = Date, y = .pred), data = future_predictions_tbl,
             alpha = 0.5, color = palette_light()[[2]]) +
  geom_smooth(aes(x = Date, y = .pred), data = future_predictions_tbl,
              method = 'loess') + 
  labs(title = "Covid19-Trend: Canada Future 2-Months Forecast", y="New Cases Per Day") +
  theme_tq()

test_resid_sd <- prediction_tbl %>%
  summarize(stdev = sd(Diff - .pred))

future_predictions_tbl <- future_predictions_tbl %>%
  mutate(
    lo.95 = .pred - 1.96 * test_resid_sd$stdev,
    lo.80 = .pred - 1.28 * test_resid_sd$stdev,
    hi.80 = .pred + 1.28 * test_resid_sd$stdev,
    hi.95 = .pred + 1.96 * test_resid_sd$stdev
  )

dat %>% 
  ggplot(aes(x = Date, y = Diff)) +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  geom_ribbon(aes(y = .pred, ymin = lo.95, ymax = hi.95), 
              data = future_predictions_tbl, 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(y = .pred, ymin = lo.80, ymax = hi.80, fill = key), 
              data = future_predictions_tbl,
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_point(aes(x = Date, y = .pred), data = future_predictions_tbl,
             alpha = 0.5, color = palette_light()[[2]]) +
  geom_smooth(aes(x = Date, y = .pred), data = future_predictions_tbl,
              method = 'loess', color = "cyan") + 
  labs(title = "Covid19 Trends: Canada 2-Months Forecast with Prediction Intervals", y="New Cases Per Day") +
  theme_tq()



