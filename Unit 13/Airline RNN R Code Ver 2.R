# airline data ... predict 3 years = 36 months

# Sunspot Vignette:  https://blogs.rstudio.com/tensorflow/posts/2018-06-25-sunspots-lstm/
# https://tensorflow.rstudio.com/keras/articles/guide_keras.html
# Good time series RNN blog post
# https://blogs.rstudio.com/tensorflow/posts/2017-12-20-time-series-forecasting-with-recurrent-neural-networks/


# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)

# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)

# Visualization
library(cowplot)

# Preprocessing
library(recipes)

# Sampling / Accuracy
library(rsample)
library(yardstick) 

# Modeling
library(keras)
library(tfruns)
library(tswge)

#install_keras()

# airline = as.numeric(sunspot.month)
data(airline)

#Backtesting
periods_train <- 12 * 8
#periods_val <- 12 * 50
periods_test  <- 12 * 8


#reshape
n_timesteps <- 36
n_predictions <- n_timesteps
batch_size <- 12

# functions used
build_matrix <- function(tseries, overall_timesteps) {
  t(sapply(1:(length(tseries) - overall_timesteps + 1), function(x) 
    tseries[x:(x + overall_timesteps - 1)]))
}

reshape_X_3d <- function(X) {
  dim(X) <- c(dim(X)[1], dim(X)[2], 1)
  X
}


# extract values from data frame
train_vals <- airline[1:periods_train]
# valid_vals <- airline[(periods_train+1):(periods_train+periods_val)]
# test_vals <- airline[(periods_train+1):length(airline)] # disjoint test and train
test_vals <- airline[(length(airline)-periods_test+1):length(airline)] # overlapping test and train


train_matrix <-
  build_matrix(train_vals, n_timesteps + n_predictions)
#valid_matrix <-
#  build_matrix(valid_vals_scaled, n_timesteps + n_predictions)
test_matrix <- build_matrix(test_vals, n_timesteps + n_predictions)


scaled = 1
#Scaled
if(scaled == 1)
{
  train_vals_scaled = (train_vals - mean(train_vals)) / sd(train_vals)
  trainMean = mean(train_vals)
  trainsd = sd(train_vals)
  #valid_vals_scaled = (valid_vals - mean(valid_vals)) / sd(valid_vals)
  test_vals_scaled = (test_vals - mean(test_vals)) / sd(test_vals)
  
  train_matrix <-
    build_matrix(train_vals_scaled, n_timesteps + n_predictions)
  #valid_matrix <-
  #  build_matrix(valid_vals_scaled, n_timesteps + n_predictions)
  test_matrix <- build_matrix(test_vals_scaled, n_timesteps + n_predictions)
}



if(scaled == 2)
{
  
  # extract values from data frame
  train_vals <- airline[1:periods_train]
  # valid_vals <- airline[(periods_train+1):(periods_train+periods_val)]
  # test_vals <- airline[(periods_train+1):length(airline)] # disjoint test and train
  test_vals <- airline[(length(airline)-periods_test+1):length(airline)] # overlapping test and train
  
  
  train_vals_scaled = (train_vals - mean(airline)) / sd(airline)
  trainMean = mean(airline)
  trainsd = sd(airline)
  #valid_vals_scaled = (valid_vals - mean(valid_vals)) / sd(valid_vals)
  test_vals_scaled = (test_vals - mean(airline)) / sd(airline)
  
  train_matrix <-
    build_matrix(train_vals_scaled, n_timesteps + n_predictions)
  #valid_matrix <-
  #  build_matrix(valid_vals_scaled, n_timesteps + n_predictions)
  test_matrix <- build_matrix(test_vals_scaled, n_timesteps + n_predictions)
}




# separate matrices into training and testing parts
# also, discard last batch if there are fewer than batch_size samples
# (a purely technical requirement) ... also  %/% is integer division 5 %/% 2 = 2
X_train <- train_matrix[, 1:n_timesteps]
y_train <- train_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_train <- X_train[1:(nrow(X_train) %/% batch_size * batch_size), ]
y_train <- y_train[1:(nrow(y_train) %/% batch_size * batch_size), ]

#X_valid <- valid_matrix[, 1:n_timesteps]
#y_valid <- valid_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
#X_valid <- X_valid[1:(nrow(X_valid) %/% batch_size * batch_size), ]
#y_valid <- y_valid[1:(nrow(y_valid) %/% batch_size * batch_size), ]

X_test <- test_matrix[, 1:n_timesteps]
y_test <- test_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_test <- X_test[1:(nrow(X_test) %/% batch_size * batch_size), ]
y_test <- y_test[1:(nrow(y_test) %/% batch_size * batch_size), ]

# add on the required third axis
X_train <- reshape_X_3d(X_train)
#X_valid <- reshape_X_3d(X_valid)
X_test <- reshape_X_3d(X_test)

y_train <- reshape_X_3d(y_train)
#y_valid <- reshape_X_3d(y_valid)
y_test <- reshape_X_3d(y_test)


#Flags for LSTM from Keras

epoch_size = 500

FLAGS <- flags(
  # There is a so-called "stateful LSTM" in Keras. While LSTM is stateful
  # per se, this adds a further tweak where the hidden states get 
  # initialized with values from the item at same position in the previous
  # batch. This is helpful just under specific circumstances, or if you want
  # to create an "infinite stream" of states, in which case you'd use 1 as 
  # the batch size. Below, we show how the code would have to be changed to
  # use this, but it won't be further discussed here.
  flag_boolean("stateful", FALSE),
  # Should we use several layers of LSTM?
  # Again, just included for completeness, it did not yield any superior 
  # performance on this task.
  # This will actually stack exactly one additional layer of LSTM units.
  flag_boolean("stack_layers", FALSE),
  # number of samples fed to the model in one go
  flag_integer("batch_size", batch_size),
  # size of the hidden state, equals size of predictions
  flag_integer("n_timesteps", 36),
  # how many epochs to train for
  flag_integer("n_epochs", epoch_size),
  # fraction of the units to drop for the linear transformation of the inputs
  flag_numeric("dropout", 0.2),
  # fraction of the units to drop for the linear transformation of the 
  # recurrent state
  flag_numeric("recurrent_dropout", 0.2),
  # loss function. Found to work better for this specific case than mean
  # squared error
  flag_string("loss", "logcosh"),
  # optimizer = stochastic gradient descent. Seemed to work better than adam 
  # or rmsprop here (as indicated by limited testing)
  flag_string("optimizer_type", "sgd"),
  # size of the LSTM layer
  flag_integer("n_units", 128),
  # learning rate
  flag_numeric("lr", 0.003),
  # momentum, an additional parameter to the SGD optimizer
  flag_numeric("momentum", 0.9),
  # parameter to the early stopping callback
  flag_integer("patience", 10)
)

# the number of predictions we'll make equals the length of the hidden state
n_predictions <- FLAGS$n_timesteps
# how many features = predictors we have
n_features <- 1
# just in case we wanted to try different optimizers, we could add here
optimizer <- switch(FLAGS$optimizer_type,
                    sgd = optimizer_sgd(lr = FLAGS$lr, 
                                        momentum = FLAGS$momentum)
)

# callbacks to be passed to the fit() function
# We just use one here: we may stop before n_epochs if the loss on the
# validation set does not decrease (by a configurable amount, over a 
# configurable time)
callbacks <- list(
  callback_early_stopping(patience = FLAGS$patience)
)


# create the model
model <- keras_model_sequential()

# add layers
# we have just two, the LSTM and the time_distributed 
model %>%
  layer_lstm(
    units = FLAGS$n_units, 
    # the first layer in a model needs to know the shape of the input data
    batch_input_shape  = c(FLAGS$batch_size, FLAGS$n_timesteps, n_features),
    dropout = FLAGS$dropout,
    recurrent_dropout = FLAGS$recurrent_dropout,
    # by default, an LSTM just returns the final state
    return_sequences = TRUE
  ) %>% time_distributed(layer_dense(units = 1))

model %>%
  compile(
    loss = FLAGS$loss,
    optimizer = optimizer,
    # in addition to the loss, Keras will inform us about current 
    # MSE while training
    metrics = list("mean_squared_error")
  )

history <- model %>% fit(
  x          = X_train,
  y          = y_train,
  validation_data = list(X_test, y_test),
  batch_size = FLAGS$batch_size,
  epochs     = FLAGS$n_epochs,
  #callbacks = callbacks
)


plot(history, metrics = "loss")

 #Predict Unstandardized Train
pred_train <- model %>%
  predict(X_train, batch_size = FLAGS$batch_size) %>%
  .[, , 1]

X_trainU = X_train[,1,1] * trainsd + trainMean
y_trainU = y_train[,1,1] * trainsd + trainMean
pred_trainU = pred_train*trainsd + trainMean

plot(y_trainU, type = "l",ylim = c(100,300))
lines(pred_trainU[,1], col = "red", lwd = 2)


ASE_TrainU = mean((y_trainU - pred_trainU)^2)
ASE_TrainU



# Predict Train Observations
pred_train <- model %>%
  predict(X_train, batch_size = FLAGS$batch_size) %>%
  .[, , 1]

# Fit of Train Data
plot(seq(1,36,1),y_train[1,,1], type = "l", xlim = c(1,36),ylim = c(min(y_train[1,,1]),max(y_train[1,,1])))
lines(seq(1,36,1),pred_train[1,], type = "l", col = "red", lwd = 2)

ASE_Train = mean((y_train[1,,1] - pred_train[1,])^2)
ASE_Train



#Test
# Predict Test Observations
pred_test <- model %>%
  predict(X_test, batch_size = FLAGS$batch_size) %>%
  .[, , 1]

# Fit of Test Data
plot(seq(1,36,1),y_test[24,,1], type = "l", xlim = c(1,36),ylim = c(min(y_test[24,,1]),max(y_test[24,,1])))
lines(seq(1,36,1),pred_test[24,], type = "l", col = "red", lwd = 2)

dfAir = data.frame(Months = seq(1,36,1), Passengers_Scaled = y_test[24,,1], Type = "Actual")
dfFore = data.frame(Months = seq(1,36,1), Passengers_Scaled = pred_test[24,], Type = "Forecast")
dfBoth = rbind(dfAir, dfFore)
dfBoth %>% ggplot(aes(x = Months, y= Passengers_Scaled, color = Type)) + geom_line()


ASE_Test = mean((y_test[24,,1] - pred_test[24,])^2)
ASE_Test


#Predict Unstandardized Test
pred_train <- model %>%
  predict(X_test, batch_size = FLAGS$batch_size) %>%
  .[, , 1]

X_testU = X_test[,1,1] * trainsd + trainMean
y_testU = y_test[,1,1] * trainsd + trainMean
pred_testU = pred_test*trainsd + trainMean

plot(y_testU, type = "l",ylim = c(100,300))
lines(pred_testU[,1], col = "red", lwd = 2)


ASE_TestU = mean((y_testU - pred_testU)^2)
ASE_TestU



#### Forecasting on the same plot

airlineS = (airline - mean(airline))/sd(airline)

dfAirline_Actual = data.frame(Passengers = airlineS, Months = seq(1,144,1), Type = "Actual")
dfAirline_Forecast = data.frame(Passengers = pred_test[24,], Months = seq(109,144,1), Type = "Forecast")
dfBoth = rbind(dfAirline_Actual,dfAirline_Forecast)
dfBoth %>% ggplot(aes(x = Months, y = Passengers, color = Type)) + geom_line()

