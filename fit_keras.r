# load libraries ####
invisible({
  pkgs = c('caret', 'dplyr', 'keras', 'kerastuneR')
  lapply(pkgs, library, character.only = TRUE)  
  inmet = read.csv('./source/inmet_list.csv', stringsAsFactors = FALSE)
})

# variables ####
weather_var = 'cdh'

# load data ####
raw_data = read.csv('./result/sample.csv')
str(raw_data)

# pre-process ####
# edit sample
raw_data$epw = inmet[raw_data$epw, weather_var]
# define qualitative and quantitative variables
qual_vars = c('hvac', 'afn', 'boundaries', 'envelope')
quant_vars = colnames(raw_data[-length(raw_data)])
quant_vars = quant_vars[!quant_vars %in% qual_vars]
# create dummy variables
raw_data[, qual_vars] = lapply(raw_data[, qual_vars], factor)
dummy_model = dummyVars(targ ~ ., data = raw_data)
dummy_data = data.frame(predict(dummy_model, newdata = raw_data))
dummy_data$targ = raw_data$targ
# split data into train and test sets
index = createDataPartition(dummy_data$targ, p = 0.8, list = FALSE)
train_data = dummy_data[index, ]
test_data = dummy_data[-index, ]
# pre-process model
pp_model = preProcess(train_data[, quant_vars], method = 'BoxCox')
train_data = predict(pp_model, train_data)
test_data = predict(pp_model, test_data)
# separate labels and targets
train_labels = as.matrix(select(train_data, -targ))
train_targets = as.matrix(select(train_data, targ))
test_labels = as.matrix(select(test_data, -targ))
test_targets = as.matrix(select(test_data, targ))

# # tune model ####
# hp = HyperParameters()
# hp$Int('num_layers', 2L, 4L)
# hp$Choice('learning_rate', c(1e-1, 1e-3))
# TuneModel = function(hp) {
#   model = keras_model_sequential() %>% 
#     layer_dense(units = 32, activation = 'relu', input_shape = ncol(train_labels))
#   for (i in 1:(hp$get('num_layers')) ) {
#     model = model %>%
#       layer_dense(32, activation='relu')
#     }
#   model = model %>%
#     layer_dense(units = 1, activation='relu')
#   model = model %>%
#     compile(loss = 'mse', metrics = 'mean_absolute_error',
#             optimizer = optimizer_adam(hp$get('learning_rate')))
#   return(model)
# }
# tuner = RandomSearch(hypermodel =  TuneModel,
#                      max_trials = 5,
#                      hyperparameters = hp,
#                      tune_new_entries = TRUE,
#                      objective = Objective('val_precision', 'min'),
#                      directory = '~/Desktop/test/',
#                      project_name = 'cbcs')
# tuner %>%
#   fit_tuner(x = train_labels,
#             y = train_targets,
#             epochs = 30,
#             validation_data = list(test_labels, test_targets))

# build model ####
# define kera's model class
model = keras_model_sequential()
# add layers to the model
model %>%
  layer_dense(units = 128, activation = 'relu', input_shape = ncol(train_labels)) %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 1)
# compile model
model %>%
  compile(loss = 'mse',
          optimizer = optimizer_adam(lr = 0.001),
          metrics = list('mean_absolute_error'))
# print model info
summary(model)
# fit model
history_fit = model %>%
  fit(train_labels, train_targets, epochs = 20,
      validation_split = 0.2, verbose = 1)
# evaluate model
model %>%
  evaluate(test_labels, test_targets)
