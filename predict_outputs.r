# setup environment ####
# load libraries
library(caret)
library(dplyr)
library(purrr)
# load predictive models
models = readRDS('./result/models.rds')

# define functions ####
# predict one output
PredOutput = function(targ, dummies, typo, mls) {
  # targ: the target name ('phft', 'dif_phft', 'cgtt' or 'dif_cgtt)
  # dummies: the input data after the dummy transformation
  # typo: the typology ('uni' or 'multi')
  # mls: list of predictive
  # associate the target and typology with the related model and execute the predictions
  out = capture.output({
    predictions = mls %>%
      pluck(typo, targ) %>%
      predict(newdata = dummies)
  })
  # return the predictions
  return(predictions)
}
# predict dweling performance
PredPerfUH = function(inputs, mls = models) {
  # inputs: list of inputs/predictors
  # tbl: table (csv) file with unlimited rows of predictors
  # mls: list of predictive models
  # fit inputs into a grid data frame if no table was provided
  if (is.list(inputs)) {
    inputs = expand.grid(inputs)
  } else if (is.character(inputs)) {
    inputs = read.csv(inputs)
  } else {
    stop(paste0('Confira se o argumento "inputs" foi associado a uma lista com as ',
                'variáveis de entrada e os seus valores ou o caminho da tabela/csv ',
                'na sua máquina.  Qualquer valor diferente disso não é aceito.'))
  }
  # check if the number of inputs is correct and define the typology
  ncols = ncol(inputs)
  if (ncols == 12) {
    message('Variáveis de entrada referentes à tipologia unifamiliar.')
    typo = 'uni'
  } else if (ncols == 14) {
    message('Variáveis de entrada referentes à tipologia multifamiliar.')
    typo = 'multi'
  } else {
    stop('Reveja as suas variáveis de entrada. Me parece que elas estão incorretas..!')
  }
  # create dummy variables
  dummies = mls[[c(typo, 'dummy_model')]] %>%
    predict(newdata = mutate(inputs, targ = NA)) %>%
    data.frame()
  # predict the outputs
  targs = c('phft', 'dif_phft', 'cgtt', 'dif_cgtt')
  predictions = targs %>%
    sapply(PredOutput, dummies, typo, mls) %>%
    as.data.frame() %>%
    round(1)
  # pos-process
  if (nrow(inputs) == 1) {
    # adjust the data into proper dataframe
    data = predictions %>%
      rename(prediction = '.') %>%
      t() %>%
      as.data.frame()
  } else {
    # bind inputs and outputs to build the final data frame
    data = cbind(inputs, predictions)
  }
  # return the data
  return(data)
}

# application examples ####
  # complete the code below in order to predict your input data
inputs = list()
inputs = ''
data = PredPerfUH()
data_path
write.csv(data, data_path)
