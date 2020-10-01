# setup environment ####
library(caret)
library(dplyr)
library(purrr)

# load predictive models ####
load('result/models.rdata')

# load('./result/models_uni_uh_')
# mls$uni$cgtt = models$xgbt
# load('./result/models_uni_uh_')
# mls$uni$dif_cgtt = models$xgbt
# load('./result/models_multi_uh_')
# mls$multi$cgtt = models$xgbt
# load('./result/models_multi_uh_')
# mls$multi$dif_cgtt = models$xgbt
# 
# models = mls
# rm(mls)
PredictData = function(targ, dummies, tipo, models) {
  out = capture.output({
    predictions = models %>%
      pluck(tipo, targ) %>%
      predict(newdata = dummies)
  })
  return(predictions)
}

PredPerfUH = function(inputs, tbl = NULL, models = models) {
  # inputs: list of inputs/predictors
  # tbl: table (csv) file with unlimited rows of predictors
  # models: compilation of models to predict phft and cgtt for single or multifamily dwelings
  
  # test
  df = read.csv('data_uni_uh.csv')
  inputs = list(
    area = 77.16,
    ratio = c(0.5, 1),
    pd = 2.5,
    azi = c(90, 180),
    comp = 'ref',
    vid = 'simples_fs87',
    abs = c(60, 80),
    paf = 30,
    fv = 0.45,
    ven = 'on',
    somb = 50,
    tbsm = 17.38078
  )
  tbl = NULL
  
  # fit inputs into a grid data frame if no table was provided
  if (is.null(tbl)) {
    inputs = expand.grid(inputs)
  }
  # check if the number of inputs is correct and define the typology
  ncols = ncol(inputs)
  if (ncols == 12) {
    message('Inputs referentes à tipologia unifamiliar.')
    tipo = 'uni'
  } else if (ncols == 14) {
    message('Inputs referentes à tipologia multifamiliar.')
    tipo = 'multi'
  } else {
    stop('Reveja seus inputs, pois eles parecem incorretos.')
  }
  # create dummy variables
  dummies = models[[c(tipo, 'dummy_model')]] %>%
    predict(newdata = mutate(inputs, targ = NA)) %>%
    data.frame()
  # predict the outputs
  targs = c('phft', 'dif_phft', 'cgtt', 'dif_cgtt')
  predictions = c('phft', 'dif_phft') %>%
    sapply(PredictData, dummies, tipo, models)
  # bind inputs and outputs to build the final data frame
  data = cbind(inputs, predictions)
}
