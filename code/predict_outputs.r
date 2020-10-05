# load packages ####
pkgs = c('caret', 'dplyr', 'purrr', 'xgboost')
lapply(pkgs, library, character.only = TRUE)

# functions ####
# load predictive models
LoadModels = function(work_dir) {
  # work_dir: working directory
  # set working directory
  if (dir.exists(work_dir)) {
    setwd(work_dir)
  } else {
    stop(paste('O diretório de trabalho definido não existe.',
               'Confira se o nome do diretório foi inserido corretamente.'))
  }
  # load predictive models
  if (file.exists('models.rds')) {
    models <- readRDS('models.rds')
  } else {
    stop(paste('O caminho para o arquivo com os metamodelos está',
               'incorreto. Confira se os arquivos predict_outputs.r',
               'e models.rds encontram-se no mesmo diretório.'))
  }
  # return predictive models
  return(models)
}
# predict one output
PredOutput = function(targ, dummies, typo) {
  # targ: the target name ('phft', 'dif_phft', 'cgtt' or 'dif_cgtt)
  # dummies: the input data after the dummy transformation
  # typo: the typology ('uni' or 'multi')
  # associate the target and typology with the related model and execute the predictions
  out = capture.output({
    predictions = models %>%
      pluck(typo, targ) %>%
      predict(newdata = dummies)
  })
  # return the predictions
  return(predictions)
}
# predict dweling performance
PredPerfUH = function(inputs, data_path = 'data.csv') {
  # inputs: list of inputs/predictors
  # data_path: path to the outputs file (if NULL, the data will not be written)
  # fit inputs into a grid data frame if no table was provided
  if (is.list(inputs)) {
    inputs = expand.grid(inputs)
  } else if (file.exists(inputs)) {
    inputs = read.csv(inputs)
  } else {
    stop(paste('Confira se o argumento "inputs" foi definido corretamente.\n',
               ' São aceitos apenas: a) uma lista com as variáveis de entrada',
               'e os seus respectivos valores; b) um caminho válido para a',
               'tabela/csv contendo os inputs.'))
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
    stop(paste('Reveja as suas variáveis de entrada.',
               'Me parece que elas estão incorretas..!'))
  }
  # create dummy variables
  dummies = models[[c(typo, 'dummy_model')]] %>%
    predict(newdata = mutate(inputs, targ = NA)) %>%
    data.frame()
  # predict the outputs
  targs = c('phft', 'dif_phft', 'cgtt', 'dif_cgtt')
  predictions = targs %>%
    sapply(PredOutput, dummies, typo) %>%
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
  # write a file containing inputs and outputs
  if (!is.null(data_path)) {
    if (dir.exists(dirname(data_path))) {
      write.csv(data, data_path, row.names = FALSE)
      message(paste('A tabela com os resultados foi salva em:',
                    normalizePath(data_path)))
    } else {
      stop(paste('O diretório para pra salvar a tabela com os resultados',
                 'me parece incorreto.\n  Confira se o caminho inserido',
                 'no argumento "data_path" realmente existe.'))
    }
  } else {
    message('A tabela com os resultados não foi salva.')
  }
  # return the data
  return(data)
}

# application examples ####
# complete the code below in order to predict your data
work_dir = ''
models = LoadModels(work_dir)
inputs1 = ''
data_path1 = ''
data1 = PredPerfUH(inputs1, data_path1)
inputs2 = ''
data_path2 = ''
data2 = PredPerfUH(inputs2, data_path1)
