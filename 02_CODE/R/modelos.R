modelo_xgboost <- list(
  label = "XGBoost",
  library = "xgboost",
  type = "Regression",
  parameters = data.frame(
    parameter = c("nrounds", "max_depth", "eta", "min_child_weight"),
    class = rep("numeric", 4),
    label = c("Iteraciones", "Profundidad", "Tasa de aprendizaje", "Peso minimo")
  ),
  grid = function(x, y, len = NULL, search = "grid") {
    expand.grid(nrounds = c(100, 300), max_depth = 2:4, eta = c(0.05, 0.1, 0.3), min_child_weight = 1)
  },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    parametros <- list(
      objective = "reg:squarederror",
      max_depth = param$max_depth,
      learning_rate = param$eta,
      min_child_weight = param$min_child_weight,
      subsample = 0.8,
      colsample_bytree = 0.8,
      nthread = 1
    )
    matriz <- xgboost::xgb.DMatrix(as.matrix(x), label = y)
    list(
      booster = xgboost::xgb.train(params = parametros, data = matriz, nrounds = param$nrounds, verbose = 0),
      variables = colnames(x)
    )
  },
  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    predict(modelFit$booster, xgboost::xgb.DMatrix(as.matrix(newdata)))
  },
  prob = NULL,
  varImp = function(object, ...) {
    ganancia <- setNames(rep(0, length(object$variables)), object$variables)
    importancia <- xgboost::xgb.importance(model = object$booster)
    ganancia[importancia$Feature] <- importancia$Gain
    data.frame(Overall = ganancia, row.names = names(ganancia))
  },
  sort = function(x) x[order(x$nrounds, x$max_depth, x$eta), ]
)

modelo_adaboost <- list(
  label = "AdaBoost.R2",
  library = "rpart",
  type = "Regression",
  parameters = data.frame(
    parameter = c("mfinal", "maxdepth"),
    class = c("numeric", "numeric"),
    label = c("Arboles", "Profundidad")
  ),
  grid = function(x, y, len = NULL, search = "grid") {
    expand.grid(mfinal = c(50, 100), maxdepth = 2:4)
  },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    datos <- data.frame(x, .y = y)
    n <- nrow(datos)
    pesos <- rep(1 / n, n)
    control <- rpart::rpart.control(maxdepth = param$maxdepth, minsplit = 4, cp = 0, xval = 0)
    arboles <- list()
    confianzas <- numeric()
    for (m in seq_len(param$mfinal)) {
      muestra <- sample.int(n, n, replace = TRUE, prob = pesos)
      arbol <- rpart::rpart(.y ~ ., data = datos[muestra, ], control = control)
      error <- abs(predict(arbol, datos) - y)
      perdida <- if (max(error) > 0) error / max(error) else error
      error_medio <- sum(pesos * perdida)
      if (error_medio >= 0.5) break
      beta <- max(error_medio / (1 - error_medio), 1e-10)
      arboles[[length(arboles) + 1]] <- arbol
      confianzas <- c(confianzas, log(1 / beta))
      pesos <- pesos * beta^(1 - perdida)
      pesos <- pesos / sum(pesos)
    }
    if (length(arboles) == 0) {
      arboles <- list(rpart::rpart(.y ~ ., data = datos, control = control))
      confianzas <- 1
    }
    list(arboles = arboles, confianzas = confianzas, variables = colnames(x))
  },
  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    nuevos <- as.data.frame(newdata)
    predicciones <- sapply(modelFit$arboles, \(arbol) predict(arbol, nuevos))
    predicciones <- matrix(predicciones, nrow = nrow(nuevos))
    apply(predicciones, 1, \(fila) {
      orden <- order(fila)
      acumulado <- cumsum(modelFit$confianzas[orden])
      fila[orden][which(acumulado >= 0.5 * sum(modelFit$confianzas))[1]]
    })
  },
  prob = NULL,
  varImp = function(object, ...) {
    importancia <- setNames(rep(0, length(object$variables)), object$variables)
    for (i in seq_along(object$arboles)) {
      parcial <- object$arboles[[i]]$variable.importance
      importancia[names(parcial)] <- importancia[names(parcial)] + object$confianzas[i] * parcial
    }
    data.frame(Overall = importancia, row.names = names(importancia))
  },
  sort = function(x) x[order(x$mfinal, x$maxdepth), ]
)

modelo_alometrico <- list(
  label = "Regresion alometrica",
  library = NULL,
  type = "Regression",
  parameters = data.frame(parameter = "parameter", class = "character", label = "Ninguno"),
  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "ninguno"),
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    datos <- data.frame(log(as.data.frame(x)), .y = log(y))
    ajuste <- lm(.y ~ ., data = datos)
    list(ajuste = ajuste, smearing = mean(exp(residuals(ajuste))), variables = colnames(x))
  },
  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    exp(predict(modelFit$ajuste, log(as.data.frame(newdata)))) * modelFit$smearing
  },
  prob = NULL,
  varImp = function(object, ...) {
    estadistico <- abs(summary(object$ajuste)$coefficients[-1, "t value"])
    data.frame(Overall = estadistico, row.names = object$variables)
  },
  sort = function(x) x
)

especificar_modelos <- function(numero_predictores) {
  modelos <- list(
    MLR = list(method = "lm"),
    Alometrico = list(method = modelo_alometrico, escalar = FALSE),
    ENet = list(method = "glmnet", tuneLength = 10),
    PLS = list(method = "pls", tuneGrid = data.frame(ncomp = seq_len(numero_predictores))),
    KNN = list(method = "knn", tuneGrid = data.frame(k = seq(3, 15, 2))),
    CART = list(
      method = "rpart",
      tuneGrid = data.frame(cp = c(0, 0.001, 0.005, 0.01, 0.02, 0.05, 0.1)),
      control = rpart::rpart.control(minsplit = 6, minbucket = 3)
    ),
    RF = list(method = "rf", tuneLength = 5, ntree = 500, importance = TRUE),
    XGBoost = list(method = modelo_xgboost),
    SVM = list(method = "svmRadial", tuneLength = 8),
    ANN = list(
      method = "nnet",
      tuneGrid = expand.grid(size = c(2, 4, 6, 8, 10, 12), decay = c(0.001, 0.01, 0.1, 0.3)),
      linout = TRUE, trace = FALSE, maxit = 2000
    ),
    AdaBoost = list(method = modelo_adaboost)
  )
  if (numero_predictores < 2) modelos$ENet <- NULL
  modelos
}

crear_control <- function(y, config) {
  set.seed(config$semilla)
  folds <- createMultiFolds(y, k = config$folds, times = config$repeticiones)
  semillas <- c(
    map(seq_along(folds), \(i) sample.int(1e6, 200)),
    list(sample.int(1e6, 1))
  )
  trainControl(
    method = "repeatedcv",
    number = config$folds,
    repeats = config$repeticiones,
    index = folds,
    seeds = semillas,
    savePredictions = "final",
    allowParallel = TRUE
  )
}

entrenar_modelos <- function(entrenamiento, variables, config) {
  x <- as.data.frame(entrenamiento[variables])
  y <- entrenamiento$PEVI
  control <- crear_control(y, config)
  imap(especificar_modelos(length(variables)), \(especificacion, nombre) {
    escalar <- especificacion$escalar %||% TRUE
    especificacion$escalar <- NULL
    argumentos <- c(
      list(x = x, y = y, trControl = control, metric = "RMSE"),
      if (escalar) list(preProcess = "range"),
      especificacion
    )
    set.seed(config$semilla)
    suppressWarnings(do.call(caret::train, argumentos))
  })
}
