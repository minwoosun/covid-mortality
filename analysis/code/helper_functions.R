#################
# Model helpers #
#################

fit.ols <- function(x, y, x.indices){
  #' fit OLS 
  #'
  #' @param x feature matrix
  #' @param y target variable vector
  #' @param x.indices vector of integers to index x 
  #' @return returns lm fit object
  
  x <- x[,x.indices]
  df.ols <- cbind(x, y) %>% data.frame
  fit <- lm(y ~ x, data=df.ols)
  
  return(fit)
}


compute_rMSE <- function(y1, y2){
  #' compute rMSE for true y and predicted y
  #'
  #' @param y1 true y
  #' @param y2 predicted y
  #' @return returns scalar rMSE
  
  return(sqrt(mean((y1 - y2)^2)))
}


select_predictions = function(query, iso, y, yhat, plot=T){
  #' @param query vector of country iso3c code of interest
  #' @param iso vector of all iso3c code the model made predictions for
  #' @param y observed y (must come first)
  #' @param yhat predicted y from a model
  #' @return [[1]] dataframe of queried prediction vs observation
  #' @return [[2]] scatter plot of predictions highlighting query
  
  # sorted index matching query
  index.query = which(iso %in% query)
  
  # create dataframe of y, yhat to filter by query
  iso_ = data.frame(iso)
  df_ = data.frame(cbind(iso_, round(y,2), round(yhat,2)))
  df.query = df_[index.query,]
  colnames(df.query) = c("iso", "y", "yhat")
  
  # coloring for plot
  colors = rep("other", length(iso))
  colors[index.query] = iso[index.query]
  
  if (plot == T){
    scatter.plot = qplot(
      y, 
      yhat,
      xlab = "Observed (Y)",
      ylab = "Predicted (Y-hat)",
      main = "Observed vs Predicted",
      color = factor(colors)) +
      geom_smooth(method = "lm", formula = y ~ x) +
      annotate(
        "text",
        x = 700, y = 10,
        label = paste0("Pearson corr: ", round(cor(y,  yhat),2)),
      ) 
  } else {
    scatter.plot = NULL
  }
  
  return(list(df.query, scatter.plot))
}


rMSE_rf <- function(model, mtry, y.mean, y.std, y.transform){
  #' Compute repeated CV rMSE for caret train object 
  #'
  #' @param model: train, train.formula object from caret::train()
  #' @param mtry: number of randomly selected predictors 
  #' @param y.mean: mean for unscaling Y and Yhat
  #' @param y.std: mean for unscaling Y and Yhat
  #' @param y.transform: int for untransforming Y and Yhat
  #' @return [[1]] minimum rMSE 
  #'         [[2]] plot of repeatedCV rMSE
  
  rf.rmse = c()
  
  for (i in 1:mtry){
    rf.df = rf$pred[rf$pred$mtry==i,]
    rf.yhat = undo_scale_and_transform(rf.df$pred, y.mean, y.std, y.transform)  
    rf.y = undo_scale_and_transform(rf.df$obs, y.mean, y.std, y.transform)  
    rf.rmse[i] = compute_rMSE(rf.y, rf.yhat)
  }
  
  df.plot = data.frame(cbind(x=1:mtry, y=rf.rmse))
  
  plt = ggplot(df.plot, aes(x=x, y=y)) +
    geom_point() +
    geom_line() +
    xlab("#Randomly Selected Predictors") +
    ylab("rMSE (repeated CV)")
  
  # print minimum rmse
  rmse.min = rf.rmse %>% min 
  
  return(list(rmse.min, plt))
}


############################
# Transformation functions #
############################

cube_root = function(x){
  #' cube root transforms each element in a vector
  #'
  #' @param x input to be transformed
  #' @return cube root transformed vector x             
  
  index.negative = which(x < 0) 
  x = abs(x)^(1/3)
  x[index.negative] = x[index.negative] * (-1)
  
  return(x)
}


transform_and_scale = function(x, transform.type){
  #' first transform then scale vector (target variable);
  #' 
  #' transform.type == 0 is no transform
  #' transform.type == 1 is log transform
  #' transform.type == 2 is cube root transform
  #' 
  #' @param x input to be transformed
  #' @param func log or cube.root
  #' @return list of [[1]] transformed and scaled x  
  #'                 [[2]] (mean, std, log/cube transform)
  
  if (transform.type == 1){
    x = log(x)
  }
  
  if (transform.type == 2){
    x = cube_root(x)
  }
  
  x.mean = mean(x)
  x.std = sd(x)
  x = scale(x)
  
  key = c("scale.mean" ,"scale.std", "transform.type")
  value = c(x.mean, x.std, transform.type)
  attr = data.frame(cbind(key, value))
  attr[,2] = as.numeric(attr[,2])
  
  return(list(x, attr))
}


undo_scale_and_transform = function(x, x.mean, x.std, transform.type){
  #' undo z-score then undo log/cube of a vector (target variable)
  #'
  #' @param x input to be untransformed
  #' @param x.mean mean that was used for scale
  #' @param x.std std that was used for scale
  #' @param func log or cube root transform
  #' @return unscaled and untransformed x
  
  x = (x*x.std) + x.mean
  if (transform.type==1){x = exp(x)}
  if (transform.type==2){x = (x^3)}
  
  return(x)
}


get_index <- function(iso.all, iso.query){
  index = which(iso.all %in% iso.query)
  return(index)
}


get_preds <- function(yhat, iso.all=iso, iso.query=c("USA", "CAN"), n_countries=149, n_repeatedcv=10){
  # transformed yhat (will get untransformed here)
  
  country_index = c()
  for (i in 1:n_countries){
    country_index = c(country_index, rep(i,n_repeatedcv))
  }
  
  index.query<- as.character(get_index(iso.all, iso.query))
  
  preds = data.frame(cbind(country_index, yhat))
  output = preds[preds$country_index %in% index.query,] 
  
  return(output)
  
}
