#Elastic Net

#Elastic Netの関数
elasticnet <- function(formula, data,
                       family = "gaussian",
                       offset = NULL,
                       nfolds = 5,
                       lambda = "lambda.1se",
                       alpha = NULL,
                       alpha_step = 0.05){
  #説明変数を指定。カテゴリカル変数はダミー変数に変換される。
  x <- model.matrix(formula,data=data)[,-1]

  #目的変数を指定。
  y <- model.frame(formula,data=data)[,1]

  #offsetを指定。
  #dataの変数を使用してoffsetの式で計算した結果を返す。
  offset_su <- substitute(offset)
  offset <- eval(offset_su, data)

  df <- data.frame()
  ret <- list(formula =formula)
  i <- 1
  a_seq <- if(is.null(alpha)){seq(0, 1, by = alpha_step)}else{alpha}
  for(a in a_seq){
    result <- cv.glmnet(x = x, y = y, offset = offset,
                        family = family, nfolds = nfolds,
                        alpha = a, keep = TRUE)
    ret$glmnet[[i]] <- result; i <- i + 1

    num <- switch(lambda,
                  "lambda.1se" = which(result$lambda == result$lambda.1se),
                  "lambda.min" = which(result$lambda == result$lambda.min),
                  stop("Please select either lambda.1se or lambda.min."))

    df0 <- data.frame(alpha = a, cvm = result$cvm[num] , num = num)
    df <- rbind(df, df0)
  }
  ret$ydata <- y
  ret$alpha_cvm <- df
  ret$best_num <- order(df[,2])[1]
  ret$lambda <- lambda
  class(ret) <- "elasticnet"
  return(ret)
}


#LASSOの関数
lasso <- function(formula, data,
                  family = c("gaussian"),
                  offset = NULL,
                  nfolds = 5,
                  lambda ="lambda.1se"){
  ret <- elasticnet(formula = formula, data = data,
                    family = family, offset = offset,
                    lambda = lambda, alpha = 1)
  return(ret)
}


#Ridgeの関数
ridge <- function(formula, data,
                  family = c("gaussian"),
                  offset = NULL,
                  nfolds = 5,
                  lambda ="lambda.1se"){
  ret <- elasticnet(formula = formula, data = data,
                    family = family, offset = offset,
                    lambda = lambda, alpha = 0)
  return(ret)
}


#Elastic Netをplot関数で表示
plot.elasticnet <- function(model){
  par(mfrow = c(1, 3))
  plot(model$alpha_cvm[,c(1:2)], type ="b")
  abline(v = model$alpha_cvm[model$best_num, 1], lty = 3)
  plot(model$glmnet[[model$best_num]])
  calc <- model$glmnet[[model$best_num]]$fit.preval[,model$alpha_cvm$num[model$best_num]]
  xy_data <- data.frame(calculation = calc, measure = model$ydata)
  plot(xy_data)
  par(mfrow = c(1, 1))
}


#Elastic Netで予測
predict.elasticnet <- function(model, data){
  x <- model.matrix(model$formula, data = data)[,-1]
  ret <- predict(model$glmnet[[model$best_num]], x,
                 s = model$lambda, type = "class")
  return(ret)
}



#Elastic Netのsummaryを表示
summary.elasticnet <- function(model){
  #lambdaの選択方法、alpha、lambdaを表示
  cat(model$lambda); cat("\n\n")
  cat(paste("alpha = ", model$alpha_cvm[model$best_num, 1]), "\n", sep = "")
  cat("lambda = ")
  switch(model$lambda,
         "lambda.1se" = cat(model$glmnet[[model$best_num]]$lambda.1se),
         "lambda.min" = cat(model$glmnet[[model$best_num]]$lambda.min))
  cat("\n\n")

  #係数を表示
  coef_cat <- data.frame(as.matrix(coef(model$glmnet[[1]], s = model$lambda)))
  names(coef_cat) <- "Coefficients"
  print(coef_cat)
}

