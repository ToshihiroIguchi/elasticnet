#Elastic Net

#Elastic Netの関数
elasticnet <- function(formula, data,
                       family = NULL,
                       offset = NULL,
                       nfolds = 5,
                       lambda = "lambda.1se",
                       alpha = NULL,
                       alpha_step = 0.05){

  #説明変数を指定。カテゴリカル変数はダミー変数に変換される。
  #[,-1]としているのはinterceptを削除するため。
  x <- model.matrix(formula,data=data)[,-1]

  #目的変数を指定。
  #修正の余地があると思う。
  y <- model.frame(formula,data=data)[,1]

  #familyを自動選択。
  if(is.null(family)){
    if(is.factor(y) || is.character(y)){
      if(length(unique(y)) >= 3){family = "multinomial"}
      if(length(unique(y)) == 2){family = "binomial"}
      if(length(unique(y)) == 1){stop("The number of factors is one.")}
    }
    if(is.numeric(y) || is.integer(y)){
      if(min(y) < 1){family = "gaussian"}
      if(is.integer(y) && min(y) > 1){
        family = "poisson"
        }else{
        family = "gaussian"
      }
    }
  }
  if(is.null(family)){stop("Family could not be selected automatically.
                           Please specify family explicitly.")}

  #offsetを指定。
  #dataの変数を使用してoffsetの式で計算した結果を返す。
  offset_su <- substitute(offset)
  offset <- eval(offset_su, data)

  #最適なαを計算。
  #forでかいたけど、applyで書いた方がRらしくていいかも。
  df <- data.frame()
  result <- NULL
  ret <- list(formula =formula)
  i <- 1
  a_seq <- if(is.null(alpha)){seq(0, 1, by = alpha_step)}else{alpha}
  for(a in a_seq){
    result[[i]] <- cv.glmnet(x = x, y = y, offset = offset,
                        family = family, nfolds = nfolds,
                        alpha = a, keep = TRUE)

    num <- switch(lambda,
                  "lambda.1se" = which(result[[i]]$lambda == result[[i]]$lambda.1se),
                  "lambda.min" = which(result[[i]]$lambda == result[[i]]$lambda.min),
                  stop("Please select either lambda.1se or lambda.min."))

    df0 <- data.frame(alpha = a, cvm = result[[i]]$cvm[num] , num = num)
    df <- rbind(df, df0)
    i <- i + 1
  }

  #メモリ開放
  best_num <- order(df[,2])[1]
  ret$glmnet <- result[[best_num]]
  result <- NULL


  #クロスバリデーションの結果保存

  if(family == "gaussian" || family == "poisson"){
    #正規分布かポアソン分布の場合は数値。
    calc <- ret$glmnet$fit.preval[,df$num[best_num]]
  }else{
    if(family == "binomial"){
      cv_class <- function(model){
        classnames <- ret$glmnet$glmnet.fit$classnames
        cv_mat <- ret$glmnet$fit.preval[,df$num[best_num]]
        ret <- classnames[round(cv_mat, 0) + 1]
        ret <- as.factor(ret)
        return(ret)
      }
      calc <- cv_class(ret)
    }
    if(family == "multinomial"){
      #多クラス分類の結果比較
      #modelからcross varidation時のclassを出力。
      cv_class <- function(model){
        classnames <- ret$glmnet$glmnet.fit$classnames
        cv_mat <- ret$glmnet$fit.preval[,,df$num[best_num]]
        ret <- classnames[apply(cv_mat, 1, which.max)]
        ret <- as.factor(ret)
        return(ret)
      }
      calc <- cv_class(ret)
    }
  }

  #戻り値を定義
  ret$ydata <- y
  ret$calc <- calc
  ret$alpha_cvm <- df
  ret$best_num <- best_num
  ret$lambda <- lambda
  ret$family <- family
  class(ret) <- "elasticnet"
  return(ret)
}



#LASSOの関数
lasso <- function(formula, data,
                  family = NULL,
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
                  family = NULL,
                  offset = NULL,
                  nfolds = 5,
                  lambda ="lambda.1se"){

  ret <- elasticnet(formula = formula, data = data,
                    family = family, offset = offset,
                    lambda = lambda, alpha = 0)
  return(ret)
}


#Elastic Netをplot関数で表示
plot.elasticnet <- function(model, font.size = 20){

  if(length(model$alpha_cvm[, 1]) > 1){
    #alpha vs cvmの結果表示
    par(mfrow = c(1, 3), ps = font.size)
    plot(model$alpha_cvm[,c(1:2)], type ="b")
    abline(v = model$alpha_cvm[model$best_num, 1], lty = 3)

  }else{
    par(mfrow = c(1, 2), ps = font.size)
  }


  #glmnetの最適なlambda計算の結果
  plot(model$glmnet)

  if(model$family == "gaussian" || model$family == "poisson"){
    #正規分布かポアソン分布の場合は回帰。
    #回帰のときの予測と実測の散布図
    xy_data <- data.frame(calculation = model$calc, measure = model$ydata)
    plot(xy_data)
    abline(a=0, b=1, lty = 3)

    #詳細表示
    rsq <- paste0("R2 = ", round(summary(lm(xy_data))$r.squared,4))
    legend("topleft", legend = rsq)

  }else{
    #モザイク図を出力。
    mosaic_data <- data.frame(Predict = model$calc, Measure = model$ydata)
    plot(~Predict + Measure, data = mosaic_data,
         col = rainbow(length(unique(model$ydata))))
  }
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
  cat(paste0("Family is ", model$family)); cat("\n\n")
  cat(model$lambda); cat("\n\n")
  cat(paste("alpha = ", format(model$alpha_cvm[model$best_num, 1], digits = 4)), "\n", sep = "")
  cat("lambda = ")
  switch(model$lambda,
         "lambda.1se" = cat(format(model$glmnet$lambda.1se, digits = 4)),
         "lambda.min" = cat(format(model$glmnet$lambda.min, digits = 4)))
  cat("\n\n")

  #解析用データ
  df <- data.frame(model$ydata, predict = model$calc)


  if(model$family == "multinomial" || model$family == "binomial"){
    #分類の場合

    #x[1],x[2]が同じ場合は1、それ以外はは0
    counttrue <- function(x){
      if(x[1] == x[2]){return(1)}else{return(0)}
    }

    #accuracyを計算
    #正解の総数をデータ数で割る
    accuracy <- sum(apply(df, 1, counttrue))/length(df[,1])
    cat(paste0("Accuracy = ", format(accuracy, digits = 4))); cat("\n\n")

    #テーブル表示
    print(table(df)); cat("\n\n")

  }else{
    #回帰の場合
    #RMSEを計算
    rmse <- sum((df[,1] -df[,2])^2)/length(df[,1])
    cat(paste0("RMSE = ", format(rmse, digits = 4))); cat("\n\n")
  }

  #係数を表示
  coef(model$glmnet, s = model$lambda)

}

