#CUI用に作成したelasticnetをShinyで動かすための関数

#手前味噌だけどこれを参考にした
#https://github.com/ToshihiroIguchi/eztree/blob/master/eztree.R


#文字からformula
chr2formula <- function(y, x){
  ret <- paste0(y, "~", paste(x, collapse = "+"))
  ret <- as.formula(ret)
  return(ret)

}

#Explanatory variable
get.explanatory <- function(df, purpose = NULL){
  df.name <- colnames(df)
  if(length(purpose) == 1){
    #https://www.trifields.jp/how-to-remove-an-element-with-a-string-in-a-string-vector-with-r-1776
    ret <- df.name[-which(df.name %in% purpose)]
    #checkboxGroupInput のchoicesに渡す値。
    #listじゃなくてもよかったみたい。
  }else{
    ret <- df.name
  }
  return(ret)
}

#Elastic Net, LASSO, Ridgeからalphaを選択
get.alpha <- function(method){
  if(method == "LASSO"){return(1)}
  if(method == "Ridge"){return(0)}
  if(method == "Elastic Net"){return(NULL)}
}




