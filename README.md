## elasticnet package
Wrapper to use the glmnet package that Elastic Net model can be analyzed by the formula format.

### Description
The glmnet package that Elastic Net can use is nice, but you can not use the formula and you need to type it in matrix.
So we created an elasticnet package that Elastic Net can use in formula.
This is a wrapper for the glmnet package, which requires the glmnet package to work.
Also, the elasticnet package can automatically tune the best alpha and lambda.

### Installation
You can install from R console.

If devtools is not installed on your PC, install devtools with Internet connection.

    install.packages("devtools")

Install from GitHub using devtools.
    
    library(devtools)
    install_github("ToshihiroIguchi/elasticnet")

Load the elasticnet package and attach it.

    library(elasticnet)

### Examples
Explain with Fisher's iris as an example.
When analyzing with Elastic Net, use elasticnet function.
The elasticnet function can be used like an lm function or a glm function which is often used in R.

The elasticnet function has the following features.
Elastic Net needs to set appropriate alpha and lambda, but if you use the elasticnet function, alpha and lambda will be tuned automatically according to the data.
The nominal scale is automatically converted to a dummy variable.
The object variable is selected automatically, but it can be specified with "method".

    result <- elasticnet(Sepal.Length ~., iris)

The objective variable can be analyzed also on the nominal scale.

    result.nominal <- elasticnet(Species ~., iris)

If you want to do Lasso you can use the lasso function, and if you want to do Ridge you can use the ridge function like the elasticnet function. Alpha = 1 in the lasso function, alpha = 0 in the ridge function, and optimization of alpha is not performed.

    result.lasso <- lasso(Sepal.length ~., iris)
    result.ridge <- ridge(Sepal.Length ~., iris)

A graph of optimization of alpha and lambda with the plot function and the result of cross validation are displayed. In the case of regression, a scatter diagram is displayed, and in case of classification, a mosaic plot is displayed.

    plot(result)

A summary function displays a list of analysis results.

    summary(result)

Prediction can be done with the predict function.

    predict(result, iris)
    
### GUI

Many people are not good at command by CUI.
[You can also execute elasticnet with browser.](https://github.com/ToshihiroIguchi/elasticnet/tree/master/shiny)


### License 
MIT

### Auther
Toshihiro Iguchi

