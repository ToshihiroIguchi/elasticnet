## elasticnet package
Wrapper to use the glmnet package that Elastic Net model can be analyzed by the formula format.

### Description
The glmnet package that Elastic Net can use is nice, but you can not use the formula and you need to type it in matrix.
So we created an elasticnet package that Elastic Net can use in formula.
This is a wrapper for the glmnet package, which requires the glmnet package to work.
Also, the elasticnet package can automatically tune the best alpha and lambda.

### Installation
You can install from R console.

If 'devtools' is not installed on your PC, install 'devtools' with Internet connection.

    install.packages("devtools")

Install from GitHub using 'devtools'.
    
    library(devtools)
    install_github("ToshihiroIguchi/elasticnet")



### License 
MIT

### Auther
Toshihiro Iguchi

