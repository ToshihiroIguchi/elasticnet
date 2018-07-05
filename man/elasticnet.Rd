\name{elasticnet}
\alias{elasticnet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Wrapper to use the glmnet package that Elastic Net model can be analyzed by the formula format.
}
\description{
The glmnet package that Elastic Net can use is nice, but you can not use the formula and you need to type it in matrix.
So we created an elasticnet package that Elastic Net can use in formula.
This is a wrapper for the glmnet package, which requires the glmnet package to work.
Also, the elasticnet package can automatically tune the best alpha and lambda.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
elasticnet(formula, data, family = NULL,
offset = NULL, nfolds = 5, lambda = "lambda.1se",
alpha = NULL, alpha_step = 0.05, seed = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{formula}{
an object of class "formula" : a symbolic description of the model to be fitted.
}

\item{data}{
an optional data frame containing the variables in the model.
}

\item{family}{
Response type.
Default is NULL.
If NULL, family is automatically selected.
}

\item{lambda}{
"lambda.1se" or "lambda.min".
}

\item{alpha}{
The elasticnet mixing parameter, 0 or more and 1 or less.
alpha=1 is the lasso penalty, and alpha=0 the ridge penalty.
}

\item{alpha_step}{
Step in searching for parameter alpha.
}

\item{seed}{
Random number generation.
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}


\references{
%% ~put references to the literature/web site here ~
}
\author{
Toshihiro Iguchi
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
result <- elasticnet(Sepal.Length ~., iris)
plot(result)
summary(result)
%%##---- Should be DIRECTLY executable !! ----
%%##-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.
%%
%%## The function is currently defined as
%%function (x)
%%{
%%  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
