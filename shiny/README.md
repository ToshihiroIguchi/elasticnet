## elasticnet
Sparse modeling with Elastic Net, LASSO, Ridge in GUI.

### Description
To do sparse modeling with Elastic Net, LASSO, Ridge in GUI, it is necessary to purchase expensive software.
Sparse modeling with GUI with free software.

### Launch web application
First, access [CRAN](https://cran.r-project.org/), download R and install.
You can install from R console.
If shiny and glmnet are not installed on your PC, install shiny and glmnet with Internet connection.

    install.packages(c("shiny", "glmnet"))

If shiny and glmnet are installed, it can be started from R console with the following command.
    
    shiny::runGitHub("elasticnet", "ToshihiroIguchi", subdir = "shiny/")

### Host
Host the Shiny application from GitHub in a private network.
Enter the following command in R console.

    #Port specification
    port <- 1603

    #Acquire private address information
    ipconfig.dat <- system("ipconfig", intern = TRUE)
    ipv4.dat <- ipconfig.dat[grep("IPv4", ipconfig.dat)][1]
    ip <- gsub(".*? ([[:digit:]])", "\\1", ipv4.dat)

    #Host the Shiny application from GitHub
    shiny::runGitHub("elasticnet", "ToshihiroIguchi", subdir = "shiny/", launch.browser = FALSE, port = port, host = ip)

If you are in the private network, you can also launch the Shiny application by entering the URL following `Listing on` to a browser.


### License 

```
MIT License

Copyright (c) 2018 Toshihiro Iguchi

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

### Auther
Toshihiro Iguchi
