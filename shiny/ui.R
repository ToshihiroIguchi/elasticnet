library(shiny)
library(glmnet)

shinyUI(
  fluidPage(
    titlePanel("Elastic Net, LASSO, Ridge"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Choose csv file",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),

        tags$hr(),
        htmlOutput("ydata"),
        htmlOutput("xdata"),

        htmlOutput("method"),
        htmlOutput("lambda"),

        actionButton("submit", "Analyze")

      ),

      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Table", tableOutput('table')),
                    tabPanel("Result",
                             plotOutput("plot"),
                             verbatimTextOutput("sum"),
                             sliderInput("font.size", "Font size",
                                         min = 5, max = 30, value = 18)
                             )

        )
      )
    )
  )
)
