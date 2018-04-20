#CUI版のelasticnet読み込み
source(file.path("../R","elasticnet.R"))

#shiny用の関数読み込み
source("shiny_elasticnet.R")


server <- function(input, output, session) {
  observeEvent(input$file, {
    #テーブルにて表示
    csv_file <- reactive({read.csv(input$file$datapath)})
    output$table <- renderTable({head(csv_file(), n = 30)})

    #目的変数を選択
    output$ydata <- renderUI({
      selectInput("ydata", "Purpose variable", choices = colnames(csv_file()))
    })
  })



  observeEvent(input$ydata, {
    csv_file <- reactive({read.csv(input$file$datapath)})
    #説明変数を選択
    output$xdata <- renderUI({
      checkboxGroupInput("xdata",
                         label = "Explanatory variable",
                         choices = get.explanatory(csv_file(), input$ydata),
                         selected = get.explanatory(csv_file(), input$ydata)
                         )
      })

    #手法を選択
    output$method <- renderUI({
      selectInput("method", "Method", choices = c("Elastic Net", "LASSO", "Ridge"))
    })

    #ラムダを選択
    output$lambda <- renderUI({
      selectInput("lambda", "Penalty parameter lambda", choices = c("lambda.1se", "lambda.min"))
    })

  })

  observeEvent(input$submit, {

    csv_file <- reactive({read.csv(input$file$datapath)})

    #Elastic Netの計算
    result <- reactive({elasticnet(chr2formula(y = input$ydata, x= input$xdata),
                               data = csv_file(),
                               lambda = input$lambda,
                               alpha = get.alpha(input$method)
                               )
      })

    #結果のプロット
    output$plot <- renderPlot({plot(result(), font.size = input$font.size)})

    #結果の表示
    output$sum <- renderPrint({summary(result())})
  })
}





