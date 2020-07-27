library(shiny)
library(ggplot2)
library(plotly)
library(rmarkdown)
library(knitr)
library(pander)
library(tidyverse)
library(dplyr)

myData = read_csv("https://raw.githubusercontent.com/cengaiyeung/DS501/master/redwinequality.csv")
myData = as.data.frame(myData)

# User interface
ui <- fluidPage(
  
  # Application title
  titlePanel("Case study 3 - Linear Regression"),
  withMathJax(),
  
  sidebarLayout(
    sidebarPanel(
      tags$b("Data:"),
      
      selectInput("indepvar", label = h3("Predictor variable"),
                  choices = list("fixed acidity" = "fixed acidity",
                                 "volatile acidity" = "volatile acidity" ,
                                 "citric acidity" = "citric acid",
                                 "residual sugar" = "residual sugar",
                                 "chlorides" = "chlorides",
                                 "free sulfur dioxide" = "free sulfur dioxide",
                                 "total sulfur dioxide" = "total sulfur dioxide",
                                 "density" = "density",
                                 "pH" = "pH", 
                                 "sulphates" = "sulphates",
                                 "alcohol" = "alcohol",
                                 "quality" = "quality"), selected = 1),
      selectInput("outcome", label = h3("Response variable"),
                  choices = list("fixed acidity" = "fixed acidity",
                                 "volatile acidity" = "volatile acidity",
                                 "citric acidity" = "citric acid",
                                 "residual sugar" = "residual sugar",
                                 "chlorides" = "chlorides",
                                 "free sulfur dioxide" = "free sulfur dioxide",
                                 "total sulfur dioxide" = "total sulfur dioxide",
                                 "density" = "density",
                                 "pH" = "pH", 
                                 "sulphates" = "sulphates",
                                 "alcohol" = "alcohol",
                                 "quality" = "quality"), selected = 1),
      hr(),
      tags$b("Plot:"),
      checkboxInput("se", "Add confidence interval around the regression line", TRUE),
      radioButtons("format", "Download report:", c("HTML", "PDF", "Word"),
                   inline = TRUE
      ),
      checkboxInput("echo", "Show code in report?", FALSE),
      downloadButton("downloadReport")
    ),
    mainPanel(
      
      tags$b("Data introduction: "), 
      uiOutput("data_introduction"),
      br(),
      tags$b("Mathematical details of simple linear regression: "),
      uiOutput("mathematical_details"),
      br(),
      tags$b("Your data: "),
      DT::dataTableOutput("tbl"),
      br(),
      uiOutput("data"),
      br(),
      tags$b("Compute parameters by hand:"),
      uiOutput("by_hand"),
      br(),
      tags$b("Regression plot:"),
      uiOutput("results"),
      plotlyOutput("plot"),
      br(),
      tags$b("Compute parameters in R:"),
      verbatimTextOutput("summary"),
      br(),
      tags$b("Interpretation:"),
      uiOutput("interpretation"),
      br(),
      tags$b("Pros and Caveats:"),
      uiOutput("PaC"),
      br(),
      br()
      
      
    )
  )
)




# Server

server <- function(input, output) {
  
  # Data output
  output$tbl <- DT::renderDataTable({
    y <- myData[,input$outcome]
    x <- myData[,input$indepvar]
    DT::datatable(data.frame(x, y),
                  extensions = "Buttons",
                  options = list(
                    lengthChange = FALSE,
                    dom = "Blfrtip",
                    buttons = c("copy", "csv", "excel", "pdf", "print")
                  )
    )
  })
  
  output$data <- renderUI({
    y <- myData[,input$outcome]
    x <- myData[,input$indepvar]
    if (anyNA(x) | length(x) < 2 | anyNA(y) | length(y) < 2) {
      "Invalid input or not enough observations"
    } else if (length(x) != length(y)) {
      "Number of observations must be equal for explainable variable and outcome"
    } else {
      withMathJax(
        paste0("\\(\\bar{x} =\\) ", round(mean(x), 3)),
        br(),
        paste0("\\(\\bar{y} =\\) ", round(mean(y), 3)),
        br(),
        paste0("\\(n =\\) ", length(x))
      )
    }
  })
  
  output$by_hand <- renderUI({
    y <- myData[,input$outcome]
    x <- myData[,input$indepvar]
    fit <- lm(y ~ x)
    withMathJax(
      paste0("\\(\\hat{\\beta}_1 = \\dfrac{\\sum^n_{i = 1}   (x_{i} - \\bar{x}) (y_{i} - \\bar{y})}{\\sum^n_{i = 1} (x_{i} - \\bar{x})^2}  = \\) ", round(fit$coef[[2]], 3)),
      br(),
      paste0("\\(\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x} = \\) ", round(fit$coef[[1]], 3)),
      br(),
      br(),
      paste0("\\( \\Rightarrow y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = \\) ", round(fit$coef[[1]], 3), " + ", round(fit$coef[[2]], 3), "\\( x \\)")
    )
  })
  
  output$distribution1 <- renderPlot({
    hist(myData[,input$outcome], main="", xlab=input$outcome)
  }, height=400, width=400)
  
  output$distribution2 <- renderPlot({
    hist(myData[,input$indepvar], main="", xlab=input$indepvar)
  }, height=400, width=400)
  
  output$results <- renderUI({
    y <- myData[,input$outcome]
    x <- myData[,input$indepvar]
    fit <- lm(y ~ x)
    withMathJax(
      paste0(
        "Adj. \\( R^2 = \\) ", round(summary(fit)$adj.r.squared, 3),
        ", \\( \\beta_0 = \\) ", round(fit$coef[[1]], 3),
        ", \\( \\beta_1 = \\) ", round(fit$coef[[2]], 3),
        ", P-value ", "\\( = \\) ", signif(summary(fit)$coef[2, 4], 3)
      )
    )
  })
  
  
  output$summary <- renderPrint({
    y <- myData[,input$outcome]
    x <- myData[,input$indepvar]
    fit <- lm(y ~ x)
    summary(fit)
  })
  
  output$interpretation <- renderUI({
    y <- myData[,input$outcome]
    x <- myData[,input$indepvar]
    fit <- lm(y ~ x)
    if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
      withMathJax(
        paste0("For a (hypothetical) value of ", input$indepvar, " = 0, the mean of ", input$outcome, " = ", round(fit$coef[[1]], 3), "."),
        br(),
        paste0("Since both p-value is less than 0.05, then we can infer that there is an association between the predictor and the response. We reject the null hypothesis-that is, we declare a relationship to exist between predictor and response."),
        br(),
        paste0("For an increase of one unit of ", input$indepvar, ", ", input$outcome, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
      )
    } else if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] >= 0.05) {
      withMathJax(
        paste0("For a (hypothetical) value of ", input$indepvar, " = 0, the mean of ", input$outcome, " = ", round(fit$coef[[1]], 3), "."),
        br(),
        paste0("Since the p-value of", "\\( \\beta_1 \\)" , " is greater than 0.05, then we can't infer that there is an association between the predictor and the response. We can't reject the null hypothesis-that is, we declare no relationship to exist between predictor and response."),
        br(),
        paste0("\\( \\beta_1 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[2, 4], 3), ") so there is no significant relationship between ", input$indepvar, " and ", input$outcome, ".")
      )
    } else if (summary(fit)$coefficients[1, 4] >= 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
      withMathJax(
        paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
        br(),
        paste0("\\( \\beta_0 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[1, 4], 3), ") so when ", input$indepvar, " = 0, the mean of ", input$outcome, " is not significantly different from 0."),
        br(),
        paste0("Since the p-value of", "\\( \\beta_1 \\)", " is less than 0.05, then we can infer that there is an association between the predictor and the response. We reject the null hypothesis-that is, we declare a relationship to exist between predictor and response."),
        br(),
        paste0("For an increase of one unit of ", input$indepvar, ", ", input$outcome, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
      )
    } else {
      withMathJax(
        paste0("\\( \\beta_0 \\)", " and ", "\\( \\beta_1 \\)", " are not significantly different from 0 (p-values = ", round(summary(fit)$coefficients[1, 4], 3), " and ", round(summary(fit)$coefficients[2, 4], 3), ", respectively) so the mean of ", input$outcome, " is not significantly different from 0."),
        br(),
        paste0("Since the p-value of", "\\( \\beta_1 \\)" , " is greater than 0.05, then we can't infer that there is an association between the predictor and the response. We can't reject the null hypothesis-that is, we declare no relationship to exist between predictor and response."),
        br()
      )
    }
  })
  
  output$mathematical_details <- renderUI({
    withMathJax(
      paste0("Mathematically, we can write this linear relationship as ", "\\(Y =\\)", "\\(\\beta_0 +\\)", "\\(\\beta_1 \\)", "\\(X + \\)", "\\(\\epsilon\\)"),
      br(),
      paste0("Let ", "\\(\\hat{y}_{i} = \\)", "\\(\\hat{\\beta}_0 + \\)", "\\(\\hat{\\beta}_1 \\)", "\\(x_{i} \\) ", " be the the predictor for ", "\\(Y \\)", " based on the ", "\\(i\\)", "th value of ", "\\(X \\)"),
      br(),
      paste0("Then ", "\\(e_{i} = y_{i} - \\hat{y}_{i} \\)", " represents the ith residual—this is the difference between the ith observed response value and the ith response value that is predicted by our linear model."),
      br(),
      paste0("We define the residual sum of squares (RSS) as ", "RSS ", "\\( = e_{1}^2 + e_{2}^2 + \\cdots + e_{n}^2 \\)"),
      br(),
      paste0("The least squares approach chooses ", "\\( \\beta_0 \\)", "and ", "\\(\\beta_1 \\)", "to minimize the RSS. Using some calculus, one can show that the minimizers are "), 
      br(),
      paste0("\\(\\hat{\\beta}_1 = \\dfrac{\\sum^n_{i = 1}   (x_{i} - \\bar{x}) (y_{i} - \\bar{y})}{\\sum^n_{i = 1} (x_{i} - \\bar{x})^2}  \\) "),
      br(),
      paste0("\\(\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x}  \\)")
    )
  })
  
  output$PaC <- renderUI({
    withMathJax(
      paste0("When using linear regression, the pros are: 1. It can give a concise representation; 2. This model is robust to redundant variables and correlated variables; 3. Easy to score data. However, it has some disadvantages such as: 1. The model does not handle missing values well; 2. Can't handle variables that affect the outcome in a discontinuous way; 3. Doesn't work well with discrete drivers that have a lot of distinct values.")
    )
  })
  
  
  output$data_introduction <- renderUI({
    withMathJax(
      paste0("In modern society, people often drink red wine since it has some benefits: 1. Drinking red wine represent people society status; 2. Red wine, in moderation, has long been thought of as heart healthy. The alcohol and certain substances in red wine called antioxidants may help prevent coronary artery disease, the condition that leads to heart attacks."), 
      br(), 
      paste0("So, I found the red wine dataset on https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009 and want to explore some features of that dataset."),
      br(),
      paste0("By using summary to simply explore the data, we found that all variables of data is numeric and there is no missing value in the data. Due to this observation, I decide to choose simple linear regression to explore the feature of each variable."),
      br(),
      paste0("The reason I choose simple linear regression model is, linear regression model is a strong tool which can help us to explain the relationship among numeric variables."),
      br(),
      br()
    )
  })
  
  
  output$plot <- renderPlotly({
    y <- myData[,input$outcome]
    x <- myData[,input$indepvar]
    fit <- lm(y ~ x)
    dat <- data.frame(x, y)
    p <- ggplot(dat, aes(x = x, y = y)) +
      geom_point() +
      stat_smooth(method = "lm", se = input$se) +
      ylab(input$outcome) +
      xlab(input$indepvar) +
      theme_minimal()
    ggplotly(p)
  })
  
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("my-report", sep = ".", switch(
        input$format, PDF = "pdf", HTML = "html", Word = "docx"
      ))
    },
    
    content = function(file) {
      src <- normalizePath("report.Rmd")
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "report.Rmd", overwrite = TRUE)
      
      library(rmarkdown)
      out <- render("report.Rmd", switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
}
shinyApp(ui = ui, server = server)

