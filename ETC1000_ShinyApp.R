# Retail Analytics Shiny App
# ----------------------------------
# Interactive Shiny application developed as part of ETC1000
# in Monash University to explore relationships between customer
# characteristics and total spending using regression and
# visualisation techniques as learned from coursework

#libraries
library(shiny)
library(ggplot2)
library(corrplot)
library(bslib)
library(sjmisc)
library(sjPlot)
library(tidyverse)

#data loading
df <- read.csv("Retail Analytics Data.csv")
summary(df$Total_Spending)

numeric_vars <- c(
  "Age",
  "Household_Income",
  "Distance_Mall",
  "Travel_Time_to_Mall",
  "Time_Spent_in_Mall",
  "N_items_purchased",
  "Distance_Walked_in_Mall"
)

#frontend
ui <- fluidPage(
  theme = bs_theme(),
  input_dark_mode(id = "mode"), 
  titlePanel("Retail Analytics Data"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Total_Spending Statistics",
               h3("Summary of Total_Spending"),
               verbatimTextOutput("total_spending_summarystatistics"),
               plotOutput("total_spending_histogram"),
               plotOutput("total_spending_boxplot")
      ),
      
      tabPanel("Total_Spending vs Age",
               plotOutput("total_spending_vs_age_correlationplot"),
               h3("Scatter Plot for Total_Spending compared to Age"),
               plotOutput("total_spending_vs_age_scatterplot")
      ),
      
      tabPanel("Total_Spending vs Household_Income",
               plotOutput("total_spending_vs_household_income_correlationplot"),
               h3("Scatter Plot for Total_Spending 
                    compared to Household_Income"),
               plotOutput("total_spending_vs_household_income_scatterplot")
      ),
      tabPanel("Age Model",
               verbatimTextOutput("age_model_summary"),
               tableOutput("age_model_predictions")
      ),
      
      tabPanel("Regression Analysis",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("dependent_variable", "Select Dependent Variable:",
                               choices = c("Total_Spending", "Age", "Household_Income",
                                           "Distance_Mall", "Travel_Time_to_Mall",
                                           "Time_Spent_in_Mall", "N_items_purchased",
                                           "Distance_Walked_in_Mall", "Spent_more_than_100"),
                               multiple = FALSE, 
                               selected = "Total_Spending"),
                   
                   selectInput(
                     "independent_variable",
                     "Select Independent Variable:",
                     choices = numeric_vars,
                     multiple = FALSE,
                     selected = "Age"
                   )
                   
                 ),
                 
                 mainPanel(
                   h3("Regression Summary"),
                   uiOutput("regression_summary"),
                   
                   h3("Regression Plot"),
                   plotOutput("regression_plot")
                 )
                 
               )
      ),
      
      tabPanel("Correlation Heatmap",
               h3("Correlation Heatmap"),
               h6("Correlation summary for all numeric variables"),
               plotOutput("correlation_plot")
      )
      
    )
  )
)

#backend
server <- function(input, output, session) {
  
  fit_model <- reactive({
    req(input$dependent_variable, input$independent_variable)  
    
    formula <- as.formula(
      paste(input$dependent_variable, "~", 
            paste(input$independent_variable, collapse = " + "))
    )
    
    lm(formula, data = df)
  })

  output$regression_summary <- renderUI({
    req(input$dependent_variable, input$independent_variable)
    
    model <- fit_model()
    
    result <- tab_model(
      model,
      show.ci = TRUE,
      show.se = TRUE,
      dv.labels = input$dependent_variable
    )
    
    HTML(result$page.complete)
  })  
  
  output$regression_plot <- renderPlot({
    req(input$independent_variable)
    
    model <- fit_model()
    
    plot_model(
      model,
      type = "pred",
      terms = as.character(input$independent_variable)
    )
  })
  
    
  output$correlation_plot <- renderPlot({
    numeric_df <- df[sapply(df, is.numeric)]  
    corr_matrix <- cor(numeric_df, use = "complete.obs")  
    
    corrplot(corr_matrix, method = "circle", type = "lower")
  })
  
  output$total_spending_summarystatistics <- renderPrint({
    summary(df$Total_Spending)
  })
  
  output$total_spending_histogram <- renderPlot({
    ggplot(df, aes(x = Total_Spending)) + 
      geom_histogram(bins = 25, fill = "skyblue", color = "black") +
      labs(title = "Histogram of Total_Spending", x= "Total_Spending",
           y = "Count")
  })
  
  output$total_spending_boxplot <- renderPlot({
    ggplot(df, aes(y = Total_Spending)) + 
      geom_boxplot(fill = "orange") +
      labs(title = "Boxplot of Total_Spending", y = "Total_Spending")
  })
  
  output$total_spending_vs_age_scatterplot <- renderPlot({
    ggplot(df, aes(x = Age, y = Total_Spending)) +
      geom_point(color = "blue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = "Total_Spending vs Age", x = "Age", y = "Total_Spending")
  })
  
  output$total_spending_vs_age_correlationplot <- renderPlot({
    total_spending_vs_age_correlationplot_data <- df[, c("Total_Spending",
                                                         "Age")]
    total_spending_vs_age_correlationplot_matrix <- 
      cor(total_spending_vs_age_correlationplot_data, use="complete.obs")
    corrplot(total_spending_vs_age_correlationplot_matrix, method = "circle",
             type = "lower", tl.col = "black", addCoef.col = "white", 
             number.digits = 5)
  })
  
  output$total_spending_vs_household_income_scatterplot <- renderPlot({
    ggplot(df, aes(x = Household_Income, y = Total_Spending)) +
      geom_point(color = "blue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = "Total_Spending vs Household_Income", x = "Household_Income", 
           y = "Total_Spending")
  })
  
  output$total_spending_vs_household_income_correlationplot <- renderPlot({
    total_spending_vs_household_income_correlationplot_data <- 
      df[, c("Total_Spending", "Household_Income")]
    total_spending_vs_household_income_correlationplot_matrix <- 
      cor(total_spending_vs_household_income_correlationplot_data, 
          use="complete.obs")
    corrplot(total_spending_vs_household_income_correlationplot_matrix, 
             method = "circle", type = "lower", tl.col = "black", 
             addCoef.col = "white", number.digits = 5)
  })
  
  output$age_model_summary <- renderPrint({
    model <- lm(Total_Spending ~ Age, data = df)
    summary(model)
  })

  output$age_model_predictions <- renderTable({
    age_ranges <- list(
      "17–21" = 17:21,
      "22–26" = 22:26,
      "27–31" = 27:31,
      "32–36" = 32:36,
      "37–41" = 37:41,
      "42–46" = 42:46,
      "47–52" = 47:52
    )
    
    prediction_data <- data.frame(
      Age = sapply(age_ranges, function(range) median(range))
    )
    
    predicted_spending <- predict(lm(Total_Spending ~ Age, data = df), 
                                  newdata = prediction_data)
    
    actual_averages <- sapply(age_ranges, function(range) {
      mean(df$Total_Spending[df$Age %in% range], na.rm = TRUE)
    })
    
    data.frame(
      Age_Range = names(age_ranges),
      Predicted_Spending = round(predicted_spending, 2),
      Actual_Average_Spending = round(actual_averages, 2)
    )
  })
  
}

shinyApp(ui = ui, server = server)
