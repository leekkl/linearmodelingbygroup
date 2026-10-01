
library(writexl)
library(dplyr)
library(stringr)
library(readxl)
library(lme4)
library(shiny)
library(bslib)
library(ggplot2)
library(rsconnect)

rsconnect::writeManifest()

PISA_dataset_mean2.2 <- read_excel("PISA_linear_dataset.xlsx")

ui <- fluidPage(
  titlePanel("Multiple Linear Regression with Group Selection: Academic Achievements of 15-Year-Olds by Country"),
  sidebarPanel(
    # Select response variable (Y)
    selectInput("dep_var", "Dependent Variable (Y):", 
                choices = names(PISA_dataset_mean2.2[c(18:21)]), selected = "score_sci_avg"),
    
    # Select multiple independent variables (X)
    checkboxGroupInput("indep_var", "Independent Predictors (X):", 
                       choices = names(PISA_dataset_mean2.2[-c(1:4,18:21)]), selected = c("ESCS", "COGABIL","GOALSET")),
    
    # Select Grouping/Clustering Variable 
    selectInput("group_var", "Group variable--Country", 
                choices = c("CNT")),
    # Dynamic choice for which specific group to analyze (if grouped)
    uiOutput("subgroup_ui"),
    wellPanel(h1(p("Source: PISA 2025", style = "font-size: 14px;"),
                 p("PI: Hyungoo Lee", style = "font-size: 12px;")))
  ),
  navset_card_tab(
    nav_panel("Model Summary", 
              verbatimTextOutput("model_summary")),
    nav_panel("Visualization: Residuals vs Fitted Values", 
              plotOutput("resid_plot", height = "550px")),
    nav_panel("Variables",
              wellPanel(
                h4("Dependent Variable Definitions"),
                p(strong("score_sci_avg:"), " Average science achievement score (Numeric variable)."),
                p(strong("score_math_avg:"), " Average math achievement score (Numeric variable)."),
                p(strong("score_env_avg:"), " Average environmental awareness score (Numeric variable)."),
                p(strong("score_read_avg:"), " Average reading score (Numeric variable)."),
                h4("Independent Variable Definitions"),
                p(strong("MALE:"), " Student's gender (Categorical variable)."),
                p(strong("ESCS:"), " Economic, social, and cultural index (Numeric variable)."),
                p(strong("HISEI:"), " Highest parental occupational status (Numeric variable)."),
                p(strong("FAMSUP:"), " Student's perception of family support (Numeric variable)."),
                p(strong("TEACHSUP:"), " Teacher's support in science class (Numeric variable)."),                  
                p(strong("COGABIL:"), " Cognitive adaptability (Numeric variable)."),
                p(strong("EFFSCIE:"), " Science self-efficacy (Numeric variable)."),
                p(strong("ENVAPART:"), " Student's participation in environment-related activities (Numeric variable)."),     
                p(strong("SELFREG:"), " Self-regulation (Numeric variable)."),
                p(strong("GOALSET:"), " Goal setting (Numeric variable)."),
                p(strong("BULLIED:"), " Student reports on bullying  (Numeric variable)."),                  
                p(strong("BELONG:"), " Sense of belonging (Numeric variable)."),
                p(strong("DISCLISCI:"), " Disciplinary climate at schools (Numeric variable).")
              ))
  )
)

server <- function(input, output, session) {
  
  # Dynamically update subgroup choices based on selected group variable
  output$subgroup_ui <- renderUI({
    req(input$group_var)
    if (input$group_var == "None") return(NULL)
    
    # Get unique values of the chosen group column
    groups <- unique(PISA_dataset_mean2.2[[input$group_var]])
    selectInput("subgroup_val", "Select Country:", choices = groups)
  })
  
  # Filter data based on user group choice
  filtered_data <- reactive({
    df <- PISA_dataset_mean2.2
    if (!is.null(input$group_var) && input$group_var != "None" && !is.null(input$subgroup_val)) {
      # Filter rows matching the selected subgroup value
      df <- df[df[[input$group_var]] == input$subgroup_val, ]
    }
    df
  })
  
  # Run linear regression on filtered data
  fit_model <- reactive({
    req(input$dep_var, input$indep_var)
    formula <- paste(input$dep_var, "~", paste(input$indep_var, collapse = "+"))
    lm(as.formula(formula), data = filtered_data())
  })
  
  output$model_summary <- renderPrint({
    summary(fit_model())
  })
  
  output$resid_plot <- renderPlot({
    mod <- fit_model()
    ggplot(mod, aes(.fitted, .resid)) +
      geom_point(color = "blue", size = 3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Residuals vs Fitted Values",
           x = "Fitted Values",
           y = "Residuals") +
      theme_minimal()
  })
}

shinyApp(ui, server)
