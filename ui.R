# Load libraries
library(shinydashboard)
library(DT)

# Define UI
ui <- dashboardPage(
  
  # Dashboard header
  dashboardHeader(title = "Biological Monitoring"),
  
  # Dashboard sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Data selection",
        tabName = "data_selection",
        icon = icon("magnifying-glass")
      ),
      menuItem(
        "Standard comparison",
        tabName = "standard_comparison",
        icon = icon("book-open")
      ),
      menuItem(
        "Inter-individual comparison",
        tabName = "interindividual_comparison",
        icon = icon("users")
      ),
      menuItem(
        "Correlation analysis",
        tabName = "correlation_analysis",
        icon = icon("arrow-right-arrow-left")
      ),
      menuItem("Raw data", tabName = "raw_data", icon = icon("database"))
    )
  ),
  
  # Dashboard body
  dashboardBody(tabItems(
    # Data selection tab
    tabItem(
      tabName = "data_selection",
      h1("Data selection"),
      fluidRow(
        column(
          width = 3,
          h4("Data file"),
          fileInput(inputId = "data_file", label = NULL)
        ),
        column(
          width = 2,
          h4("Subjects"),
          checkboxGroupInput(inputId = "subject_selection", label = NULL)
        ),
        column(
          width = 2,
          h4("Dates"),
          checkboxGroupInput(inputId = "date_selection", label = NULL)
        ),
        column(
          width = 2,
          h4("Variables"),
          checkboxInput(inputId = "anthropometric", label = "Anthropometric variables"),
          conditionalPanel(
            condition = "input.anthropometric",
            style = "margin-left: 20px;",
            checkboxGroupInput("anthropometric_selection", label = NULL)
          ),
          checkboxInput(inputId = "performance", label = "Performance variables"),
          conditionalPanel(
            condition = "input.performance",
            style = "margin-left: 20px;",
            checkboxGroupInput("performance_selection", label = NULL)
          ),
          checkboxInput(inputId = "serum_chemistry_blood", label = "Serum chemistry blood variables"),
          conditionalPanel(
            condition = "input.serum_chemistry_blood",
            style = "margin-left: 20px;",
            checkboxGroupInput("serum_chemistry_blood_selection", label = NULL)
          ),
          checkboxInput(inputId = "whole_blood_analysis", label = "Whole blood analysis variables"),
          conditionalPanel(
            condition = "input.whole_blood_analysis",
            style = "margin-left: 20px;",
            checkboxGroupInput("whole_blood_analysis_selection", label = NULL)
          ),
          checkboxInput(inputId = "hematologic_and_iron_status_indicators", label = "Hematologic and iron status indicators variables"),
          conditionalPanel(
            condition = "input.hematologic_and_iron_status_indicators",
            style = "margin-left: 20px;",
            checkboxGroupInput("hematologic_and_iron_status_indicators_selection", label = NULL)
          ),
          checkboxInput(inputId = "hormones", label = "Hormones variables"),
          conditionalPanel(
            condition = "input.hormones",
            style = "margin-left: 20px;",
            checkboxGroupInput("hormones_selection", label = NULL)
          ),
          checkboxInput(inputId = "vitamin_status", label = "Vitamin status variables"),
          conditionalPanel(
            condition = "input.vitamin_status",
            style = "margin-left: 20px;",
            checkboxGroupInput("vitamin_status_selection", label = NULL)
          )
        ),
        column(
          width = 3,
          actionButton(
            inputId = "data_selection_button",
            label = "Apply",
            style = "font-size: 25px;"
          ),
          align = "center",
          style = "margin-top: 50px;"
        )
      )
    ),
    
    # Standard comparison tab
    tabItem(
      tabName = "standard_comparison",
      h1("Standard comparison"),
      sidebarPanel(
        h4("Subject to compare"),
        radioButtons(
          "subject_to_compare_standard",
          label = NULL,
          choices = c("")
        ),
        h4("Comparison variable"),
        radioButtons(
          "comparison_variable_standard",
          label = NULL,
          choices = c("")
        ),
        h4("Comparison dates"),
        checkboxGroupInput(
          inputId = "comparison_dates_standard",
          label = NULL,
          choices = NULL
        ),
        column(
          width = 12,
          actionButton(inputId = "comparison_button_standard", label = "Apply"),
          align = "center",
          style = "margin-top: 30px;"
        )
      ),
      mainPanel(
        plotOutput("standard_comparison_plot"),
        tags$p(
          h4("Information:"),
          "The dotted horizontal red lines represent the recommended limits from the literature for measuring variable. A value outside this range may be considered a statistical outlier or an alarm signal. If they do not appear, it is probably because there are no recommended values indicated in the literature."
        )
      )
    ),
    
    # Inter-individual comparison tab
    tabItem(
      tabName = "interindividual_comparison",
      h1("Inter-individual comparison"),
      sidebarPanel(
        h4("Subject to compare"),
        radioButtons(
          "subject_to_compare_interindividual",
          label = NULL,
          choices = c("")
        ),
        h4("Comparison subjects"),
        checkboxGroupInput(
          inputId = "comparison_subjects_interindividual",
          label = NULL,
          choices = NULL
        ),
        h4("Comparison variable"),
        radioButtons(
          "comparison_variable_interindividual",
          label = NULL,
          choices = c("")
        ),
        h4("Comparison dates"),
        checkboxGroupInput(
          inputId = "comparison_dates_interindividual",
          label = NULL,
          choices = NULL
        ),
        column(
          width = 12,
          actionButton(inputId = "comparison_button_interindividual", label = "Apply"),
          align = "center",
          style = "margin-top: 30px;"
        )
      ),
      mainPanel(
        plotOutput("interindividual_comparison_plot"),
        tags$p(
          h4("Information:"),
          "The dotted horizontal blue lines represent the limits of a 95% confidence interval, calculated as the mean plus or minus 2 times the standard deviation, for the measurements of the comparison subjects. A value outside this interval may be considered as a statistical outlier or a warning signal."
        )
      )
    ),
    
    # Correlation analysis tab
    tabItem(
      tabName = "correlation_analysis",
      h1("Correlation analysis"),
      h3("Correlation coefficients"),
      dataTableOutput("correlation_table"),
      h3("Correlation pie charts"),
      plotOutput("correlation_plot")
    ),
    
    # Raw data tab
    tabItem(tabName = "raw_data",
            h1("Raw data"),
            DTOutput("data_table"))
    
  ))
)