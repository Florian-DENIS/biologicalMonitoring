# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)

# Define server
server <- function(input, output, session) {
  
  # Function to select data based on subjects, dates, and variables
  selectData <- function(data_frame, subjects, dates, variables) {
    df <- as.data.frame(data_frame)
    result_df <-
      df %>% filter(row_number() <= 2 | df[, 1] %in% variables)
    subject_columns <-
      c('1', colnames(result_df)[sapply(result_df[1,], function(x)
        any(x %in% subjects))])
    result_df <- result_df[, subject_columns]
    date_columns <-
      c('1', colnames(result_df)[sapply(result_df[2,], function(x)
        any(x %in% dates))])
    result_df <- result_df[, date_columns]
    return(result_df)
  }
  
  # Function to get a biological value for a specific subject, date, and variable
  getBiologicalValue <-
    function(data_frame, subject, date, variable) {
      variable_df <-
        data_frame %>% filter(row_number() <= 2 |
                                data_frame[, 1] == variable)
      subject_columns <-
        names(variable_df)[variable_df[1,] %in% subject]
      subject_df <- variable_df %>% select(all_of(subject_columns))
      date_column <- which(subject_df[2,] == date)
      value <-
        as.numeric(subject_df[3, date_column])
      return(value)
    }
  
  # Function to get data for a single subject, multiple dates and a single variable
  getSingleSubjectData <- function(subject, variable, dates) {
    values <- c()
    for (date in dates) {
      value <-
        getBiologicalValue(reactive_values$file_data, subject, date, variable)
      values <- c(values, value)
    }
    data.frame(
      Subject = rep(subject, each = length(dates)),
      Variable = rep(variable, each = length(dates)),
      Date = dates,
      Value = values,
      Group = rep("Subject to compare", each = length(dates))
    )
  }
  
  # Function to get data for multiple subjects, multiple dates and a single variable
  getMultipleSubjectData <- function(subjects, variable, dates) {
    values <- c()
    for (subject in subjects) {
      for (date in dates) {
        value <-
          getBiologicalValue(reactive_values$file_data, subject, date, variable)
        values <- c(values, value)
      }
    }
    data.frame(
      Subject = rep(subjects, each = length(dates)),
      Variable = rep(variable, length(subjects) * length(dates)),
      Date = rep(dates, length(subjects)),
      Value = values,
      Group = "Comparison subjects"
    )
  }
  
  # Update the database based on the selected data file
  observe({
    
    # Check if a data file has been selected
    if (!is.null(input$data_file)) {
      
      # Read data from the selected Excel file
      data <- as.data.frame(read_excel(input$data_file$datapath, col_names = FALSE))
      
      # Rename columns with consecutive numbers
      colnames(data) <- 1:ncol(data)
      
      # Concatenate variable and unit columns
      data$'1' <- with(data, ifelse(
        is.na(data$'2'),
        data$'1',
        paste0(data$'1', " (", data$'2', ")")
      ))
      data <- subset(data, select = -c(2))
      colnames(data) <- 1:ncol(data)
      
      # Remove "<" characters and replace "," with "." in the data
      data[] <- lapply(data, function(column) gsub("<", "", column))
      data[] <- lapply(data, function(column) gsub(",", ".", column))
      
      # Convert numeric values representing dates to Date format
      data[2, -1] <- format(as.Date(as.numeric(data[2, -1]), origin = "1899-12-30"), "%Y-%m-%d")
      
      # Check for and handle Neutrophils-to-Lymphocyte Ratio (NLR) case
      if (all(c("Neutrophils (%)", "Lymphocytes (%)") %in% data$'1')) {
        neutrophils_index <- which(data$'1' == "Neutrophils (%)")
        lymphocytes_index <- which(data$'1' == "Lymphocytes (%)")
        NLR_index <- max(neutrophils_index, lymphocytes_index) + 1
        
        neutrophils_data <- apply(data[neutrophils_index, 2:ncol(data)], 2, as.numeric)
        lymphocytes_data <- apply(data[lymphocytes_index, 2:ncol(data)], 2, as.numeric)
        
        ratios <- round(neutrophils_data / lymphocytes_data, 3)
        
        NLR_row <- data.frame(t(c(
          "Neutrophils-to-Lymphocyte Ratio", ratios
        )))
        colnames(NLR_row) <- 1:ncol(NLR_row)
        
        data <- rbind(data[1:NLR_index - 1, ],
                      NLR_row,
                      data[NLR_index:nrow(data), ])
      }
      
      # Check for and handle Testosterone/Cortisol Ratio (TCR) case
      if (all(c("Testosterone (ng/dL)", "Cortisol (\u00B5g/dL)") %in% data$'1')) {
        testosterone_index <- which(data$'1' == "Testosterone (ng/dL)")
        cortisol_index <- which(data$'1' == "Cortisol (\u00B5g/dL)")
        TCR_index <- max(testosterone_index, cortisol_index) + 1
        
        testosterone_data <- apply(data[testosterone_index, 2:ncol(data)], 2, as.numeric)
        cortisol_data <- apply(data[cortisol_index, 2:ncol(data)], 2, as.numeric)
        
        ratios <- round(testosterone_data / cortisol_data, 3)
        
        TCR_row <- data.frame(t(c(
          "Testosterone/Cortisol Ratio", ratios
        )))
        colnames(TCR_row) <- 1:ncol(TCR_row)
        
        data <- rbind(data[1:TCR_index - 1, ],
                      TCR_row,
                      data[TCR_index:nrow(data), ])
      }
      
      # Update the reactiveValues with the processed data
      reactive_values$file_data <- data
      reactive_values$user_data <- data
    }
  })
  
  # Update subject selection based on the selected data
  observe({
    
    # Ensure that file_data in reactive_values is not null
    req(reactive_values$file_data)
    
    # Extract subjects from the data
    subject_row <- subset(reactive_values$file_data, reactive_values$file_data[, 1] == "Subject")
    subjects <- unique(unlist(subject_row[, -1]))
    
    # Update the subject selection checkbox group input
    updateCheckboxGroupInput(session, "subject_selection", choices = subjects, selected = subjects)
  })
  
  # Update date selection based on the selected data
  observe({
    # Ensure that file_data in reactive_values is not null
    req(reactive_values$file_data)
    
    # Extract dates from the data
    date_row <- subset(reactive_values$file_data, reactive_values$file_data[, 1] == "Collection date")
    dates <- unique(unlist(date_row[, -1]))
    
    # Update the date selection checkbox group input
    updateCheckboxGroupInput(session, "date_selection", choices = dates, selected = dates)
  })
  
  # Update variable selection based on the selected data
  observe({
    
    # Ensure that file_data in reactive_values is not null
    req(reactive_values$file_data)
    
    # Extract variable groups from the data
    variable_groups <- subset(
      reactive_values$file_data,
      !is.na(reactive_values$file_data[, 1]) &
        is.na(reactive_values$file_data[, 3])
    )[, 1]
    variable_groups <- unique(variable_groups)
    
    # Iterate through variable groups and update corresponding checkbox groups
    for (group in variable_groups) {
      group_variables <- character()
      
      index <- which(reactive_values$file_data[, 1] == group) + 1
      
      # Extract variables within the current group
      while (!(reactive_values$file_data[index, 1] %in% variable_groups) &
             index != nrow(reactive_values$file_data) + 1) {
        if (!is.na(reactive_values$file_data[index, 1])) {
          group_variables <- c(group_variables, reactive_values$file_data[index, 1])
        }
        index <- index + 1
      }
      
      # Construct checkbox group input name
      checkBox_name <- paste0(gsub(" ", "_", tolower(group)), "_selection")
      
      # Update checkbox group input for the current group
      if (!is.null(group_variables) && length(group_variables) > 0) {
        updateCheckboxGroupInput(session, checkBox_name, choices = group_variables)
      }
    }
  })
  
  # Store user-selected data when the data selection button is clicked
  observeEvent(input$data_selection_button, {
    
    # Ensure that required inputs are selected
    req(
      input$subject_selection,
      input$date_selection,
      input$anthropometric |
        input$performance |
        input$serum_chemistry_blood |
        input$whole_blood_analysis |
        input$hematologic_and_iron_status_indicators |
        input$hormones |
        input$vitamin_status
    )
    
    # Update subjects chosen by the user
    reactive_values$user_subjects <- input$subject_selection
    
    # Update dates chosen by the user
    reactive_values$user_dates <- input$date_selection
    
    # Update variables chosen by the user
    variables <- character()
    if (input$anthropometric) {
      variables <- c(variables, input$anthropometric_selection)
    }
    if (input$performance) {
      variables <- c(variables, input$performance_selection)
    }
    if (input$serum_chemistry_blood) {
      variables <- c(variables, input$serum_chemistry_blood_selection)
    }
    if (input$whole_blood_analysis) {
      variables <- c(variables, input$whole_blood_analysis_selection)
    }
    if (input$hematologic_and_iron_status_indicators) {
      variables <-
        c(variables,
          input$hematologic_and_iron_status_indicators_selection)
    }
    if (input$hormones) {
      variables <- c(variables, input$hormones_selection)
    }
    if (input$vitamin_status) {
      variables <- c(variables, input$vitamin_status_selection)
    }
    reactive_values$user_variables <- variables
    
    # Update user-selected data in reactive values
    reactive_values$user_data <-
      selectData(
        reactive_values$file_data,
        reactive_values$user_subjects,
        reactive_values$user_dates,
        reactive_values$user_variables
      )
  
    ## Update inter-individual comparison options
    
    # Update subject to compare options
    updateRadioButtons(
      session,
      "subject_to_compare_interindividual",
      choices = reactive_values$user_subjects,
      selected = reactive_values$user_subjects[1]
    )
    
    # Update comparison subjects options
    updateCheckboxGroupInput(
      session,
      "comparison_subjects_interindividual",
      choices = reactive_values$user_subjects,
      selected = reactive_values$user_subjects
    )
    
    # Update comparison variable options
    updateRadioButtons(
      session,
      "comparison_variable_interindividual",
      choices = reactive_values$user_variables,
      selected = reactive_values$user_variables[1]
    )
    
    # Update comparison dates options
    updateCheckboxGroupInput(
      session,
      "comparison_dates_interindividual",
      choices = reactive_values$user_dates,
      selected = reactive_values$user_dates
    )
    
    ## Update standard comparison options
    
    # Update subject to compare options
    updateRadioButtons(
      session,
      "subject_to_compare_standard",
      choices = reactive_values$user_subjects,
      selected = reactive_values$user_subjects[1]
    )
    
    # Update comparison variable options
    updateRadioButtons(
      session,
      "comparison_variable_standard",
      choices = reactive_values$user_variables,
      selected = reactive_values$user_variables[1]
    )
    
    # Update comparison dates options
    updateCheckboxGroupInput(
      session,
      "comparison_dates_standard",
      choices = reactive_values$user_dates,
      selected = reactive_values$user_dates
    )
  })
  
  # Plot bibliographic standard comparison when the button is clicked
  observeEvent(input$comparison_button_standard, {
    
    # Check if required inputs are available
    req(
      reactive_values$file_data,
      input$subject_to_compare_standard,
      input$comparison_variable_standard,
      input$comparison_dates_standard
    )
    
    # Extract inputs
    subject_to_compare <- input$subject_to_compare_standard
    comparison_variable <- input$comparison_variable_standard
    comparison_dates <- input$comparison_dates_standard
    
    # Render the standard comparison plot
    output$standard_comparison_plot <- renderPlot({
      # Generate data for the subject to compare
      subject_to_compare_data <-
        getSingleSubjectData(subject = subject_to_compare,
                             variable = comparison_variable,
                             dates = comparison_dates)
      
      # Define custom color for the group
      custom_color <- c("Subject to compare" = "red")
      
      # Check if comparison_variable is present in bibliographic_standard_bounds
      if (comparison_variable %in% bibliographic_standard_bounds$Variable) {
        # Define bounds
        lower_bound <-
          bibliographic_standard_bounds$Minimum[bibliographic_standard_bounds$Variable == comparison_variable]
        upper_bound <-
          bibliographic_standard_bounds$Maximum[bibliographic_standard_bounds$Variable == comparison_variable]
        
        # Plot
        ggplot(subject_to_compare_data,
               aes(
                 x = Date,
                 y = Value,
                 color = Group
               )) +
          geom_point(size = 5) +
          geom_hline(
            yintercept = lower_bound,
            linetype = "dashed",
            color = "red"
          ) +
          geom_hline(
            yintercept = upper_bound,
            linetype = "dashed",
            color = "red"
          ) +
          labs(
            title = paste(
              "Evolution of",
              toString(comparison_variable),
              "over time"
            ),
            x = "Dates",
            y = comparison_variable
          ) +
          theme_minimal() +
          scale_y_continuous(expand = c(0, 0),
                             limits = c(min(0, lower_bound), 1.1 * max(
                               upper_bound, max(subject_to_compare_data$Value)
                             ))) +
          scale_color_manual(values = custom_color)
      } else {
        # If comparison_variable is not present, plot without lines
        ggplot(subject_to_compare_data,
               aes(
                 x = Date,
                 y = Value,
                 color = Group
               )) +
          geom_point(size = 5) +
          labs(
            title = paste(
              "Evolution of",
              toString(comparison_variable),
              "over time"
            ),
            x = "Dates",
            y = comparison_variable
          ) +
          theme_minimal() +
          scale_y_continuous(expand = c(0, 0),
                             limits = c(0, 1.1 * max(subject_to_compare_data$Value))) +
          scale_color_manual(values = custom_color)
      }
    })
  })
  
  # Plot inter-individual comparison when the button is clicked
  observeEvent(input$comparison_button_interindividual, {
    
    # Check if required inputs are available
    req(
      reactive_values$file_data,
      input$subject_to_compare_interindividual,
      input$comparison_subjects_interindividual,
      input$comparison_variable_interindividual,
      input$comparison_dates_interindividual
    )
    
    # Extract inputs
    subject_to_compare <- input$subject_to_compare_interindividual
    comparison_subjects <- input$comparison_subjects_interindividual
    comparison_variable <- input$comparison_variable_interindividual
    comparison_dates <- input$comparison_dates_interindividual
    
    # Render the inter-individual comparison plot
    output$interindividual_comparison_plot <- renderPlot({
      # Generate data for the subject to compare
      subject_to_compare_data <-
        getSingleSubjectData(subject = subject_to_compare,
                             variable = comparison_variable,
                             dates = comparison_dates)
      
      # Generate data for comparison subjects
      comparison_subjects_data <-
        getMultipleSubjectData(subjects = comparison_subjects,
                               variable = comparison_variable,
                               dates = comparison_dates)

      # Combine both datasets for plotting
      plot_data <- rbind(comparison_subjects_data, subject_to_compare_data)
      
      # Calculate comparison values
      mean_value <- mean(comparison_subjects_data$Value)
      standard_deviation <- sd(comparison_subjects_data$Value)
      lower_bound <- mean_value - 2 * standard_deviation
      upper_bound <- mean_value + 2 * standard_deviation
      
      # Define custom colors for the groups
      custom_colors <- c(
        "Subject to compare" = "red",
        "Comparison subjects" = "skyblue"
      )
      
      # Plot
      ggplot(plot_data, aes(x = Date, y = Value, color = Group)) +
        geom_point(size = 5) +
        geom_hline(
          yintercept = lower_bound,
          linetype = "dashed",
          color = "skyblue"
        ) +
        geom_hline(
          yintercept = upper_bound,
          linetype = "dashed",
          color = "skyblue"
        ) +
        labs(
          title = paste(
            "Evolution of",
            toString(comparison_variable),
            "over time"
          ),
          x = "Dates",
          y = comparison_variable
        ) +
        theme_minimal() +
        scale_y_continuous(expand = c(0, 0),
                           limits = c(min(0, lower_bound), 1.1 * max(
                             upper_bound,
                             max(
                               subject_to_compare_data$Value,
                               comparison_subjects_data$Value
                             )
                           ))) +
        scale_color_manual(values = custom_colors)
    })
  })
  
  # Display correlation coefficients and pie charts
  observe({
    
    # Check if required data is available
    req(reactive_values$user_data,
        reactive_values$user_variables)
    
    # Check if there are more than one variable for correlation analysis
    if (length(reactive_values$user_variables) > 1) {
      # Calculating correlation matrix
      correlation_data <- reactive_values$user_data[-c(1, 2),]
      variables <- correlation_data[, 1]
      individual_statements <-
        apply(correlation_data[, -1], 2, as.numeric)
      
      # Prepare data for processing
      correlation_data <- t(individual_statements)
      colnames(correlation_data) <- t(variables) 
      
      # Calculate the correlation matrix
      correlation_matrix <- cor(correlation_data)
      
      # Render the correlation plot
      output$correlation_plot <- renderPlot({
        corrplot(correlation_matrix,
                 method = "pie",
                 type = "lower",
                 col = colorRampPalette(brewer.pal(11, "RdYlGn"))(10),
                 tl.col = "black",
                 tl.srt = 45
        )
      })
      
      # Format data to be displayed
      correlation_matrix <- round(correlation_matrix, 3)
      rownames(correlation_matrix) <- variables
      
      # Render the correlation matrix as a data table
      output$correlation_table <- renderDataTable({
        datatable(correlation_matrix)
      })
    }
  })
  
  # Render the data table
  output$data_table <- renderDT({
    req(reactive_values$user_data)
    datatable(reactive_values$user_data)
  })
}