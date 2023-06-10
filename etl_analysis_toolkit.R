library(readr)
library(ggplot2)
library(forecast)

merge_csv_files <- function(file_paths, output_file) {
  if (length(file_paths) < 2) {
    stop("Please provide at least two file paths.")
  }
  
  data <- NULL
  
  for (file_path in file_paths) {
    if (!file.exists(file_path)) {
      stop(paste("File not found:", file_path))
    }
    
    file_extension <- tools::file_ext(file_path)
    
    if (file_extension != "csv") {
      stop(paste("Unsupported file type:", file_extension, ". Please provide CSV files only."))
    }
    
    file_data <- read_csv(file_path)
    
    if (is.null(data)) {
      data <- file_data
    } else {
      data <- rbind(data, file_data)
    }
  }
  
  write_csv(data, output_file)
  cat("Merged data saved to:", output_file, "\n")
}

filter_csv_data <- function(input_file, output_file, condition) {
  if (!file.exists(input_file)) {
    stop("Input file not found. Please provide a valid file path.")
  }
  
  file_extension <- tools::file_ext(input_file)
  
  if (file_extension != "csv") {
    stop(paste("Unsupported file type:", file_extension, ". Please provide a CSV file."))
  }
  
  data <- read_csv(input_file)
  
  filtered_data <- subset(data, condition)
  
  write_csv(filtered_data, output_file)
  cat("Filtered data saved to:", output_file, "\n")
}

remove_duplicates_csv <- function(input_file, output_file) {
  if (!file.exists(input_file)) {
    stop("Input file not found. Please provide a valid file path.")
  }
  
  file_extension <- tools::file_ext(input_file)
  
  if (file_extension != "csv") {
    stop(paste("Unsupported file type:", file_extension, ". Please provide a CSV file."))
  }
  
  data <- read_csv(input_file)
  
  unique_data <- unique(data)
  
  write_csv(unique_data, output_file)
  cat("Data with duplicates removed saved to:", output_file, "\n")
}

analyze_distribution <- function(data, variable) {
  if (!(variable %in% names(data))) {
    stop("Variable not found in the dataset.")
  }
  
  variable_data <- data[[variable]]
  
  hist_plot <- ggplot(data, aes(x = variable_data)) +
    geom_histogram(fill = "steelblue", color = "white") +
    labs(title = paste("Distribution of", variable))
  
  density_plot <- ggplot(data, aes(x = variable_data)) +
    geom_density(fill = "steelblue", color = "white") +
    labs(title = paste("Density Plot of", variable))
  
  return(list(hist_plot = hist_plot, density_plot = density_plot))
}

analyze_correlation <- function(data) {
  corr_matrix <- cor(data)
  
  corr_plot <- ggplot(data, aes(x = factor(rownames(corr_matrix)), y = factor(colnames(corr_matrix)))) +
    geom_tile(aes(fill = corr_matrix), color = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Correlation Matrix")
  
  return(corr_plot)
}

analyze_time_series <- function(data, timestamp_column, value_column) {
  if (!(timestamp_column %in% names(data)) || !(value_column %in% names(data))) {
    stop("Invalid column names. Please provide valid timestamp and value columns.")
  }
  
  time_series <- ts(data[[value_column]], frequency = 1)
  
  decomposition <- decompose(time_series)
  
  plot_original <- ggplot(data, aes_string(x = timestamp_column, y = value_column)) +
    geom_line() +
    labs(title = "Original Time Series")
  
  plot_trend <- ggplot(data.frame(x = time(decomposition$trend), y = decomposition$trend), aes(x = x, y = y)) +
    geom_line() +
    labs(title = "Trend")
  
  plot_seasonal_random <- ggplot(data.frame(x = time(decomposition$seasonal), y = decomposition$seasonal), aes(x = x, y = y)) +
    geom_line() +
    labs(title = "Seasonal Component")
  
  plot_residuals <- ggplot(data.frame(x = time(decomposition$random), y = decomposition$random), aes(x = x, y = y)) +
    geom_line() +
    labs(title = "Residuals")
  
  return(list(
    plot_original = plot_original,
    plot_trend = plot_trend,
    plot_seasonal_random = plot_seasonal_random,
    plot_residuals = plot_residuals
  ))
}

analyze_linear_regression <- function(data, x_column, y_column) {
  lm_model <- lm(data[[y_column]] ~ data[[x_column]], data = data)
  
  regression_plot <- ggplot(data, aes_string(x = x_column, y = y_column)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
    labs(title = "Linear Regression") +
    theme_minimal()
  
  return(list(regression_model = lm_model, regression_plot = regression_plot))
}

forecast_arima <- function(data, value_column, forecast_horizon) {
  if (!(value_column %in% names(data))) {
    stop("Invalid column name. Please provide a valid value column.")
  }
  
  time_series <- ts(data[[value_column]], frequency = 1)
  
  arima_model <- auto.arima(time_series)
  
  forecast_data <- forecast(arima_model, h = forecast_horizon)
  
  forecast_plot <- plot(forecast_data, main = "ARIMA Forecast")
  
  return(list(arima_model = arima_model, forecast_data = forecast_data, forecast_plot = forecast_plot))
}

main_menu <- function() {
  while (TRUE) {
    cat("\n=== ETL Script Menu ===\n")
    cat("1. Merge CSV Files\n")
    cat("2. Filter CSV Data\n")
    cat("3. Remove Duplicates from CSV\n")
    cat("4. Analyze Data Distribution\n")
    cat("5. Analyze Data Correlation\n")
    cat("6. Analyze Time Series\n")
    cat("7. Analyze Linear Regression\n")
    cat("8. Forecast Time Series with ARIMA\n")
    cat("0. Exit\n")
    
    choice <- readline("Enter your choice (0-8): ")
    
    if (choice == "1") {
      file_paths <- readline("Enter file paths to merge (separated by spaces): ")
      file_paths <- strsplit(file_paths, " ")[[1]]
      output_file <- readline("Enter output file path: ")
      
      merge_csv_files(file_paths, output_file)
    } else if (choice == "2") {
      input_file <- readline("Enter input file path: ")
      output_file <- readline("Enter output file path: ")
      condition <- readline("Enter filter condition (e.g., column_name > 100): ")
      
      filter_csv_data(input_file, output_file, condition)
    } else if (choice == "3") {
      input_file <- readline("Enter input file path: ")
      output_file <- readline("Enter output file path: ")
      
      remove_duplicates_csv(input_file, output_file)
    } else if (choice == "4") {
      input_file <- readline("Enter input file path: ")
      data <- read_csv(input_file)
      
      variable <- readline("Enter variable name for analysis: ")
      
      plots <- analyze_distribution(data, variable)
      hist_plot <- plots$hist_plot
      density_plot <- plots$density_plot
      
      print(hist_plot)
      print(density_plot)
    } else if (choice == "5") {
      input_file <- readline("Enter input file path: ")
      data <- read_csv(input_file)
      
      corr_plot <- analyze_correlation(data)
      print(corr_plot)
    } else if (choice == "6") {
      input_file <- readline("Enter input file path: ")
      data <- read_csv(input_file)
      
      timestamp_column <- readline("Enter timestamp column name: ")
      value_column <- readline("Enter value column name: ")
      
      plots <- analyze_time_series(data, timestamp_column, value_column)
      plot_original <- plots$plot_original
      plot_trend <- plots$plot_trend
      plot_seasonal_random <- plots$plot_seasonal_random
      plot_residuals <- plots$plot_residuals
      
      print(plot_original)
      print(plot_trend)
      print(plot_seasonal_random)
      print(plot_residuals)
    } else if (choice == "7") {
      input_file <- readline("Enter input file path: ")
      data <- read_csv(input_file)
      
      x_column <- readline("Enter predictor variable column name: ")
      y_column <- readline("Enter target variable column name: ")
      
      regression_results <- analyze_linear_regression(data, x_column, y_column)
      regression_model <- regression_results$regression_model
      regression_plot <- regression_results$regression_plot
      
      print(summary(regression_model))
      print(regression_plot)
    } else if (choice == "8") {
      input_file <- readline("Enter input file path: ")
      data <- read_csv(input_file)
      
      value_column <- readline("Enter value column name: ")
      forecast_horizon <- as.integer(readline("Enter forecast horizon: "))
      
      forecast_results <- forecast_arima(data, value_column, forecast_horizon)
      arima_model <- forecast_results$arima_model
      forecast_data <- forecast_results$forecast_data
      forecast_plot <- forecast_results$forecast_plot
      
      print(summary(arima_model))
      print(forecast_plot)
    } else if (choice == "0") {
      cat("Exiting. Goodbye!\n")
      break
    } else {
      cat("Invalid choice. Please try again.\n")
    }
  }
}

main_menu()
