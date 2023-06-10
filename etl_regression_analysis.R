library(readxl)
library(ggplot2)

perform_etl <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File not found. Please provide a valid file path.")
  }
  
  file_extension <- tools::file_ext(file_path)
  
  if (!file_extension %in% c("csv", "xlsx", "xls")) {
    stop("Unsupported file type. Please provide a CSV or Excel file.")
  }
  
  data <- switch(file_extension,
                 csv = read.csv(file_path),
                 xlsx = read_excel(file_path),
                 xls = read_excel(file_path, sheet = 1))
  
  numeric_data <- Filter(is.numeric, data)
  
  correlated_pairs <- combn(names(numeric_data), 2, simplify = FALSE)
  
  for (pair in correlated_pairs) {
    x <- numeric_data[[pair[1]]]
    y <- numeric_data[[pair[2]]]
    
    correlation <- cor(x, y)
    
    if (correlation > 0.7) {
      model <- lm(y ~ x)
      
      plot_data <- data.frame(x = x, y = y)
      
      plot <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        labs(title = paste("Linear Regression:", pair[1], "vs", pair[2]),
             subtitle = paste("Correlation:", round(correlation, 2)))
      
      output_file <- paste0("regression_plot_", pair[1], "_vs_", pair[2], ".png")
      ggsave(output_file, plot)
      
      cat("Generated plot:", output_file, "\n")
    }
  }
}

file_path <- "path/to/file.xlsx"
perform_etl(file_path)
