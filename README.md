# ETL Analysis Toolkit

The ETL Analysis Toolkit is a collection of R functions for performing Extract, Transform, Load (ETL) tasks and data analysis on CSV files.

## Functionalities

- **Merge CSV Files:** Combines two or more CSV files into a single file.
- **Filter CSV Data:** Filters data from a CSV file based on a provided condition.
- **Remove Duplicates from CSV:** Removes duplicate records from a CSV file.
- **Analyze Data Distribution:** Analyzes the distribution of a variable in a dataset.
- **Analyze Data Correlation:** Calculates and displays the correlation matrix between variables in a dataset.
- **Analyze Time Series:** Analyzes a time series, displaying the original series, trend, seasonal component, and residuals.
- **Analyze Linear Regression:** Performs simple linear regression between two variables and displays the regression model and plot.
- **Forecast Time Series with ARIMA:** Generates forecasts for a time series using the ARIMA model and displays the results.

## Dependencies

The toolkit requires the following libraries:

- readr
- ggplot2
- forecast

Make sure you have the libraries installed before running the code.

## Usage

1. Clone the repository or download the `etl_analysis_toolkit.R` file.
2. Open the file in an R environment (such as RStudio) or run it in an R terminal.
3. Make sure the required dependencies are installed.
4. Call the `main_menu()` function to start the main menu.
5. Choose an option by entering the corresponding number and follow the instructions to provide the required parameters.
6. The results will be displayed in the console or saved to files as specified.

## Notes

- Ensure you provide the correct file paths and valid parameters to avoid errors.
- The toolkit is designed to handle CSV files. Other file formats are not supported.

Enjoy exploring the ETL Analysis Toolkit!
