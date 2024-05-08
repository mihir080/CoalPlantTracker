# Coal Power Plant Impact Analysis

This script is designed to analyze the impact of coal power plants and their CO2 emissions. It encompasses data reading, cleaning, wrangling, and visualization. Below is a breakdown of each section:

## Package Installation

This section installs the necessary R packages required for data analysis and visualization. If any of these packages are not already installed, they will be automatically installed.

## Data Reading

Here, two datasets containing information about coal power plants from 2017 and 2023 are read into the script using the `read_excel()` function from the `readxl` package.

## Data Cleaning and Wrangling

The data cleaning and wrangling process includes subsetting the data to keep only relevant variables, merging the datasets, renaming variables, handling missing values, standardizing power plant operating status, and creating a consolidated dataset for analysis. The final cleaned dataset is then exported to an Excel file named "FinalDataset.xlsx".

## Data Visualization

This section includes various data visualization techniques to understand the impact of coal power plants:

- **Plotting Change in Total Global Capacity**: Visualizes the change in total global capacity of coal power plants from 2017 to 2023.
- **Changes in Operating Capacity**: Shows the change in operating capacity and the number of power plants by country.
- **Power Plants Under Development**: Illustrates the number of power plants under development and those that have been canceled by the country.
- **Annual CO2 Emissions**: Presents the annual CO2 emissions by country on world maps for 2017 and 2023, both with and without outliers.

Each plot provides insights into the trends and changes in coal power plant operations and their environmental impact over time.

## Usage

To utilize this script, follow these steps:

1. Install the required R packages listed in the "Package Installation" section.
2. Place the datasets ("Global-Coal-Plant-Tracker-Jan-2017.xlsx" and "Global-Coal-Plant-Tracker-July-2023.xlsx") in the same directory as the script.
3. Run the script sequentially, ensuring that each section executes without errors.
4. Review the generated visualizations and exported dataset for insights into the impact of coal power plants.

## Notes

- Ensure that the dataset filenames and paths are correctly specified in the script.
- Customize the visualization parameters and plot aesthetics as needed for your analysis.

For any questions or assistance, feel free to reach out. Happy analyzing!
