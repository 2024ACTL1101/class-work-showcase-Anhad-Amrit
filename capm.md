
# CAPM Analysis


## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.
```{r packages}
library(quantmod)
library(ggplot2)
library(tidyverse)
```
The code chunk above assures that the needed packages are applied in the library after they have been installed.
```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, 
                       to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = 
                          end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, 
                      auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  
# above is accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```
First, objects for the start and end dates of analysis are created. Then specific column data is loaded into an object and then converted into a data frame for the dated region provided. Finally, the different data frames are merged into one data frame.
#### Data Processing 
```{r data}
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```
We can see from this output, that there were 9 N/A elements in the RF column, which were filled by the code chunk below the output.

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

\[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \]

Where:

- \( E(R_i) \) is the expected return on the capital asset,
- \( R_f \) is the risk-free rate,
- \( \beta_i \) is the beta of the security, which represents the systematic risk of the security,
- \( E(R_m) \) is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```{r return}
df <- df %>%
    mutate(AMD_Return = (AMD / lag(AMD) - 1),
    GSPC_Return = (GSPC / lag(GSPC) - 1))
```
Here, the pipe operator `%>%` allows for the data from the data frame made in Step 1 to be used to calculate the change in each days trading price.
The `AMD_Return` column gives us the daily return of the AMD stock, and the `GSPC_Return` column gives us the daily return of the S&P 500 index.

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}
df <- df %>%
  mutate(RF_Daily = (1 + RF/100)^(1/360) - 1)
```
Here, the annual risk free rate is RF, which is plugged directly into the formula given to form a new `RF_daily` column. 

- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
df <- df %>%
  mutate(AMD_Excess_Return = AMD_Return - RF_Daily,
         GSPC_Excess_Return = GSPC_Return - RF_Daily)
```
Using the direct subtraction of each daily return and the daily risk-free rate, new columns for the respective excess returns for both AMD stock and the S&P 500 index.

- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

Just before our regression analysis, we can remove an N/A values of our data frame:
```{r NA removal}
df <- df %>% na.omit()
```

```{r lm}
capm_model <- lm(AMD_Excess_Return ~ GSPC_Excess_Return, data = df)
summary(capm_model)
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
The \(\beta\) estimation from the analysis is 1.5696, as found in the Estimate column of the Coefficients table of the regression summary. Since the \(\beta\) value is greater than one, it is more volatile than the market. Essentially, it is implied that AMD's stock is approximately 57% more volatile than the market. The \(\beta\) value being greater than 1 also implies there is higher systematic risk with the stock, which further implies a higher expected return as a result.


#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.
First we can create a theme for the plot, to affect its visual style.
```{r theme}
theme_rbook <- function(base_size = 13,
                        base_family = "",
                        base_line_size = base_size/22,
                        base_rect_size = base_size/22) {
  theme(axis.title = element_text(size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.caption = element_text(size = 10, face = "italic"),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(linewidth = 1, colour = "black"),
        strip.background = element_rect(fill = "#cddcdd"),
        panel.border = element_rect(colour = "black", fill=NA),
        strip.text = element_text(colour = "black"),
        legend.key = element_blank()
  )
}
```
This code chuck will visually improve the plot with features such as a centered main title, removal of grid lines and more.
```{r plot}
ggplot(df, aes(x = GSPC_Excess_Return, y = AMD_Excess_Return)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "CAPM: AMD vs. S&P 500 Excess Returns",
       x = "S&P 500 Excess Return",
       y = "AMD Excess Return") +
theme_rbook()
```


**Answer:**

```r
#fill the code
```
