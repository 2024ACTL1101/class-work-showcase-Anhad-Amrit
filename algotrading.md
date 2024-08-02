## Algorithmic trading strategy
## Step 1: Data Loading

To first begin the Algorithmic Trading Strategy, the data provided regarding AMD's stock prices must be loaded into RStudio.

```{r Data loading}
# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Conversion of the 'date' column into the Date data type and 'Adjusted Close' column 
# into the numeric data type.
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

Plotting the data can also help illustrate the trends of AMD's stock price, so it should be plotted as follows:

```{r plot}
plot(amd_df$date, amd_df$close,'l')
```

## Step 2: Trading Algorithm
Now we need to implement the trading algorithm as per the instructions. First, we create the new columns in our data frame for 'trade_type', 'cost proceeds' and 'accumulated_shares'. Then we introduce variables for the previous price, share size and accumulated shares to use for the trading logic.

```{r trading}
# Creating the new columns we will use for analysis.
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA
amd_df$accumulated_shares <- 0

# Initialize variables for trading logic. 
# The previous price starts at 0 so we can buy on the first day.
previous_price <- 0
# The share size stays at 100 due to the algorithm buying shares in sets of 100, 
# and initially the number of shares accumulated is 0.
share_size <- 100
accumulated_shares <- 0

# A for loop is used to apply the conditional logic to all rows of the data frame. 
# The logic is explained under this code chunk.
for (i in 1:nrow(amd_df)) {
  current_price <- amd_df$close[i]
  
  if (previous_price == 0) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -current_price * share_size
    accumulated_shares <- accumulated_shares + share_size
  } else if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- current_price * accumulated_shares
  } else if (current_price < previous_price) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -current_price * share_size
    accumulated_shares <- accumulated_shares + share_size
  } else if (current_price > previous_price) {
    amd_df$costs_proceeds[i] <- 0
  } 
  amd_df$accumulated_shares[i] <- accumulated_shares
  previous_price <- current_price
}

```
The code above follows the given trading algorithm, as it documents the costs and proceeds metrics for buys of 100 shares over the given time period. 

The trading algorithm buys on the first day as the previous price is zero, buys 100 shares when the current price is below the previous price, and sells on the last day, which is the last row of the data frame. 

To represent the buying and selling actions, the `trade_type` variable is changed in these cases. 

The value of the `trade_type` then affects the `cost_proceeds`, as when we buy the proceeds are the negative multiplication of the current price and 100, and if we don't buy due to the price being higher then the proceeds are 0. 

The program sells on the last day, which is given by the condition that `i` is the final row of the data frame. On this day, the `trade_type` is sell and the `cost_proceeds` is the multiplication of all accumulated shares and the `current_price`.

## Step 3: Customising the Trading Period
As the data set given is quite large, it is effective to define a smaller trading period to conduct our analysis. Let's choose our trading period from the beginning of 2020 to the end of 2021, to view what happened during COVID-19 years.
```{r customising}
# Define the custom trading period of 2 years.
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2021-12-31")

# Restrict the Data frame to only between these 2 dates.
amd_df <- subset(amd_df, date >= start_date & date <= end_date)
```
We can now run our code again to view what occurs in this period.

## Step 4: Run Algorithim and Analyse results
```{r running again}
# Re-run the trading algorithm on the filtered data
previous_price <- 0
accumulated_shares <- 0
share_size <- 100

for (i in 1:nrow(amd_df)) {
  current_price <- amd_df$close[i]
  
  if (previous_price == 0) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -current_price * share_size
    accumulated_shares <- accumulated_shares + share_size
  } #Since we restricted the rows of the Data frame in the previous step, 
  # the final row is now the last element in 2021.
  else if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- current_price * accumulated_shares
  }
  else if (current_price < previous_price) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -current_price * share_size
    accumulated_shares <- accumulated_shares + share_size
  } 
  else if (current_price > previous_price) {
    amd_df$costs_proceeds[i] <- 0
  } 
  amd_df$accumulated_shares[i] <- accumulated_shares
  previous_price <- current_price
}
```
This is the same algorithm that was used in Step 2, now having restricted the rows of the data frame `amd_df` to the stock prices from 2020-2021 in Step 3.

Now the profit or loss, invested capital and ROI figures must be calculated.

- Total Profit/Loss: This will be the sum of all entries in the `cost_proceeds` column of the data frame. A positive value reflects the money earned and a negative value indicates a loss in revenue. 

- Invested Capital: This is how much we have invested into the stock ourselves. To calculate this, simply sum all `cost_proceeds` elements when you buy, which can be done by using a sum function and if the trade type of the cell equals exactly 'buy'.

- ROI: Is calculated using the following formula:$$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$
By calculating the 2 figures above first, we can just apply this formula to find the ROI.

Provided below is the R code used to calculate and print these 3 figures.
```{r calculations}
# Total Profit/Loss Calculation. Since we applied a 0 value to all cases of not 
# buying, we do not need to remove NA elements.
total_profit_loss <- sum(amd_df$costs_proceeds)

# Invested Capital Calculation
Invested_Capital <- -sum(amd_df$costs_proceeds[amd_df$trade_type == 'buy'], na.rm = TRUE)

# ROI Calculation using the formula
roi <- (total_profit_loss / Invested_Capital) * 100

# Print results
total_profit_loss
Invested_Capital
roi

```

Interpreting these figures we can see that we have made a 1.520474 million dollars profit and invested 2.062636 million dollars into AMD over the 2 years. This gives us a 73.72% ROI. When applying these calculations to the whole data set (not just 2020-21) we have a higher ROI of approximately 85%. This higher ROI implies that years after 2021 have higher increases for AMD stock, which is supported by the graph we plotted at the beginning of this document.

## Step 5: Profit-Taking Strategy

- A profit taking strategy is used to retain a portion of profits while the stock is higher than the average purchase price, in an attempt to increase ROI in case of a sudden crash. In this strategy, half of the current holdings are sold if the price has increased by a certain percentage. I am going to choose the percentage of 20%.

For the implementation of this new algorithm, we continue with the 

```{r profit taking}
# Set the profit-taking threshold
profit_threshold <- 1.2

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
total_spent <- 0
# Previous trade allows for logic to ensure the accumulated shares are 
# correctly displayed.
previous_trade <- "none"

for (i in 1:nrow(amd_df)) {
  current_price <- amd_df$close[i]
  
  if (accumulated_shares > 0) {
    # Calculate the running average price
    running_average_price <- total_spent / accumulated_shares
    threshold_price <- running_average_price * profit_threshold
  } else {
    running_average_price <- 0
    threshold_price <- 0
  }
  
  # Check if a profit-taking condition is met
  if (current_price >= threshold_price && accumulated_shares > 0) {
    # Sell half of the holdings
    amd_df$trade_type[i] <- 'sell'
    shares_to_sell <- accumulated_shares / 2
    amd_df$costs_proceeds[i] <- (current_price * shares_to_sell)
    total_spent <- total_spent - (running_average_price * shares_to_sell)
        accumulated_shares <- accumulated_shares - shares_to_sell
            previous_trade <- 'sell'
  } else if (i == nrow(amd_df)) {
    # Sell all holdings on the last day
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- current_price * accumulated_shares
    previous_trade <- 'sell'
    accumulated_shares <- 0  # All shares sold
    total_spent <- 0
  }else if (previous_price == 0 || current_price < previous_price) {
    # Buy if the price is lower than the previous day, as in Step 2
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -current_price * share_size
    if (previous_trade == 'sell') {
      # Half the accumulated shares if previous trade was a sell
      accumulated_shares <- (accumulated_shares / 2) + share_size
      total_spent <- (total_spent / 2) + (current_price * share_size)
    } else {
      accumulated_shares <- accumulated_shares + share_size
      total_spent <- total_spent + (current_price * share_size)
    }
    previous_trade <- 'buy'
  }
  
  amd_df$accumulated_shares[i] <- accumulated_shares
  previous_price <- current_price
}


```
We can once again apply our P/L, Invested Capital and ROI code as follows:
```{r cals}
# Total Profit/Loss Calculation. Since we applied 0 to all cases of not 
# buying, we do not need to remove NA elements.
total_profit_loss <- sum(amd_df$costs_proceeds)

# Invested Capital Calculation
Invested_Capital <- -sum(amd_df$costs_proceeds[amd_df$trade_type == 'buy'], na.rm = TRUE)

# ROI Calculation using the formula
roi <- (total_profit_loss / Invested_Capital) * 100

# Print results
total_profit_loss
Invested_Capital
roi

```

Clearly these metrics are much lower than the original algorithm of buying when price decreases and only selling in the final period, so lets discuss the differences in Step 6.

## Step 6: Summarize Your Findings
We can see by comparing our results in Step 4, with a Profit equal to 1520474 dollars and an ROI of 73.72%, while with the Profit-Taking Strategy we have the Profit of 69714 and ROI of 20.69%. The ROI and profit have both decreased, and there are many reasons as to why.

- Firstly, the level of investment is lower as the algorithm is selling more regularly which doesn't allow us to gather as many shares, and as a result limits the potential return, possibly leading to the lower profit and ROI. The profit-taking strategy has the algorithm sell more than just once at the end, and you cannot buy in periods when you sell. As a result, there are less opportunities to buy overall, leading to lower investment. This is seen clearly as the amount invested is 336970 in the second strategy, much lower than the 2062636 invested in the original algorithm. A lower level of investment can result in a lower overall profit.

- Another potential reason for the lower ROI is the higher growth of the stock price during 2021. Since the running average price is based on previous average bought and sold metrics, as AMD's stock price rises from 2020's figures to 2021, as shown in the plot below, the prices will begin to exceed the average continuously. Since this occurs many times in a row, the amount of accumulated shares will continuously be halved. This decay of accumulated shares results in less return as the stock increases in price, as the algorithm preemptively extracts the growth without holding on to the improving AMD stock.
```{r plot2}
plot(amd_df$date, amd_df$close,'l')
```

AMD's sudden increase from 2020 to 2021 in stock price can be related to a major market event. After the world was effected by COVID, many markets boomed in 2021 as restrictions eased and more consumers were willing to spend money on non-essential goods. Since AMD processors aren't entirely essential to the average consumer, this reason for 2021 success would also apply to the company. In addition to this, in late 2021 it was reported that AMD was expected to close a deal with the circuitry engineer Xilinix in the first quarter of 2022, and many investors may have started buying shares in anticipation for the stocks growth when this deal closed.

Overall, the analysis of these algorithms on historical AMD stock show that Profit-Taking strategies reduce ROI and overall Profit when implemented onto a flourishing stock.



