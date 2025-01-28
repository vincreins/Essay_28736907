---
output:
  md_document:
    variant: markdown_github
---

### Risk Parity Portfolios (RPPs): A Comparative Study in South Africa  

#### Introduction  
Risk parity portfolios (RPPs) aim to equalize the risk contribution of each asset within a portfolio. This strategy contrasts with modern portfolio theory (MPT)-based methods, such as the mean-variance approach, which rely on assumptions about expected returns and variance. RPPs bypass the need for return forecasts, instead emphasizing risk estimates, particularly volatility and correlations.  

Unlike MPT, which focuses on either maximizing returns or minimizing variance, RPPs prioritize explicit risk allocation. This research investigates the performance of RPPs relative to South African index funds, posing the key question:  

**How can the risk parity approach be effectively implemented to construct a diversified portfolio of South African assets, and how does it compare to traditional asset allocation strategies in terms of risk-adjusted returns?**  

#### Focus on Risk-Adjusted Returns  
The study aims to evaluate risk-adjusted returns, a core objective of RPPs. To achieve this, an expanded set of performance metrics will be analyzed:  
- Sharpe ratio  
- Sterling ratio  
- Alpha and Beta  
- Sortino ratio  
- Maximum drawdown  
- Calmar ratio  
- Information ratio (vs. a relevant benchmark)  

#### Asset Selection  
For constructing a South African RPP, a diverse range of traded assets will be included:  
- Stocks listed on the Johannesburg Stock Exchange  
- South African government bond yields (2-year and 10-year maturities)  

#### Incorporating Economic Regimes  
To enhance analytical rigor, portfolio performance will be examined across South Africa’s distinct economic regimes, such as:  
- Periods of high and low economic growth  
- Periods of high and low inflation  

This evaluation will illuminate the adaptability of risk parity strategies compared to traditional asset allocation methods under varying market conditions.  

#### Conclusion  
By integrating additional performance metrics and analyzing economic regimes, this study will deliver a thorough evaluation of the risk parity approach within the South African context. The findings aim to deepen understanding of RPP effectiveness in constructing diversified portfolios and their comparative performance against traditional strategies under diverse market scenarios.  

```{r}

rm(list = ls()) # Clean your environment:
gc() # garbage collection - It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.
library(tidyverse)
list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

Loading the necessary libraries and data sets. 
```{r, warning=FALSE}
library(PerformanceAnalytics)
library(riskParityPortfolio)
#library(fPortfolio)
library(dplyr)
library(tidyr)
library(xts)
library(lubridate)
library(kableExtra)
library(RiskPortfolios)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(zoo)
library(DEoptim)
LCL_Stock_Returns <- read_rds("/Users/x/Downloads/Data_2024/LCL_Stock_Returns.rds")
BondYields_10Y_SA <- read_rds("/Users/x/Downloads/Data_2024/BondYields_10Y_SA.rds")
BondYields_2Y_SA <- read_rds("/Users/x/Downloads/Data_2024/BondYields_2Y_SA.rds")
SA_Growth <- read_csv2("/Users/x/Downloads/SA_Growth.csv", show_col_types = FALSE)
```

In a first step, the filter_stock_function will be applied to the LCL_Stock_Returns dataset. The function filters and transforms stock data by removing weekends, reshaping data into a wide format with tickers as columns, dropping columns with only missing values or constant values, and converting the result into an xts time-series object.

```{r}
LCL_filtered <- filter_stock_function(LCL_Stock_Returns)
```

In the following chunk, the weights and returns for the MVP and the MDP are calculated. The weights for the MVP are calculated using the rolling_min_vol_portfolio_weights function and for the MDP the most_diversified_weights function. 

The MVP function calculates portfolio weights for a rolling minimum volatility strategy. The function optimizes portfolio weights over a specified window of historical returns window_size and adjusts these weights at regular intervals defined by the rebalance_interval. Key constraints ensure the portfolio remains fully invested, uses only long positions, and individual asset weights are bounded between 0 and 1. At each rebalance date, the portfolio optimization uses the most recent window_size days of data, employing a variance minimization objective. The resulting weights are normalized, stored as an xts time-series object, and validated to ensure they are non-negative and sum to one for every rebalancing period.

The MDP function calculates portfolio weights for the MDP strategy. This function optimizes weights to maximize the diversification ratio, which is the ratio of the weighted sum of asset volatilities to the portfolio's overall volatility. The portfolio is rebalanced at regular intervals (rebalance_interval). Asset weights are constrained to be non-negative and bounded between 0 and 1. Optimization is performed using the Differential Evolution (DEoptim) algorithm, with covariance and standard deviation of asset returns used as inputs. 

The returns for each portfolio are then calculated using the Return.portfolio package from the Performance Analytics package.

```{r}
weights_mv <- min_vol_portfolio_weights(LCL_filtered, 252, 63)
returns.mv <- Return.portfolio(LCL_filtered, weights = weights_mv, verbose = TRUE)
weights_md <- most_diversified_weights(LCL_filtered, window_size = 252, rebalance_interval = 63) 
returns.md <- Return.portfolio(LCL_filtered, weights = weights_md, verbose = TRUE)
```

In order to include the bonds in one of the portfolios, the returns of the bonds has to be calculated. THis is done using the Bond_return function. Calculates bond returns based on yield and maturity. The function processes the input data by selecting relevant columns, renaming the yield column, and calculating bond prices using the formula 
$$ \text{BondPrice} = \frac{1}{(1 + \text{Yield}/100)^\text{Maturity}} . $$

```{r}
Bond_return_10 <- Bond_return(BondYields_10Y_SA, "BondYield_10", 10)
Bond_return_2 <- Bond_return(BondYields_2Y_SA, "BondYield_2", 2)
```

After merging the bond and stock returns the covariance matrix is calculated.

```{r}
LCL_Bond <- merge.xts(Bond_return_10, Bond_return_2, LCL_filtered, join = "inner")
Sigma <- cov(LCL_Bond)
```

The riskParityPortfolio function from the same named package will be used to determine the weights for the RPP. This function is embedded in the RollingRiskParity function which rebalances the portfolio every three months. 

```{r}
portfolio.parity <- riskParityPortfolio(Sigma)
rWindows<-rollingWindows(LCL_Bond, period="12m",
                         by="3m")
parity.weights <- RollingRiskParity(rWindows$from@Data, rWindows$to@Data, LCL_Bond) %>% na.omit()
parity.returns <- Return.portfolio(LCL_Bond, weights=parity.weights,verbose=TRUE)
```

The same steps are repeated for the portfolio without bonds. For further analysis all portfolios are then merged to one data frame.

```{r}
Sigma_nb <- cov(LCL_filtered)
portfolio.parity.nb <- riskParityPortfolio(Sigma_nb)
rWindows.nb<-rollingWindows(LCL_filtered, period="12m",
                         by="3m")
parity.weights.nb <- RollingRiskParity(rWindows.nb$from@Data, rWindows.nb$to@Data, LCL_filtered) %>% na.omit()
parity.returns.nb <- Return.portfolio(LCL_filtered, weights=parity.weights.nb,verbose=TRUE)
p.returns.nb <- merge.xts(returns.mv$returns, parity.returns.nb$returns, parity.returns$returns, returns.md$returns, join = "inner")
names(p.returns.nb)<-c("MVP", "RPP w/o Bonds", "RPP", "MDP")
```

For a first impression on the performance, the cumulative returns and the annualized returns are calculated.

```{r}
PerformanceAnalytics::chart.CumReturns(p.returns.nb, colorset=rich6equal,
                          lwd=1, cex.legend = 0.5, event.labels = TRUE, main = "", legend.loc = "topleft")
```

```{r}
chart.RollingPerformance(p.returns.nb, width = 252, colorset=rich6equal,
                          lwd=1, cex.legend = 0.5, event.labels = TRUE, main = "", legend.loc = "top")
```

To quantify the results they are then calculated and the numeric values of both are displayed in a table.

```{r}
bind_rows(
  as.data.frame(Return.cumulative(p.returns.nb)),
  as.data.frame(Return.annualized(p.returns.nb))
) %>% kbl(caption = "Cumulative and Annualized Returns", label = "returns") %>% 
    kable_styling()
```

Since the focus of the portfolios is to minimize risk, the drawdowns are very interesting to visually compare how the portfolios behave when the financial markets are falling.

```{r}
chart.Drawdown(p.returns.nb, colorset=rich6equal,
                          lwd=1, cex.legend = 0.5, event.labels = TRUE, main = "", legend.loc = "bottom")
```

To get more insights, in addition to the chart a table with different drawdown measures such as the Sterling, Calmar, Burke, Pain, and Martin Ratios, along with the Pain Index and the Ulcer Index, will be calculated.

```{r}
table.DrawdownsRatio(p.returns.nb, Rf = 0.08975) %>%
  kbl(caption = "Drawdowns ratio table", label = "drawdowns-ratio") %>%
  kable_styling()
```

To compare the downside risk, different Sharpe Ratios will be calculated.

```{r}
SharpeRatio(p.returns.nb, Rf = 0.095) %>% 
    kbl(caption = "Sharpe Ratios", label = "sharpe-ratios")  %>% 
    kable_styling()
```

The calculate_ratios function filters bond yield data to include only observations after 2007 and determines the risk-free rate as the average 10-year bond yield. It computes the Sortino Ratio for risk-adjusted returns, Conditional Drawdown at Risk (CDD) at a 95% confidence level, the Upside Potential Ratio for assessing return potential relative to downside risk, Conditional Value at Risk (CVaR) for average losses beyond the Value at Risk, and Value at Risk (VaR) for maximum expected losses. The results are returned as a data frame of calculated ratios.

```{r}
calculate_ratios(p.returns.nb, BondYields_10Y_SA) %>% 
    kbl(caption = "Downside and Opportunity-based Performance Metrics", label = "downside-metrics") %>%
    kable_styling()
```

To evaluate the performance of RPPs across various economic environments, their performance will be analyzed during periods of recession, moderate growth, and high growth. The economic_regime function classifies years based on real GDP growth rates, while the regime_return function integrates the output of the economic_regime function and computes three performance metrics: return, volatility, and Sharpe ratio.

```{r}
return_parity <- return_data(parity.returns)
return_mv <- return_data(returns.mv)
return_parity_nb <- return_data(parity.returns.nb)
return_md <- return_data(returns.md)

regime <- economic_regime(SA_Growth, 3, 0)

results_mv <- regime_returns(return_mv, regime)
results_RPP <- regime_returns(return_parity, regime)
results_RPP_nb <- regime_returns(return_parity_nb, regime)
results_md <- regime_returns(return_md, regime)

combine_regime_results(results_mv, results_RPP, results_RPP_nb, results_md) %>% 
    kbl(caption = "Performance at Different Economic States", label = "regime") %>% 
    kable_styling()
```



