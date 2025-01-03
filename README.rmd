---
output:
  md_document:
    variant: markdown_github
---

### Risk Parity Portfolios (RPPs) Analysis: A South African Perspective  

#### Introduction  
Risk parity portfolios (RPPs) aim to construct portfolios where each asset contributes equally to the overall portfolio risk. Unlike traditional portfolio creation methods based on modern portfolio theory (MPT), such as the mean-variance approach, RPPs avoid reliance on return forecasts. Instead, they focus on risk estimates, particularly volatility and correlation.  

While MPT methods prioritize maximizing returns or minimizing variance, RPPs emphasize explicit risk allocation. This research seeks to evaluate the performance of RPPs compared to portfolios created using the MPT approach and Index funds, guided by the following question:  

**How can the risk parity approach be effectively implemented to construct a diversified portfolio of South African assets, and how does it compare to traditional asset allocation strategies in terms of risk-adjusted returns?**  

#### Focus of the Study  
The research centers on risk-adjusted returns, aiming to assess how RPPs perform under various market conditions. To achieve this, the study will evaluate an expanded set of performance metrics:  
- Sharpe ratio  
- Sterling ratio  
- Alpha and Beta  
- Sortino ratio  
- Maximum drawdown  
- Calmar ratio  
- Information ratio (vs. a relevant benchmark)  

#### Asset Selection for the South African RPP  
A diverse set of traded assets will be considered, including:  
- Stocks from the Johannesburg Stock Exchange  
- South African government bond yields (2-year and 10-year maturities)  

#### Consideration of Economic Regimes  
To enhance robustness, the study will examine portfolio performance across South Africa's specific economic regimes, such as:  
- High and low economic growth periods  
- High and low inflation environments  

This approach aims to capture how the risk parity strategy adapts to varying market conditions compared to traditional asset allocation methods.  

#### Objectives and Expected Insights  
By incorporating additional performance metrics and economic regime analyses, the research will provide a comprehensive evaluation of the risk parity approach in the South African context. It aims to deliver deeper insights into the effectiveness of RPPs in constructing diversified portfolios and their comparative performance against traditional strategies across diverse market scenarios.  

```{r}

rm(list = ls()) # Clean your environment:
gc() # garbage collection - It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.
library(tidyverse)
list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```


