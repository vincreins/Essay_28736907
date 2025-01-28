CalculateRiskParity <- function(r){
    library(riskParityPortfolio)
    return(riskParityPortfolio(cov(r))$w)
}
