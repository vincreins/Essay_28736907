library(knitr)

calculate_ratios_table <- function(p_returns, bond_yields, mar = 0.053) {
    risk_f <- mean(bond_yields) / 100
    ratios <- data.frame(
        Metric = c("Sharpe Ratio", "Sortino Ratio", "CDD", "Upside Potential Ratio"),
        Value = c(
            SharpeRatio(p_returns, Rf = risk_f),
            SortinoRatio(p_returns, MAR = mar),
            CDD(p_returns, Rf = risk_f),
            UpsidePotentialRatio(p_returns, MAR = mar)
        )
    )

    kable(ratios, format = "markdown", col.names = c("Metric", "Value"))
}
