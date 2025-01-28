calculate_ratios <- function(p_returns, bond_yields) {
    bond_yields$date <- as.Date(bond_yields$date)
    filtered_bond_yields <- subset(bond_yields, date > as.Date("2007-01-01"))
    risk_f <- mean(filtered_bond_yields$BondYield_10) / 100
    sortino_ratio <- SortinoRatio(p_returns, MAR = risk_f)
    cdd <- CDD(p_returns, p = 0.95)
    upside_potential_ratio <- UpsidePotentialRatio(p_returns, MAR = risk_f)
    cvar <- CVaR(p.returns.nb, method = "historical")
    var <- VaR(p.returns.nb, method = "historical")

    ratios_matrix <- rbind(
        "SortinoRatio" = sortino_ratio,
        "CDD" = cdd,
        "UpsidePotentialRatio" = upside_potential_ratio,
        "CVaR" = cvar,
        "VaR" = var
    )

    ratios_df <- as.data.frame(ratios_matrix)

    return(ratios_df)
}
