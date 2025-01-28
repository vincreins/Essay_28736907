min_vol_portfolio_weights <- function(returns, window_size = 252, rebalance_interval = 63, portfolio) {
    returns <- returns
    portfolio <- portfolio.spec(assets = colnames(returns))
    portfolio <- add.constraint(portfolio, type = "full_investment")
    portfolio <- add.constraint(portfolio, type = "long_only")
    portfolio <- add.constraint(portfolio, type = "box", min = 1e-10, max = 0.99)
    portfolio <- add.objective(portfolio, type = "risk", name = "var")
    rebalance_dates <- index(returns)[seq(window_size, nrow(returns), by = rebalance_interval)]
    rebalancing_weights <- list()

    for (date in rebalance_dates) {
        end_index <- which(index(returns) == date)
        rolling_data <- returns[(end_index - window_size + 1):end_index, ]
        opt <- optimize.portfolio(R = rolling_data, portfolio = portfolio, optimize_method = "ROI")
        rebalancing_weights[[length(rebalancing_weights) + 1]] <- extractWeights(opt)
    }

    weights_matrix <- do.call(rbind, rebalancing_weights)
    weights_matrix <- weights_matrix / rowSums(weights_matrix)
    weights_xts <- xts(weights_matrix, order.by = rebalance_dates)

    stopifnot(all(weights_xts >= 0))
    stopifnot(all(abs(rowSums(weights_xts) - 1) < 1e-8))

    return(weights_xts)
}
