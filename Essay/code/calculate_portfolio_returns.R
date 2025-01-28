calculate_portfolio_returns <- function(returns, weights_xts, rebalance_dates) {
    portfolio_returns <- rep(NA, nrow(returns))

    for (i in seq_along(rebalance_dates)) {
        start_index <- which(index(returns) == rebalance_dates[i])
        if (i < length(rebalance_dates)) {
            end_index <- which(index(returns) == rebalance_dates[i + 1]) - 1
        } else {
            end_index <- nrow(returns)
        }

        current_weights <- as.numeric(weights_xts[i, ])
        portfolio_returns[start_index:end_index] <- rowSums(returns[start_index:end_index, ] * current_weights)
    }

    portfolio_returns_xts <- xts(portfolio_returns, order.by = index(returns))
    portfolio_returns_xts <- na.omit(portfolio_returns_xts)

    cumulative_returns <- cumprod(1 + portfolio_returns_xts) - 1

    list(portfolio_returns = portfolio_returns_xts, cumulative_returns = cumulative_returns)
}

