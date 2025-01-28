most_diversified_weights <- function(returns, window_size = 252, rebalance_interval = 63) {
    returns <- na.omit(returns)

    if (!is.matrix(returns)) {
        returns <- as.matrix(returns)
    }

    if (!all(apply(returns, 2, is.numeric))) {
        stop("All columns in returns must be numeric.")
    }

    cov_matrix_func <- function(data) cov(data)
    sd_func <- function(data) apply(data, 2, sd)

    diversification_ratio <- function(weights, sd_vector, cov_matrix) {
        numerator <- sum(weights * sd_vector)
        denominator <- sqrt(t(weights) %*% cov_matrix %*% weights)
        -(numerator / denominator)
    }

    create_mdp_weights <- function(data) {
        cov_matrix <- cov_matrix_func(data)
        sd_vector <- sd_func(data)
        n_assets <- ncol(data)
        lower_bounds <- rep(0, n_assets)
        upper_bounds <- rep(1, n_assets)

        result <- DEoptim(
            fn = function(weights) diversification_ratio(weights, sd_vector, cov_matrix),
            lower = lower_bounds,
            upper = upper_bounds,
            DEoptim.control(itermax = 500, trace = FALSE)
        )

        weights <- result$optim$bestmem / sum(result$optim$bestmem)
        return(weights)
    }

    rebalance_dates <- index(returns)[seq(window_size, nrow(returns), by = rebalance_interval)]
    rebalancing_weights <- list()

    for (date in rebalance_dates) {
        end_index <- which(index(returns) == date)
        rolling_data <- returns[(end_index - window_size + 1):end_index, ]
        weights <- create_mdp_weights(rolling_data)
        rebalancing_weights[[length(rebalancing_weights) + 1]] <- weights
    }

    weights_matrix <- do.call(rbind, rebalancing_weights)
    colnames(weights_matrix) <- colnames(returns)
    weights_xts <- xts(weights_matrix, order.by = rebalance_dates)

    return(weights_xts)
}
