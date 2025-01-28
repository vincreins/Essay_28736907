regime_returns <- function(data, regime) {
    d1 <- data %>%
        inner_join(regime, by = "year") %>%
        mutate(portfolio.returns = as.numeric(portfolio.returns))

    results <- d1 %>%
        as.data.frame() %>%
        mutate(date = index(d1)) %>%
        group_by(regime) %>%
        summarize(
            mean_return = mean(portfolio.returns, na.rm = TRUE),
            volatility = sd(portfolio.returns, na.rm = TRUE),
            sharpe_ratio = mean(portfolio.returns, na.rm = TRUE) / sd(portfolio.returns, na.rm = TRUE)
        )


}
