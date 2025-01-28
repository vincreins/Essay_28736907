Bond_return <- function(data, Yield, Maturity) {
    data %>%
        #filter(!lubridate::wday(date) %in% c(1, 7)) %>%
        select(date, Ticker, !!sym(Yield)) %>%
        rename(Yield = !!sym(Yield)) %>%
        mutate(
            BondPrice = 1 / ((1 + Yield / 100) ^ Maturity)
        ) %>%
        select(date, BondPrice) %>%
        tidyr::drop_na() %>%
        as.xts() %>%
        CalculateReturns(method = "discrete") %>%
        na.omit()
}
