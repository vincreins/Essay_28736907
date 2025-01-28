filter_stock_function <- function(data) {
    data %>%
        filter(!wday(date) %in% c(1, 7)) %>%
        select(date, Tickers, Return) %>%
        spread(Tickers, Return) %>%
        select(where(~ all(!is.na(.)))) %>%
        select(where(~ any(replace_na(., 0) != 0))) %>%
        as.xts()
}
