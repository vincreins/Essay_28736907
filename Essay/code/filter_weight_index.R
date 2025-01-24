
filter_weight_index <- function(data, weight){
    data %>%
        filter(!wday(date) %in% c(1, 7)) %>%
        select(date, Tickers, weight) %>%
        spread(Tickers, weight) %>%
        select(where(~ all(!is.na(.)))) %>%
        select(where(~ any(replace_na(., 0) != 0))) %>%
        as.xts()
}
