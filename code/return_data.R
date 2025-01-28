return_data <- function(returns) {
    return <- returns$returns
    data_df <- data.frame(date = index(return), coredata(return))
    data_df$year <- year(data_df$date)
    data_df$year <- as.double(data_df$year)
    return(data_df)
}
