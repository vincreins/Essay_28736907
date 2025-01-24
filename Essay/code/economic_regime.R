economic_regime <- function(data, High, Low) {
    data <- data %>%
        mutate(regime = case_when(
            Growth > High ~ "High Growth",
            Growth > Low & Growth <= High ~ "Moderate Growth",
            Growth <= Low ~ "Recession"
        ))
    colnames(data) <- c("year", "growth", "regime")
    return(data)
}
