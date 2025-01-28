combine_regime_results <- function(results_mv, results_RPP, results_RPP_nb, results_md) {

    results_mv$Dataset <- "MVP"
    results_RPP$Dataset <- "RPP"
    results_RPP_nb$Dataset <- "RPP_nb"
    results_md$Dataset <- "MDP"

    combined_results <- rbind(results_mv, results_RPP, results_RPP_nb, results_md)

    combined_results <- combined_results %>%
        select(Dataset, regime, mean_return, volatility, sharpe_ratio)

    return(combined_results)
}

