ApplyRolling <- function(from, to, R, FUN){
    library(purrr)
    return(map2(from, to, ApplyFilter, R=R, FUN=FUN))
}
