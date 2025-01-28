ApplyFilter <- function(from, to, R, FUN){
    return(FUN(R[paste0(from, "/", to)]))
}
