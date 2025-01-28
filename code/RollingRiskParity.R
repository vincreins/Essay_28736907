RollingRiskParity <- function(from, to, r){
    library(rlist)
    p<-ApplyRolling(from, to, r, CalculateRiskParity)
    names(p)<-to
    return(list.rbind(p))
}
