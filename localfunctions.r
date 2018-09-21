func.percentFormatX <- function(x) c( paste0(x[1]*100,'%'), x[-1]*100 )
func.percentFormatY <- function(y) c(y[1:length(y)-1]*100 , paste0(rev(y)[1]*100,'%'))