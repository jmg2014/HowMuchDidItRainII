medianColumn<-function(column){
idx <- is.na(column) ## This uses is.na.ff_vector from ffbase
idx <- ffwhich(idx, idx == TRUE) ## Is part of ffbase
column <- ffindexset(x=column, index=idx, value=ff(mean(column,na.rm=TRUE), length=length(idx), vmode = "double")) ## Is part of ff

}