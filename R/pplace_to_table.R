pplace_to_table <-
function(pplace,type="full",run_id=NULL){
  if(class(pplace)!="pplace"){
    return("ERROR: the input is not an object of class pplace")
  }
  if(class(pplace)=="pplace"){
    if(!is.null(run_id)){
      pplace <- sub_pplace(pplace,run_id=run_id)
    }
    out <- NULL
    if(nrow(pplace$multiclass)>0){
      out <- merge(pplace$multiclass,pplace$placement_positions,by="placement_id")
      if(type=="best"){
	out <- out[order(out$ml_ratio,decreasing=TRUE),]
	out <- out[match(unique(out$placement_id),out[,1]),]
      }
      out <- out[order(out$placement_id),]
      rownames(out) <- NULL
      colnames(out)[c(5,12)] <- c("tax_id_multilcass","tax_id_placement")
    }
    return(out)
  }
}
