pplace_to_taxonomy <-
function(pplace,taxonomy,rank=c("phylum","class","order","family","genus","species"),tax_name=TRUE,run_id=NULL){
  if(class(pplace)!="pplace"){
    return("ERROR: the input is not an object of class pplace")
  }
  if(class(pplace)=="pplace"){
    if(!is.null(run_id)){
      pplace <- sub_pplace(pplace,run_id=run_id)
    }
    out <- pplace$multiclass[,c("name","rank","tax_id")]
    if(sum(is.na(match(out$tax_id,taxonomy$tax_id)))/length(out$tax_id)>0.5) warning("the taxonomy doesn't seems to match the pplace object")
    out <- cbind(out,taxonomy[match(out$tax_id,taxonomy$tax_id),colnames(taxonomy)%in%rank])
    out2 <- as.matrix(out[,-(1:3)])
    rownames(out2) <- out[,1]
    if(tax_name){
      out3 <- out2
      tax_id <- unique(as.vector(out3))
      tax_id <- tax_id[tax_id!=""]
      for(i in 1:length(tax_id)){
	if(!is.na(tax_id[i])){
	  out2[out3==tax_id[i]] <- taxonomy$tax_name[taxonomy$tax_id==tax_id[i]]
	}
      }
    }
    out2[!is.na(out2) & out2==""] <- "Unclassified"  
    return(out2)
  }
}
