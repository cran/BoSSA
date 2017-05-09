pplace_to_taxonomy <-
function(pplace,taxonomy,rank=c("phylum","class","order","family","genus","species"),tax_name=TRUE){
  out <- pplace$multiclass[,c(2,4,5)]
  out <- cbind(out,taxonomy[match(out$tax_id,taxonomy$tax_id),colnames(taxonomy)%in%rank])
  out2 <- as.matrix(out[,-(1:3)])
  rownames(out2) <- out[,1]
  if(tax_name){
    out3 <- out2
    tax_id <- unique(unlist(out3))
    tax_id <- tax_id[tax_id!=""]
    for(i in 1:length(tax_id)){
      out2[out3==tax_id[i]] <- taxonomy$tax_name[taxonomy$tax_id==tax_id[i]]
    }
  }
  out2[apply(out2=="",1,sum)==ncol(out2),] <- "Unclassified"
  pos <- (1:nrow(out2))[apply(out2=="",1,sum)>0]
  if(length(pos)>0){
    for(i in pos){
      out2[i,][out2[i,]==""] <- paste("Unclassified",rev(out2[i,][out2[i,]!=""])[1])
    }
  }
  out2
}