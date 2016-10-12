sub_pplace <-
function(pplace,placement_id=NULL,ech_id=NULL,ech_regexp=NULL){
  if(!is.null(placement_id)){
    pplace$multiclass <- pplace$multiclass[pplace$multiclass$placement_id%in%placement_id,]
    pplace$placement <- pplace$placement[pplace$placement$placement_id%in%placement_id,]
  }
  if(!is.null(ech_id)){
    pplace$multiclass <- pplace$multiclass[pplace$multiclass$name%in%ech_id,]
    pplace$placement <- pplace$placement[pplace$placement$placement_id%in%pplace$multiclass$placement_id,]
  }
  if(!is.null(ech_regexp)){
    pplace$multiclass <- pplace$multiclass[grep(ech_regexp,pplace$multiclass$name),]
    pplace$placement <- pplace$placement[pplace$placement$placement_id%in%pplace$multiclass$placement_id,]
  }
  pplace
}
