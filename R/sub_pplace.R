sub_pplace <-
function(x,placement_id=NULL,ech_id=NULL,ech_regexp=NULL,run_id=NULL){
  if(!is.null(placement_id)){
    x$multiclass <- x$multiclass[x$multiclass$placement_id%in%placement_id,]
    x$placement_positions <- x$placement_positions[x$placement_positions$placement_id%in%placement_id,]
  }
  if(!is.null(ech_id)){
    x$multiclass <- x$multiclass[x$multiclass$name%in%ech_id,]
    x$placement_positions <- x$placement_positions[x$placement_positions$placement_id%in%x$multiclass$placement_id,]
  }
  if(!is.null(ech_regexp)){
    x$multiclass <- x$multiclass[grep(ech_regexp,x$multiclass$name),]
    x$placement_positions <- x$placement_positions[x$placement_positions$placement_id%in%x$multiclass$placement_id,]
  }
  if(!is.null(run_id)){
    pid <- x$placements$placement_id[x$placements$run_id%in%run_id]
    x$multiclass <- x$multiclass[x$multiclass[,1]%in%pid,]
    x$placement_positions <- x$placement_positions[x$placement_positions$placement_id%in%x$multiclass$placement_id,]
  }
  if(class(x)=="pplace"){
    if(!is.null(run_id)){
      x$run <- x$run[x$run$run_id%in%run_id,]
    }
    pid <- unique(x$placement_positions$placement_id)
    x$placement_classifications <- x$placement_classifications[x$placement_classifications$placement_id%in%pid,]
    x$placement_evidence <- x$placement_evidence[x$placement_evidence$placement_id%in%pid,]
    x$placement_names <- x$placement_names[x$placement_names$placement_id%in%pid,]
    x$placements <- x$placements[x$placements$placement_id%in%pid,]
    if(nrow(x$placement_median_identities)>0)  x$placement_median_identities <- x$placement_median_identities[x$placement_median_identities$placement_id%in%pid,]    
    if(nrow(x$placement_nbc)>0) x$placement_nbc <- x$placement_nbc[x$placement_nbc$placement_id%in%pid,]
    x$sqlite_sequence[x$sqlite_sequence[,1]=="runs",2] <- length(unique(x$run$run_id))
    x$sqlite_sequence[x$sqlite_sequence[,1]=="placements",2] <- length(pid)
  }
  return(x)
}
