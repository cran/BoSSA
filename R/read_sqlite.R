read_sqlite <-
function(SQLITE,jplace_file=NULL,rank="species"){
  out <- list()
  db <- dbConnect(SQLite(),dbname=SQLITE)
  out$run <- dbGetQuery(db,"select * from runs")
  if(is.null(jplace_file)) JPLACE <- gsub("sqlite","jplace",SQLITE)
  if(!is.null(jplace_file)) JPLACE <- jplace_file
  out$taxo <- dbGetQuery(db,"select * from taxa")
  out$multiclass <- dbGetQuery(db,paste("select * from multiclass where want_rank=\'",rank,"\'",sep=""))
  out$placement <- dbGetQuery(db,"select * from placement_positions")
  if(nrow(out$multiclass)>0){
    out <- c(out,read_jplace(JPLACE))
    pplacer_branch_id <- out$placement$location
    out$placement$location <- out$edge_key[1,match(pplacer_branch_id,out$edge_key[2,])]
    out$edge_key <- NULL
  }
  if(nrow(out$multiclass)==0){
    out$arbre <- NULL
  }
  dbDisconnect(db)
  out
}
