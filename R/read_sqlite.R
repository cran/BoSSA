read_sqlite <-
function(sqlite_file,jplace_file=gsub("sqlite","jplace",sqlite_file),rank="species"){
  out <- list()
  db <- dbConnect(SQLite(),dbname=sqlite_file)
  out$run <- dbGetQuery(db,"select * from runs")
  out$taxo <- dbGetQuery(db,"select * from taxa")
  out$multiclass <- dbGetQuery(db,paste("select * from multiclass where want_rank=\'",rank,"\'",sep=""))
  out$placement <- dbGetQuery(db,"select * from placement_positions")
  if(nrow(out$multiclass)>0){
    out <- c(out,read_jplace(jplace_file,full=FALSE))
    pplacer_branch_id <- out$placement$location
    out$placement$location <- out$edge_key[1,match(pplacer_branch_id,out$edge_key[2,])]
    out$edge_key <- NULL
  }
  if(nrow(out$multiclass)==0){
    out$arbre <- NULL
  }
  dbDisconnect(db)
  class(out) <- "pplace"
  out  
}
