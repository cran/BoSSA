read_sqlite <-
function(sqlite_file,jplace_file=gsub("sqlite$","jplace",sqlite_file),rank="species"){
  out <- list()
  db <- dbConnect(SQLite(),dbname=sqlite_file)
  if(is.null(rank)) out$multiclass <- dbGetQuery(db,"select * from multiclass") 
  if(!is.null(rank)) out$multiclass <- dbGetQuery(db,paste0("select * from multiclass where want_rank=\'",rank,"\'"))
  out$placement_classifications <- dbGetQuery(db,"select * from placement_classifications")
  out$placement_evidence <- dbGetQuery(db,"select * from placement_evidence")
  out$placement_median_identities <- dbGetQuery(db,"select * from placement_median_identities")
  out$placement_names <- dbGetQuery(db,"select * from placement_names")
  out$placement_nbc <- dbGetQuery(db,"select * from placement_nbc")
  out$placement_positions <- dbGetQuery(db,"select * from placement_positions")
  out$placements <- dbGetQuery(db,"select * from placements")
  out$ranks <- dbGetQuery(db,"select * from ranks")
  out$runs <- dbGetQuery(db,"select * from runs")
  out$sqlite_sequence <- dbGetQuery(db,"select * from sqlite_sequence")  
  out$taxa <- dbGetQuery(db,"select * from taxa")
  dbDisconnect(db)
  
  out <- c(out,read_jplace(jplace_file,full=FALSE))
  if(nrow(out$multiclass)>0){
    pplacer_branch_id <- out$placement_positions$location
    out$placement_positions$location <- out$edge_key[1,match(pplacer_branch_id,out$edge_key[2,])]
  }
  class(out) <- "pplace"
  out  
}
