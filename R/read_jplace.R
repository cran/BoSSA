read_jplace <-
function(JPLACE){
  json_data <- fromJSON(JPLACE)
  arbre <- json_data[[1]]
  arbre2 <- gsub("}","",gsub("{","#",arbre,fixed=TRUE),fixed=TRUE)
  arbre3 <- read.tree(text=gsub("#","",gsub(":[0-9].[0-9]+#",":",gsub("e-","",arbre2))))
  edge_key <- rbind(1:nrow(arbre3$edge),arbre3$edge.l)
  arbre <- gsub("}","#",gsub("{","#",arbre,fixed=TRUE),fixed=TRUE)
  arbre <- read.tree(text=gsub("#[0-9]+#","",arbre))
  out <- list(arbre,edge_key)
  names(out) <- c("arbre","edge_key")
  out
}

