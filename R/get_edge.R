get_edge <-
function(){
    lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    subedge <- lastPP$edge
    XX <- lastPP$xx[subedge[, 1]]
    YY <- lastPP$yy[subedge[, 2]]
    cbind(XX,YY)
}
