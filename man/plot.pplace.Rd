\name{plot.pplace}
\alias{plot.pplace}
\alias{plot.jplace}
\title{
Plot a pplace or jplace object
}
\description{
Plot the tree and placements from a pplace or a jplace object
}
\usage{
\method{plot}{pplace}(x,type="precise",simplify=FALSE,
		main="",N=NULL,transfo=NULL,legend=TRUE,stl=FALSE,
		asb=FALSE,edge.width=1,max_width=10,cex.number=0.5,
		cex.text=0.8,transp=80,add=FALSE,color=NULL,discrete_col=FALSE,
		pch=16,run_id=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A pplace or jplace object
}
  \item{type}{
The type of ploting desired with either, "precise", "color", "fattree" or "number". For each option, placement sizes represent the multiplication of the N value with the placement ML ratio.
}
  \item{simplify}{
If set to TRUE, only plot the best position for each placement. default is FALSE.
}
  \item{main}{
An optionnal title to plot along the tree
}
  \item{N}{
An optionnal vector of the weight of each placement. Must be of the same length and order as placements in the multiclass table. Note that the placement mass (potentially) available from the original files are imported into R but aren't use in the analysis. The N parameter should be used instead.
}
  \item{transfo}{
An optionnal function to transform the placement size when type set to "precise". Beware that it is also applied to the legend so that it does not anymore correspond to the placement size but to the transform dot size
}
  \item{legend}{
Plot a legend. Not available for type "number" or "fattree"
}
  \item{stl}{
Show tip labels
}
  \item{asb}{
Add scale bar
}
  \item{edge.width}{
The tree edge width
}
  \item{max_width}{
The maximum edge width when type is set to "fattree"
}
  \item{cex.number}{
Control the size of the text when type is set to "number"
}
  \item{cex.text}{
Control the size of the main
}
  \item{transp}{
Control the transparency of the placement when type is "precise" and the transparency of the branch without placement when type is set to "color". Encoded in hexadecimal scale (i.e. range from "00" to "FF")
}
  \item{add}{
Add placement to an existing plot when type is set to precise. Default is FALSE. If it was drawn, the legend won't be updated. Beware to use the same value for the "transfo" option in each plot. Dots color scale won't be accurate when using the "add" option. It is highly recommanded to use a single color.
}
  \item{color}{
The colors used for pendant branch length scale when type is set to "precise". Default is a color ramp with "blue", "green", "yellow" and "red"
}
  \item{discrete_col}{
Discretise the color scale for pendant branch length
}
  \item{pch}{
The dot style used for placements when type is set to "precise"
}
  \item{run_id}{
A vector of run_id to subset
}
  \item{...}{
Further arguments passed to or from other methods.
}
}
\author{
pierre lefeuvre
}
\examples{

data(pplace)

### number type
plot(pplace,type="number",main="number")

### color type without and with legend
plot(pplace,type="color",main="color without legend",legend=FALSE)
plot(pplace,type="color",main="color with legend",legend=TRUE)

### fattree type
plot(pplace,type="fattree",main="fattree")

### precise type
plot(pplace,type="precise",main="precise vanilla")
plot(pplace,type="precise",simplify=TRUE,main="precise simplify")

# using the read number information encoded here in the name (if available)
Npplace <- sample(1:100,nrow(pplace$multiclass),replace=TRUE)
# in the following exemple, the dots are too large...
plot(pplace,type="precise",main="precise N",legend=TRUE,N=Npplace,simplify=TRUE)

# using the transfo option to modify dot sizes
# note that placements sizes inferior to 1 won't
# behave properly with log10 as a transformation function.
# In this case, you rather use simplify (all the placement
# will corresponds to at least one sequence).
# Beware that when using the transfo option, 
# the legend does not anymore correspond to the actual placement
# size but to the transform placement size
# (i.e. the transform function applied to the dot size).
# we will use the the log10 function
plot(pplace,type="precise",main="precise log10",
	legend=TRUE,N=Npplace,transfo=log10)
# or without simplify, you can use a custom function
# as transfo that will produce positive sized dots
plot(pplace,type="precise",main="precise custom"
	,legend=TRUE,N=Npplace,transfo=function(X){log10(X+1)})
}
