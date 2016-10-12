\name{pplace_tree}
\alias{pplace_tree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot a pplace object
}
\description{
function to plot a tree from a pplace object
}
\usage{
pplace_tree(pplace, type = "precise", main = "",
		N = NULL, transfo = NULL, legend = TRUE,
		stl = FALSE, asb = FALSE, edge.width = 1,
		cex.number = 0.5, cex.text = 0.8, transp = 80)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pplace}{
a pplace object
}
  \item{type}{
the type of ploting desired with either, "precise", "color" or "number". For every option, the number of placement per branch is the multiplication of the N value with the placement ML ratio for this branch.
}
  \item{main}{
an optionnal title to plot along the tree
}
  \item{N}{
an optionnal vector with a number of occurence associated to each placed sequence
}
  \item{transfo}{
an optionnal function to transform the placement number
}
  \item{legend}{
plot a legend
}
  \item{stl}{
show tip labels
}
  \item{asb}{
add scale bar
}
  \item{edge.width}{
the tree edge width
}
  \item{cex.number}{
control the size of the text
}
  \item{cex.text}{
control the size of the text
}
  \item{transp}{
control the transparency of the placement when type is "precise" and the transparency of the branch without assignment when type is "color" or "number"
}
}
\value{
a plot
}
\author{
pierre lefeuvre
}
\examples{

data(pplace)

### number type
pplace_tree(pplace,type="number",main="number")

### color type
pplace_tree(pplace,type="color",main="color")
pplace_tree(pplace,type="color",main="color",legend=TRUE)

### precise type
pplace_tree(pplace,type="precise",main="precise")
pplace_tree(pplace,type="precise",main="precise",legend=TRUE)
pplace_tree(pplace,type="precise",main="precise",legend=TRUE,N=sample(1:20,17))

# using the read number information encoded in the name (if available)
Npplace <- as.numeric(gsub(".*_","",pplace$multiclass$name))
pplace_tree(pplace,type="precise",main="precise",legend=TRUE,N=Npplace,transfo=log10)

### using the transfo option
pplace_tree(pplace,type="precise",main="precise",legend=TRUE,N=sample(1:20,17),transfo=log10)
FUN=function(X){log(X+1)}
pplace_tree(pplace,type="precise",main="precise",legend=TRUE,N=sample(1:20,17),transfo=FUN)
}
