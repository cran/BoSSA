\name{circular_tree}
\alias{circular_tree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot an inside-out circular tree
}
\description{
Plot a tree in a circular manner with the tips pointing inward
}
\usage{
circular_tree(phy,ratio=0.5,def=1000,pos_out=FALSE,tip_labels=TRUE,cex_tips=0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{phy}{
a class phylo object
}
  \item{ratio}{
the ratio of the tree size compared to the plot size
}
  \item{def}{
the def parameter controls the granularity of the curves
}
  \item{pos_out}{
a matrix with the x and y coordinates of the branches extremities (i.e. nodes and tips) is outputed when set to TRUE
}
  \item{tip_labels}{
whether or not the tiplabels should be plotted
}
  \item{cex_tips}{
the size of the tiplabels
}

}
\details{
The function plot a tree in a circular manner. Note that the tree will produce a correct output only if there is no topology modifications after reading the original tree using the ape read.tree function.
}
\value{
a plot
}
\author{
pierre lefeuvre
}
\examples{

library(ape)

test_tree <- rtree(20)

circular_tree(test_tree)

}
