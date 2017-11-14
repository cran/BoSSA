\name{refpkg}
\alias{refpkg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary data and plots for reference packages
}
\description{
Summary data and plots for reference packages
}
\usage{
refpkg(refpkg_path,type="summary",rank_tree="species",
rank_pie=c("phylum","class","order","family","genus"),
scale_pie=TRUE,alpha_order=TRUE,cex.text=0.7,
cex.legend=1,asb=TRUE,rotate_label=TRUE,
out_krona="for_krona.txt",text2krona=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{refpkg_path}{
The path of the reference package directory
}
  \item{type}{
The type of summary to perform with "summary", "taxonomy", "tree", "pie" or "krona" available.
}
  \item{rank_tree}{
The desired rank for tree coloring
}
  \item{rank_pie}{
The ranks to be plot for the taxonomy pie chart
}
  \item{scale_pie}{
Wether or not to take into account the number of sequences available within the reference package for the pie chart
}
  \item{alpha_order}{
Wether or not to color should follows taxa alpahabetic order when type set to "tree"
}
  \item{cex.text}{
The tip labels cex parameter when type is set to "tree" and the text cex parameter when type is set to "pie"
}
  \item{cex.legend}{
The size of the legend when type set to "tree"
}
  \item{asb}{
Add a scale bar on the tree
}
  \item{rotate_label}{
Rotates the pie slice labels
}
  \item{out_krona}{
Teh name of the output when type is set to "krona".
}
  \item{text2krona}{
The full path to  the krona "ImportText.pl" script when KronaTools is installed and you want to produce the html krona file.
}

}
\value{
A summary print on screen when tupe set to "summary". A data frame when type set to "taxonomy". A plot otherwise.
}
\references{
https://github.com/marbl/Krona/wiki/KronaTools
http://fhcrc.github.io/taxtastic/
}
\author{
pierre lefeuvre
}
\examples{

refpkg_path <- paste(find.package("BoSSA"),"/extdata/example.refpkg",sep="")

### summary
refpkg(refpkg_path)

### taxonomy
taxonomy <- refpkg(refpkg_path,type="taxonomy")
head(taxonomy)

### tree
refpkg(refpkg_path,type="tree",rank_tree="order",cex.text=0.5)

### pie
refpkg(refpkg_path,type="pie",rank_pie=c("class","order","family"),cex.text=0.6)

### krona
# it will produce a flat text file
# this file can be use as input for the the "ImportText.pl" krona script
# see https://github.com/marbl/Krona/wiki/KronaTools for more details on krona
refpkg(refpkg_path,type="krona",out_krona="for_krona.txt")

}
