\name{grpPhylogeo}
\alias{grpPhylogeo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Determine sets of sequence presenting phylogeography signal }
\description{
  From an alignment of sequence (fasta format) and a csv file with colomns nom (name of the sequence), lon and lat (GPS coordinates in degree.decimals of the longitude and latitude of sampling of the sequence), the function define sets of sequence presenting phylogeography signal using a mantel test between physic and genetic distance matrices.
}
\usage{
grpPhylogeo(gbinfo, align, seuil = 0.1, method = "single",
		model = "raw", pairwise.deletion=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gbinfo}{ name/path of a csv file with information on name (nom colomn) and GPS coordiantes of the sequence (lon and lat colomns) }
  \item{align}{ sequence alignment in fasta format with the same name as those specified in the gbinfo file }
  \item{seuil}{ significativity threshold for the mantel test }
  \item{method}{ clustering method used; set to "single" by default; See the hclust function for details }
  \item{model}{ DNA evolutionnary model use to compute genetic distance. Set to "raw" by default; See dist.dna (ape package) for details }
  \item{pairwise.deletion}{ See dist.dna (ape package) for details }
}
\details{
  The function draw a clustering plot (with physic distance represented) with a red frame arround sequences with significative mantel test for correlation between physic and genetic distance.
}
\value{
  A list of group of sequences with significative correlation between physic and genetic distance is returned.
}
\author{ Pierre Lefeuvre }
\seealso{ \code{\link{hclust}}, \code{\link[ape]{dist.dna}}, \code{\link[ape]{mantel.test}} }



