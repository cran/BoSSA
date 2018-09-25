\name{read_sqlite}
\alias{read_sqlite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read a pplacer/guppy sqlite file
}
\description{
Read a pplacer/guppy sqlite file
}
\usage{
read_sqlite(sqlite_file,jplace_file=gsub("sqlite$","jplace",sqlite_file),
rank="species")
}
\arguments{
  \item{sqlite_file}{
A pplacer/guppy sqlite path/file name
}
  \item{jplace_file}{
An optionnal jplace file name. By default, the sqlite file name with the suffix changed from "sqlite" to jplace" is used. If different, the jplace path/name must be specified.
}
  \item{rank}{
The desired taxonomic assignation rank to extract. default is "species".
}
}
\details{
As the tree informations are not available in the sqlite file, the jplace file is also required.
When the jplace or sqlite files are import into R, the node numbering available in the original
file is converted to the class "phylo" numbering.
}
\value{
A list with
\item{runs}{The command line used to obtained the sqlite file}
\item{taxa}{The taxonomic information table}
\item{multiclass}{The multiclass table}
\item{placement_positions}{A data frame with the posiotn of each placement in the reference tree}
\item{arbre}{The tree in class "phylo" over wich placements are performed}
\item{edge_key}{A matrix with correspondance of node numbering between the original tree in the jplace file and the class phylo tree of the "arbre" component}
\item{original_tree}{The tree string from the jplace file}

For details on the other components (i.e. "placements, "placement_classifications", "placement_evidence", "placement_median_identities", "placement_names", "placement_nbc", "placements", "ranks" and "sqlite_sequence", please, refer to http://erick.matsen.org/pplacer/generated_rst/guppy_classify.html)
}
\author{
pierre lefeuvre
}
\references{
http://erick.matsen.org/pplacer/generated_rst/guppy_classify.html
}
\examples{
### the path to the sqlite and jplace files
sqlite_file <- system.file("extdata", "example.sqlite", package = "BoSSA")
jplace_file <- system.file("extdata", "example.jplace", package = "BoSSA")
pplace <- read_sqlite(sqlite_file,jplace_file)
}
