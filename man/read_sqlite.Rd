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
read_sqlite(sqlite_file,jplace_file=gsub("sqlite","jplace",sqlite_file),
rank="species")
}
\arguments{
  \item{sqlite_file}{
A pplacer/guppy sqlite file name
}
  \item{jplace_file}{
An optionnal jplace file name. By default, a jplace file with the same prefix as the sqlite file is made. If not, the jplace file path/name as to be specified.
}
  \item{rank}{
The desired taxonomic assignation rank to extract
}
}
\details{
As the tree informations are not available in the sqlite file, the jplace file is also required.
}
\value{
A list with
\item{run}{The command line used to obtained the sqlite file}
\item{taxo}{The taxonomic information table}
\item{multiclass}{The multiclass table}
\item{placement}{The placement table}
\item{arbre}{The tree in class "phylo" over wich placements are performed}
}
\author{
pierre lefeuvre
}
\examples{
### the path to the sqlite and jplace files
sqlite_file <- system.file("extdata", "example.sqlite", package = "BoSSA")
jplace_file <- system.file("extdata", "example.jplace", package = "BoSSA")
pplace <- read_sqlite(sqlite_file,jplace_file=jplace_file)
}
