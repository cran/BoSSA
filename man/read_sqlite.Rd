\name{read_sqlite}
\alias{read_sqlite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read a pplacer/guppy sqlite file
}
\description{
read a pplacer/guppy sqlite file
}
\usage{
read_sqlite(SQLITE, jplace_file = NULL, rank = "species")
}
\arguments{
  \item{SQLITE}{
a pplacer/guppy sqlite file name
}
  \item{jplace_file}{
an optionnal jplace file name. By default, the jplace file path is obtained from the sqlite file.
}
  \item{rank}{
the desired taxonomic assignation rank to extract
}
}
\details{
the tree information are not contained in the sqlite file (obtained using guppt on the jplace file) but in the jplace file (obtained using pplacer). the path of the jplace file is available in the sqlite file. if for some reason this sqlite file is wrong (e.g. you move the file), you can indicate a new path for the japlce file using the jplace_file argument.
}
\value{
a list with
\item{run}{the command line used to obtained the sqlite file}
\item{taxo}{the taxonomic information table}
\item{multiclass}{the multiclass table}
\item{placement}{the placement table}
\item{arbre}{the tree in class "phylo" over wich placements are performed}
}
\author{
pierre lefeuvre
}
\examples{
### the path to the sqlite and jplace files
sqlite_file <- system.file("extdata", "example.sqlite", package = "BoSSA")
jplace_file <- system.file("extdata", "example.jplace", package = "BoSSA")
### the default is to read the jplace file from the path available in the "run" item.
### Since the path is different there, you must provide the jplace file path (here "jplace_file")
pplace <- read_sqlite(sqlite_file,jplace_file=jplace_file)
}
