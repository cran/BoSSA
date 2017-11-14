\name{read_jplace}
\alias{read_jplace}
\title{
Read a jplace file
}
\description{
Read a jplace file
}
\usage{
read_jplace(JPLACE, full = TRUE)
}
\arguments{
  \item{JPLACE}{
A jplace file name
}
  \item{full}{
If set to FALSE, only the tree is read from the jplace file
}
}
\value{
A list with
\item{arbre}{The tree in class "phylo" over wich placements are performed}
\item{placement}{The placement table}
\item{multiclass}{The multiclass table}
}
\author{
pierre lefeuvre
}
\seealso{
read_sqlite
}
