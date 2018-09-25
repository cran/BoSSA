\name{read_jplace}
\alias{read_jplace}
\title{
Read a jplace file
}
\description{
Read a jplace file
}
\usage{
read_jplace(jplace_file, full = TRUE)
}
\arguments{
  \item{jplace_file}{
A jplace file name
}
  \item{full}{
If set to FALSE, only the tree is read from the jplace file
}
}
\details{
When the jplace or sqlite files are imported into R, the node numbering available in the original
file is converted to the class "phylo" numbering. The class phylo is defined in the "ape" package.
}
\value{
A list with
\item{arbre}{The tree in class "phylo" over wich placements are performed}
\item{placement}{The placement table}
\item{multiclass}{The multiclass table}
\item{run}{The command line used to obtained the jplace file}
}
\author{
pierre lefeuvre
}
\seealso{
read_sqlite
}
