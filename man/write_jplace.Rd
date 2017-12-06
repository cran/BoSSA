\name{write_jplace}
\alias{write_jplace}
\title{
Write a jplace or pplace object to the disk
}
\description{
Write a jplace or pplace object to the disk in the jplace JSON format
}
\usage{
write_jplace(x,outfile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A pplace or jplace object
}
  \item{outfile}{
The name of the output file
}
}
\author{
pierre lefeuvre
}
\examples{

data(pplace)

write_jplace(pplace,"test.jplace")

}
