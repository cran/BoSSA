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
\note{
Note that the placement mass (potentially) available from the original files are imported into R but aren't use in the analysis. Anyway, the write_jplace function takes into account possible weight/mass information available in the the "nm" column of the multiclass table for jplace objects and in the "mass" column from the placement_names table for the pplace objects. The values in these column can be edited before writing the jplace file if one want to use distinct mass/weight in downtstream analysis (e.g. using the guppy program functionalities).
}
\author{
pierre lefeuvre
}
\examples{

data(pplace)
\dontrun{
write_jplace(pplace,"test.jplace")
}

}
