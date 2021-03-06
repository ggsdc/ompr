% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-api.R
\name{write_MPS}
\alias{write_MPS}
\title{Write the model as a .mps file}
\usage{
write_MPS(model, modelFile = NULL, returnMapping = 0, modelName = "OMPR1")
}
\arguments{
\item{model}{the model}

\item{modelFile}{the route and name of the .mps file. If none is give a random name is given.}

\item{returnMapping}{If the mapping should be returned or not for the user to use
(Should be returned if it is going to be solved if it is going to be solved with CBC).
By default it is 0 (not to return), change to 1 to return mapping.}

\item{modelName}{(optional). Sets the model name inside the file, default is "OMPR1"}
}
\value{
It writes the model to the indicated file and returns the mapping used (as a data.frame)
}
\description{
Write the model as a .mps file
}
\examples{
library(magrittr)
result <- MIPModel() \%>\%
 add_variable(x, type = "integer") \%>\%
 add_variable(y, type = "continuous", lb = 0) \%>\%
 set_bounds(x, lb = 0) \%>\%
 set_objective(x + y, "max") \%>\%
 add_constraint(x + y <= 11.25)

mapping <- write_MPS(model = model, file = "test.mps", modelname = "TEST")
}

