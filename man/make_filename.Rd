% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_function.R
\name{make_filename}
\alias{make_filename}
\title{Create a file name of a .csv file}
\usage{
make_filename(year)
}
\arguments{
\item{year}{An argument (can be a string or numeric)
giving the text the function combines to create a file name}
}
\value{
This function returns a character string
of a .csv data file
}
\description{
This is a simple function that creates a file name
based on the supplied input argument
}
\examples{
make_filename("2013")

}
