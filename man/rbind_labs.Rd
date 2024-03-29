% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combine_labs.R
\name{rbind_labs}
\alias{rbind_labs}
\title{Title}
\usage{
rbind_labs(
  lab_list = cdcfluview::who_nrevss("state"),
  lab_name = c("clinical_labs", "combined_prior_to_2015_16")
)
}
\arguments{
\item{lab_list}{(The dataset that contains lists of dataframes)}

\item{lab_name}{(The desired labs that want to be analyzed)}
}
\value{
(A dataframe that combines all desired labs in lab_name)
}
\description{
Title
}
\examples{
combinedDF = rbind_labs()
}
