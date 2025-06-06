% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_meta.R
\name{table_meta}
\alias{table_meta}
\title{Generate Meta-Analysis Summary Table}
\usage{
table_meta(meta_result)
}
\arguments{
\item{meta_result}{A meta-analysis object of class \code{meta_ratio}, \code{meta_mean}, or \code{meta_prop}.}
}
\value{
A \code{gt} table summarizing study-level and pooled results.
}
\description{
Dispatches to the appropriate internal function to generate a summary table
depending on the class of the meta-analysis object.
}
