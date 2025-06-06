% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doi_plot.R
\name{doi_plot}
\alias{doi_plot}
\title{DOI Plot for Meta-Analyses with Fewer Than 10 Studies}
\usage{
doi_plot(
  object,
  save_as = c("viewer", "pdf", "png"),
  filename = NULL,
  width = 7,
  height = 7,
  ...
)
}
\arguments{
\item{object}{A meta-analysis object of class \code{meta_ratio}, \code{meta_mean}, or \code{meta_prop}.}

\item{save_as}{"viewer", "pdf", or "png". Default is "viewer".}

\item{filename}{Optional export filename.}

\item{width, height}{Plot size in inches. Defaults: 7x7.}

\item{...}{Additional arguments passed to \code{metasens::doiplot()}.}
}
\value{
Invisible NULL. Displays or saves the DOI plot.
}
\description{
Displays a DOI plot and LFK index using \code{metasens::doiplot()}. Recommended when fewer than 10 studies are included.
}
