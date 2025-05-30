% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta_reg.R
\name{meta_reg}
\alias{meta_reg}
\title{Meta-regression for meta-analysis results}
\usage{
meta_reg(meta_object, data, moderators, studylab)
}
\arguments{
\item{meta_object}{A fitted object from meta_ratio, meta_mean, or meta_prop.}

\item{data}{The original dataset used to fit the meta-analysis model.}

\item{moderators}{A formula specifying the moderators (e.g., ~ age + gender).}

\item{studylab}{Character string naming the study label column in \code{data} (e.g., "author").}
}
\value{
A list of class 'meta_reg' with the regression model, tidy coefficients, R² analog, and tau² estimates.
}
\description{
Performs meta-regression on a fitted meta-analysis object from meta_ratio, meta_mean, or meta_prop.
}
