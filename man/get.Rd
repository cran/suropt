% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.R
\name{get}
\alias{get}
\alias{get_feasibility}
\alias{get_entropy}
\alias{get_stats}
\title{get: get prediction data of models}
\usage{
get_feasibility(model, newdata)

get_entropy(model, newdata)

get_stats(model, newdata, slot = "all", stats = c("mean", "sd"))
}
\arguments{
\item{model}{suropt model}

\item{newdata}{input data where predictions will be done}

\item{slot}{character indicating slot, must be 'y', 'g' or 'all' (default)}

\item{stats}{character indicating statistics to be taken, must be one or some of
'mean', 'sd', 'lower95', 'upper95' or 'trend'}
}
\description{
Get prediction dafa of models
}
\details{
Bla
}
