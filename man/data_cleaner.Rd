% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cmt_data_cleaner.R
\name{cmt_data_cleaner}
\alias{cmt_data_cleaner}
\title{Data Preprocessing Function}
\usage{
cmt_data_cleaner(cognos.data, clientstatus.data, tat.data)
}
\arguments{
\item{cognos.data}{Data that comes from the Cognois CMT report.}

\item{clientstatus.data}{Data that comes from the at-risk report.}

\item{tat.data}{Data that comes from Snowflake.}
}
\description{
This function allows you to preprocess the data that is needed as input into the three risk models.
}
