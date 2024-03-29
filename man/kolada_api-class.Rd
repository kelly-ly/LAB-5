% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_access.R
\docType{class}
\name{kolada_api-class}
\alias{kolada_api-class}
\alias{kolada_api}
\title{KOLADA API Reference Class Object}
\arguments{
\item{muni}{data frame of municipality}

\item{kpi}{data frame of KPI}

\item{search_type}{id type that user selected}

\item{input_str}{id input by users}
}
\description{
KOLADA API Reference Class Object
}
\section{Methods}{

\describe{
\item{\code{search_data(input_kpi, input_municipality, input_year)}}{Search data when all three parameters or only two are given.Functions returns a data frame.}

\item{\code{search_ou(input_kpi, input_ou, input_year)}}{Search orgranization unit when all three parameters or only two are given.Functions returns a data frame.}

\item{\code{search_with_id(search_type, input_str)}}{user select search type and input the id, function returns a data frame of result}

\item{\code{search_with_title(search_type, input_str)}}{user select search type and input the title, function returns a data frame of result}
}}

\examples{
kolada_mod <- kolada_api$new()
kolada_mod$search_with_title(search_type = "municipality", input_str = "lund")
kolada_mod$search_with_id(search_type = "municipality_groups", input_str = "G124026")
kolada_mod$search_data(input_kpi="N00945",input_municipality="1860",input_year="")
kolada_mod$search_ou("N15033,N15030","V15E144001301,V15E144001101","2009,2008,2007")
}
