% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LandTrendr.AGB.R
\name{extract.LandTrendr.AGB}
\alias{extract.LandTrendr.AGB}
\title{extract.LandTrendr.AGB}
\usage{
extract.LandTrendr.AGB(
  site_info,
  dataset = "median",
  buffer = NULL,
  fun = "mean",
  data_dir = NULL,
  product_dates = NULL,
  output_file = NULL,
  ...
)
}
\arguments{
\item{site_info}{list of site info for parsing AGB data: list(site_id, site_name, lat, lon, time_zone)}

\item{dataset}{Which LandTrendr dataset to parse, "median" or "stdv".Default: "median"}

\item{buffer}{Optional. operate over desired buffer area (not yet implemented)}

\item{fun}{Optional function to apply to buffer area.  Default - mean}

\item{data_dir}{directory where input data is located. Can be NUL if con is specified}

\item{product_dates}{Process and extract data only from selected years. Default behavior
(product_dates = NULL) is to extract data from all availible years in BETYdb or data_dir}

\item{output_file}{Path to save LandTrendr_AGB_output.RData file containing the 
output extraction list (see return)}

\item{...}{Additional arguments, currently ignored}
}
\value{
list of two containing the median AGB values per pixel and the corresponding 
standard deviation values (uncertainties)
}
\description{
extract.LandTrendr.AGB
}
\examples{
\dontrun{

# Example 1 - using BETYdb site IDs to extract data
# Database connection (optional)

con <- PEcAn.DB::db.open(
  list(user='bety', password='bety', host='localhost',
  dbname='bety', driver='PostgreSQL',write=TRUE))

site_ID <- c(2000000023,1000025731,676,1000005149) # BETYdb site IDs
suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon, 
ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})", 
ids = site_ID, .con = con))
suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
site_info <- list(site_id=qry_results$id, site_name=qry_results$sitename, lat=qry_results$lat, 
lon=qry_results$lon, time_zone=qry_results$time_zone)
data_dir <- "~/scratch/agb_data/"

results <- extract.LandTrendr.AGB(site_info, "median", buffer = NULL, fun = "mean", 
data_dir, product_dates, output_file)

}

}
\author{
Shawn Serbin, Alexey Shiklomanov
}
