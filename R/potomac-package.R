#' @title Potomac River Kayaking Safety Analysis
#'
#' @description
#' Predictive analytics for kayaking safety on the Potomac River,
#' specifically calibrated for Little Falls conditions. This package provides
#' real-time data integration with USGS water services, safety scoring, and
#' 7-day forecasting to help kayakers plan safe trips in advance.
#'
#' @details
#' The package provides several key functions:
#'
#' \itemize{
#'   \item \code{\link{predict_kayaking_safety}}: Main function that generates a complete safety assessment and 7-day forecast
#'   \item \code{\link{calculate_safety_metrics}}: Calculates safety metrics based on flow rate and trend
#'   \item \code{\link{fetch_realtime_data}}: Retrieves real-time water flow data from USGS
#'   \item \code{\link{assess_safety_status}}: Evaluates safety metrics and generates notifications
#' }
#'
#' The package also includes a Shiny dashboard for interactive visualization of the safety analysis.
#'
#' @section Data Sources:
#' The package uses data from the following sources:
#'
#' \itemize{
#'   \item USGS Water Services API - Real-time and historical flow data
#'   \item Site-specific Little Falls calibration data
#' }
#'
#' @section Safety Algorithm:
#' The safety scoring algorithm considers multiple factors:
#'
#' \itemize{
#'   \item Flow Rate (40 points): Based on optimal ranges for Little Falls
#'   \item Trend Stability (30 points): How quickly water levels are changing
#'   \item Seasonal Factor (20 points): Time of year affects safety
#'   \item Experience Bonus (10 points): Bonus for ideal learning conditions
#' }
#'
#' @docType package
#' @name potomac-package
#' @aliases potomac
NULL