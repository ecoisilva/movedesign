#' Fix rates of animal tracking devices.
#'
#' A dataset listing typical GPS fix rates for animal tracking devices.
#' Useful for selecting typical sampling schedules in wildlife tracking
#' projects.
#' 
#' @docType data
#' 
#' @format A data.frame with 40 rows and 7 variables:
#' \describe{
#'   \item{dti_notes}{
#'     Human-readable fix schedule, e.g., "1 fix every month".
#'     Helps interpret sampling intervals in practical terms.}
#'   \item{dti}{
#'     Sampling interval in seconds, i.e., time between consecutive
#'     location fixes.}
#'   \item{frq}{
#'     Sampling frequency in seconds, i.e., how often a fix occurs
#'     (inverse of dti).}
#'   \item{frq_hrs}{
#'     Sampling frequency in hours, offering a more intuitive
#'     unit for comparison.}
#'   \item{highlighted}{
#'     Logical. TRUE if the fix rate is commonly used in
#'     animal tracking studies. Useful for identifying standard
#'     settings.}
#'   ...
#' }
"fixrates"


#' Table of movement processes.
#'
#' Lists all continuous-time movement process models in \pkg{ctmm}.
#' Each row is a different movement model applicable for animal movement.
#'
#' @docType data
#'
#' @format A \code{data.frame} with 5 rows and 6 variables:
#' \describe{
#'   \item{name}{Full descriptive name of the model (e.g., "Ind. Ident.
#'     Distr. (IID)"). Used throughout `ctmm`. See reference for more
#'     details on each model and their properties.}
#'   \item{name_short}{Abbreviated name, used where space is limited.}
#'   \item{tau_p}{Logical. TRUE if the model includes the position
#'     autocorrelation timescale (i.e., home range crossing time).}
#'   \item{tau_v}{Logical. TRUE if the model includes the velocity
#'     autocorrelation timescale (i.e., directional persistence).}
#'   \item{hrange}{Logical; TRUE if the model supports range residency,
#'     meaning the animal is likely to remain within a bounded area or
#'     "home range" instead of expanding indefinitely.}
#'   \item{pars}{Character string summarizing which autocorrelation
#'     parameters (e.g., tau_p, tau_v) the model estimates. Shown in HTML
#'     for documentation.}
#'   ...
#' }
#' 
#' @references
#'   * Calabrese et al. (2016). `ctmm`: an `R` package for analyzing
#'     animal relocation data as a continuous-time stochastic process.
#'     Methods in Ecology and Evolution, 7(9), 1124-1132
#'     <doi:10.1111/2041-210X.12559>.
#'   * Silva et al. (2022). Autocorrelation‐informed home range
#'     estimation: A review and practical guide. Methods in Ecology and
#'     Evolution, 13(3), 534-544 <10.1111/2041-210X.13786>.
#'
"movmods"
