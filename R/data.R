#' Fix rates of animal tracking devices.
#'
#' Dataset with a list of fix rates typically available for animal tracking devices.
#' 
#' @docType data
#' 
#' @format An object of class \code{"data.frame"},
#' with 40 rows and seven variables: \code{dti_notes}, \code{dti},
#' \code{highlight}, \code{frq}, and \code{frq_hrs}.
#'
#' \describe{
#'   \item{dti_notes}{Sampling interval as text (e.g., "1 fix every month")}
#'   \item{dti}{Sampling interval in seconds}
#'   \item{frq}{Sampling frequency in seconds}
#'   \item{frq_hrs}{Sampling frequency in hours}
#'   \item{highlighted}{Highlight a group of commonly-used GPS fix rates}
#'   ...
#' }
"fixrates"

#' Table of movement processes.
#'
#' Table listing all current movement processes used
#' within the ctmm R package.
#'
#' @docType data
#'
#' @format An object of class \code{"data.frame"},
#' with five rows and six variables: \code{name}, \code{name_short},
#' \code{tau_p}, \code{tau_v}, \code{hrange}, and \code{pars}.
#'
#' @references Calabrese et al. (2016). Methods in Ecology and Evolution, 7(9), 1124-1132.
#' (\href{https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12559}{MEE})
#'
#' \describe{
#'   \item{name}{Movement process names (e.g., "Ind. Ident. Distr. (IID)")}
#'   \item{name_short}{Abbreviated movement process names}
#'   \item{tau_p}{A Boolean denoting whether or not the movement process includes position autocorrelation}
#'   \item{tau_v}{A Boolean denoting whether or not the movement process includes velocity autocorrelation}
#'   \item{hrange}{A Boolean denoting whether or not the movement process includes range residency}
#'   \item{pars}{Character variable, to display autocorrelation parameters in HTML}
#'   ...
#' }
"movmods"


#' Home range error simulations.
#'
#' Expanded simulations from Silva et al. (2021).
#' 
#' @docType data
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @references Silva et al. (2021). Methods in Ecology and Evolution, 13(3), 534-544.
#' (\href{https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13786}{MEE})
#'
"sims_hrange"


#' Speed & distance error simulations.
#' 
#' Expanded simulations from Noonan et al. (2019) and Silva et al. (in prep).
#' 
#' @docType data
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @references Noonan et al. (2019).  Movement Ecology, 7(1), 1-15.
#' (\href{https://movementecologyjournal.biomedcentral.com/articles/10.1186/s40462-019-0177-19}{ME})
#'
"sims_speed"
