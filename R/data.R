#' Dissolved oxygen measurements from the Tres Palacios River
#'
#' Data from the Texas Commission on Environmental Quality Surface Water Quality
#' Monitoring Information System. The `AverageDO`` field is the mean of dissolved oxygen
#' concentrations (mg/L) measured at a field site at that day. The MinDO is the minimum
#' dissolved oxygen concentration measured at that site on that day.
#'
#' @docType data
#'
#' @usage data(Dissolved_Oxygen)
#'
#' @format A data frame with 236 rows and 6 variables:
#' \describe{
#'   \item{Station_ID}{unique water quality monitoring station identifier}
#'   \item{Date}{sampling date in yyyy-mm-dd format}
#'   \item{Param_Code}{unique parameter code}
#'   \item{Param_Desc}{parameter description with units}
#'   \item{Average_DO}{mean of dissolved oxygen measurement, in mg/L}
#'   \item{Min_DO}{minimum of dissolved oxygen measurement, in mg/L}
#'   }
#' @source https://www80.tceq.texas.gov/SwqmisPublic/public/default.htm
#'
"Dissolved_Oxygen"


#' Enterococci bacteria measurements from the Tres Palacios River
#'
#' Data from the Texas Commission on Environmental Quality Surface Water Quality
#' Monitoring Information System. The `Value`` field is the lab measured value 
#' of Enterococci bacteria (MPN/100 mL) from grab samples collected at 
#' `Station ID` on the Tres Palacios River on `Date`. 
#'
#' @docType data
#'
#' @usage data(Entero)
#'
#' @format A data frame with 212 rows and 5 variables:
#' \describe{
#'   \item{Station_ID}{unique water quality monitoring station identifier}
#'   \item{Date}{sampling date in yyyy-mm-dd format}
#'   \item{Param_Code}{unique parameter code}
#'   \item{Param_Desc}{parameter description with units}
#'   \item{Value}{Enterococci concentration, in MPN/L}
#'   }
#' @source https://www80.tceq.texas.gov/SwqmisPublic/public/default.htm
#'
"Entero"