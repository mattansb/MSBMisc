#' Age in some units
#'
#' @param DOB,REFDATE Two dates
#' @param years,months,weeks,days the units.
#'
#' @example examples/examples.age_in_unit.R
#'
#' @export
age_in_unit <- function(DOB,
                        REFDATE = Sys.Date(),
                        years = TRUE,
                        months = TRUE,
                        weeks = TRUE,
                        days = TRUE) {
  .check_namespace("lubridate")

  diff <- lubridate::as.duration(lubridate::interval(DOB, REFDATE))
  age <- NULL

  if (years) {
    yy <- floor(diff / lubridate::dyears())

    age <- paste0(c(age, paste0(yy, " years")), collapse = ", ")
    diff <- diff - lubridate::dyears(yy)
  }

  if (months) {
    mm <- floor((diff / lubridate::dyears()) * 12)

    age <- paste0(c(age, paste0(mm, " months")), collapse = ", ")
    diff <- diff - lubridate::dyears(mm / 12)
  }

  if (weeks) {
    ww <- floor(diff / lubridate::dweeks())

    age <- paste0(c(age, paste0(ww, " weeks")), collapse = ", ")
    diff <- diff - lubridate::dweeks(ww)
  }

  if (days) {
    dd <- floor(diff / lubridate::ddays())

    age <- paste0(c(age, paste0(dd, " days")), collapse = ", ")
  }

  return(age)
}
