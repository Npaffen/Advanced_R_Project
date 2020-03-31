# load packages -------------------------------------------------
library(lubridate)
library(purrr)
library(dplyr)

make_dates <- function(year = 2020L,
                       month = 1L,
                       from_day = 1L,
                       to_day = 2L,
                       all_dates = FALSE) {
  stopifnot(is.integer(as.integer(year))) # accepts quoted year.
  stopifnot(all(between(as.integer(month), 1, 12)))
  stopifnot(all(between(c(
    as.integer(from_day),
    as.integer(to_day)
  ), 1, 31)))


  if (missing(to_day) && !missing(from_day)) to_day <- from_day
  stopifnot(from_day <= to_day)

  if (all(
    missing(year), missing(month),
    missing(from_day), missing(to_day)
  )) {
    return(today())
  } else {
    if (all_dates) {
      n <- length(month)
      to_day <- vector("integer", n)
      if (n == 12) {
        to_day[[n]] <- day(ymd(paste(
          year + 1,
          "01",
          "01"
        )) - 1) # for December

        for (mon in 1:(n - 1)) {
          to_day[[mon]] <- day(ymd(paste(
            year,
            mon + 1, "01"
          )) - 1)
        }
      } else {
        for (mon in 1:n) {
          to_day[[mon]] <- day(ymd(paste(
            year,
            mon + 1, "01"
          )) - 1)
        }
      }
    }
  }

  a <- ymd(paste(year, month, from_day, sep = "-"))
  b <- map2(month, to_day, function(x, y) {
    ymd(paste(year, x, y))
  })

  dds <- map2(a, b, ~ seq.Date(.x, .y, by = 1))
  labels <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
  set_names(dds, labels[month])
}
