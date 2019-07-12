#' fars_read
#'
#' This function takes a string (file name), and returns a data frame containing the corresponding data.
#' If the filename is not correct it will results in an error.
#'
#' @param filename A string giving the file name of dataset
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @return A dataframe containing the corresponding data.
#' @export
#'

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' make_filename
#'
#' This function takes a string (year), and returns a string which is the file name of
#' the data for that particular year. If the year is not 2013-2015, there will be an error.
#'
#' @param year  string which defines a year, 2013, 2014, or 2015
#'
#' @return A file name (string) for the data corresponding to a particular year.
#' @export
#'


make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years
#'
#' This function does the same as fars_read, but takes a vector of strings (years)
#' and returns a list containing dataframes.If the years are not 2013-2015, there will be an error.
#'
#' @param years   A vector containing strings which contains 1 or more years.
#'
#' @importFrom dplyr tbl_df mutate select
#' @return A list of dataframes
#' @export
#'


fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
#' fars_summarize_years
#'
#' This function takes a vector of strings (years) and returns a tibble containing
#' the number of fatal injuries by year and month.If the years are not 2013-2015, there will be an error.
#'
#' @param years   A vector containing strings which contains 1 or more years.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @return A tibble containing the number of fatal injuries by year and month
#' @export
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' fars_map_state
#'
#' This function takes a state number and year, and returns a plot showing all traffic fatalities.
#' If the years are not 2013-2015, or the number doesn't correspond to a state, there will be an error.
#'
#' @param state.num A number which represents a state
#' @param year  string which defines a year, 2013, 2014, or 2015
#'
#'
#' @importFrom dplyr  filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @return A plot shoing all traffic fatalities.
#' @export
#'

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}




