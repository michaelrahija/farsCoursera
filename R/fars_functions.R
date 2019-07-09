
#' This set of functions are used to analyze the Fatality Analysis Reporting datasets.
#'
#' \itemize{} 	\code{fars_read()} takes a string (file name), and returns a data frame containing the corresponding data.
#' If the filename is not correct it will results in an error.
#' \itemize{} 	\code{make_file()} takes a string (year), and returns a string which is the file name of
#' the data for that particular year. If the year is not 2013-2015, there will be an error.
#' \itemize{} 	\code{fars_read_years()} does the same as fars_read, but takes a vector of strings (years)
#' and returns a list containing dataframes.If the years are not 2013-2015, there will be an error.
#' \itemize{} \code{fars_summarize_years()} takes a vector of strings (years) and returns a tibble containing
#' the number of fatal injuries by year and month.If the years are not 2013-2015, there will be an error.
#' \itemize{} \code{fars_map_state()} takes a state number and year, and returns a plot showing all traffic fatalities.
#' If the years are not 2013-2015, or the number doesn't correspond to a state, there will be an error.
#'
#' @param filename A string giving the file name of dataset
#' @param year A string which defines a year, 2013, 2014, or 2015
#' @param years A svector containing strings which contains 1 or more years.
#' @param state.num A number which represents a state
#'
#' @return See description.
#'
#' @export
#' @examples
#' head(fars_read("accident_2013.csv.bz2"))
#' make_filename("2014")
#' fars_read_years(c("2014","2015"))
#' fars_summarize_years(c("2014","2015"))
#' fars_map_state(state.num = "10", year = "2015")
#' @importFrom readr read_csv
#' @importFrom tidyr spread
#' @importFrom dplyr tbl_df mutate select bind_rows group_by summarize filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @name FARSfunctions

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' @rdname FARSfunctions
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' @rdname FARSfunctions
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

#' @rdname FARSfunctions
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' @rdname FARSfunctions
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


