#' Read .csv data files
#'
#' This is a simple function that reads a .csv data file
#' based on the supplied file name. Additionally,
#' it converts loaded data into data frame tibble.
#' It stops program execution and print an error message
#' if non-existing file name is supplied.
#'
#' @param filename A character string giving the name
#' of a data file to read
#'
#' @return This function returns a data frame tibble which
#' is ready to parse
#'
#' @importFrom "dplyr" "tbl_df"
#'
#' @note .csv data file needs to be in your current directory
#' as the function does not take the whole file path but
#' filename only
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#' @export
fars_read <- function(filename) {
  fpath <- system.file("extdata",
                       filename,
                       package="farsFunctions")
  if(!file.exists(fpath))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(fpath, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create a file name of a .csv file
#'
#' This is a simple function that creates a file name
#' based on the supplied input argument
#'
#' @param year An argument (can be a string or numeric)
#' giving the text the function combines to create a file name
#'
#' @return This function returns a character string
#' of a .csv data file
#'
#' @examples
#' make_filename("2013")
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read specific .csv data files
#'
#' This is a function that reads a .csv data file based on
#' the specifically supplied argument. It uses lapply function
#' to call make_filename function and subsequently intends to
#' read a .csv data file using fars_read function based on the
#' newly created file name. The function uses tryCatch block
#' to evaluate the validity of the supplied argument. Within
#' the block, the function manipulates the data frame tibble
#' using dplyr package.
#'
#' @param years A vector of character strings or numeric objects
#' giving the specific year value the function will use to create
#' a new file name and subsequently read a .csv data file
#'
#' @return This function returns a list of manipulated data frame
#' tibbles that result from data parsing within tryCatch block.
#' If the supplied argument is incorrect, the error block is executed
#' and NULL value fills up the appropriate list slot.
#'
#' @importFrom "dplyr" "mutate" "select"
#' @importFrom "magrittr" "%>%"
#' @importFrom "rlang" ".data"
#'
#' @note .csv data file needs to be in your current directory
#' as the function does not take the whole file path but
#' filename only
#'
#' @examples
#' fars_read_years("2013")
#' fars_read_years(c(2013, 2014))
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(.data$MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize traffic accident data
#'
#' This is a function that summarizes the number of traffic accidents
#' based on the supplied argument. It takes advantage of
#' fars_read_years function. The list output of that function is then
#' parsed using "tidyverse" set of packages.
#'
#' @param years A vector of character strings or numeric objects
#' giving the specific year value the function will use to call
#' fars_read_years function
#'
#' @return This function returns a tibble which results
#' from data manipulation using "tidyverse" packages. It groups
#' the data by year and month and summarizes it resulting in a number
#' of traffic accidents per month in the previously specified year
#'
#' @importFrom "dplyr" "bind_rows" "group_by" "summarize"
#' @importFrom "tidyr" "spread"
#' @importFrom "magrittr" "%>%"
#'
#' @note .csv data file needs to be in your current directory
#' as the function does not take the whole file path but
#' filename only
#'
#' @examples
#' fars_summarize_years("2013")
#' fars_summarize_years(c(2013, 2014))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(.data$year, .data$MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(.data$year, n)
}

#' Create a map of accidents in a state
#'
#' This is a function that draws a map of traffic accidents in a given year and state
#' based on the specified arguments. It first uses make_filename and fars_read functions
#' to create a .csv filename and subsequently read it. Then it filters out the data based
#' on the specified state. Finally, the function plots the traffic accident locations in
#' the given state using maps and graphics packages
#'
#' @param state.num A character string or numeric object specifying the number of the state
#' the function will use to filter out the data and subsequently draw a map
#'
#' @param year A character string or numeric object giving the specific year value
#' the function will use to create a new file name, read a .csv data file and subsequently
#' create a map of traffic accidents locations
#'
#' @return This function returns a map of the specified state with
#' traffic accidents locations based on the specified year. If the state
#' number of invalid, the error message is printed out. If there is no
#' accidents to plot in a given year in a given state, the message is
#' printed out.
#'
#' @importFrom "dplyr" "filter"
#' @importFrom "maps" "map"
#' @importFrom "graphics" "points"
#' @importFrom "rlang" ".data"
#'
#' @note .csv data file needs to be in your current directory
#' as the function does not take the whole file path but
#' filename only
#'
#' @examples
#' fars_map_state(1, 2013)
#' fars_map_state("1", 2013)
#' fars_map_state("1", "2013")
#' fars_map_state(1, "2013")
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, .data$STATE == state.num)
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
