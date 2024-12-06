#' Canvas API helpers
#'
#' These functinos set your Canvas API token, as well as the Canvas base URL.
#' These functions are necessary for `rcanvas` to run.
#'
#' @name apihelpers
#' @md

#' @param token your API token
#' @export
#' @rdname apihelpers
#' @examples
#' set_canvas_token("abc123")
set_canvas_token <- function(token) {
  Sys.setenv(CANVAS_API_TOKEN = token)
}

#' @param domain Canvas domain
#' @export
#' @rdname apihelpers
#' @examples
#' set_canvas_domain("https://canvas.upenn.edu")
set_canvas_domain <- function(domain) {
  Sys.setenv(CANVAS_DOMAIN = domain)
}

#' @rdname apihelpers
check_token <- function() {
  token <- Sys.getenv("CANVAS_API_TOKEN")
  if (identical(token, "")) {
    stop("Please set env var CANVAS_API_TOKEN to your access token.",
         call. = FALSE)
  }
  token
}

canvas_url <- function() paste0(Sys.getenv("CANVAS_DOMAIN"), "/api/v1/")

canvas_query <- function(urlx, args, type = "GET") {
  fun <- getFromNamespace(type, "httr")
  args <- sc(args)
  resp <- fun(urlx,
              httr::user_agent("Ze adam - https://github.com/icto-psy"),
              httr::add_headers(Authorization = paste("Bearer", check_token())),
              query = args)
  httr::stop_for_status(resp)
  return(resp)
}

iter_args_list <- function(x, label) {
  ln <- list()
  for (i in seq_along(x)) {
    ln[[i]] <- x[i]
    names(ln)[[i]] <- label
  }
  ln
}

sc <- function(x) {
  Filter(Negate(is.null), x)
}

convert_dates <- function(base_date = Sys.Date(), days) {
  new_date <- base_date + lubridate::ddays(days)
  format(new_date, "%Y-%m-%d")
}


#' Process a Canvas API response
#'
#' Wrapper function for common tasks in going from Canvas url to dataframe. Most
#' of the heavy lifting is done in \code{paginate}, which finds which pages to
#' download. This function adds necessary arguments to those pages (e.g. the
#' authentication token), downloads the content, converts from JSON into data
#' frame format, and if there are multiple pages/dataframes, converts it into
#' one final dataframe.
#'
#' @param url url to query
#' @param args query arguments to be passed to \code{httr}, e.g. auth token
#'
#' @return processed dataframe
#'
process_response <- function(url, args) {
  resp <- canvas_query(url, args, "GET")
  df <-  purrr::map(paginate(resp), httr::content, "text")
  df <-  purrr::map(df, jsonlite::fromJSON, flatten = TRUE)
  df <- tryCatch({
     purrr::map_df(df, purrr::flatten_df)
  },
  error = function(e) {
     dplyr::bind_rows(df)
  }
  )
  return(df)
}

#' @title Get responses from Canvas API pages
#'
#' @description The Canvas headers include a link object (usually), in form:
#' \code{Link:
#' <https://canvas.url/api/v1/[...]?page=1&per_page=10>; rel="current",
#' <https://canvas.url/api/v1/[...]?page=2&per_page=10>; rel="next",
#' <https://canvas.url/api/v1/[...]?page=1&per_page=10>; rel="first",
#' <https://canvas.url/api/v1/[...]?page=15&per_page=10>; rel="last"}
#'
#' In this case, we need to download every page from 1 to 15 to capture all data.
#' This function parses the response object intelligently, using only HEAD
#' requests, to figure out these page requirements.
#'
#' @param x a httr response object
#'
#' @return unparsed responses
#'
#' @examples
#' \dontrun{resp <- canvas_query(url, args, "HEAD")
#' get_pages(resp)}
paginate <- function(x) {
  first_response <- list(x)
  stopifnot(httr::status_code(x) == 200) # OK status
  pages <- httr::headers(x)$link
  if (is.null(pages)) return(first_response)
  should_continue <- TRUE
  inc <- 2
  if (has_rel(pages, "last")) {
    last_page <- get_page(x, "last")
    n_pages <- readr::parse_number(stringr::str_extract(last_page, "page=[0-9]{1,}"))
    if (n_pages == 1) return(first_response)
    pages <- increment_pages(last_page, 2:n_pages)
    responses <- purrr::map(pages, canvas_query, args = list(access_token = check_token()))
    responses <- c(first_response, responses)
    return(responses)
  } else if (has_rel(httr::headers(x)$link, "next")) {
    # edge case for if there is no 'last' header, see:
    # https://canvas.instructure.com/doc/api/file.pagination.html
    # https://github.com/daranzolin/rcanvas/issues/4
    while (should_continue) {
      page_temp <- get_page(x, "next")
      pages[[inc]] <- page_temp
      x <- canvas_query(page_temp,
                        args = list(access_token = check_token()),
                        type = "HEAD")
      if (!has_rel(httr::headers(x)$link, "next")) {
        should_continue <- FALSE
      } else {
        inc <- inc + 1
      }
    }
    pages[1] <- gsub("page=2","page=1",pages[2])
    responses <- purrr::map(pages, canvas_query, args = list(access_token = check_token()))
  }
}

increment_pages <- function(base_url, n_pages) {
  # odd regex but necessary, see http://regexr.com/3evr4
  stringr::str_replace(base_url, "([\\?&])(page=[0-9a-zA-Z]{1,})",
                       sprintf("\\1page=%s", n_pages))
}

has_rel <- function(x, rel) {
  stopifnot(!is.null(rel))
  any(grepl(paste0("rel=\"", rel, "\""), x))
}

get_page <- function(resp, page) {
  pages <- resp$headers$link
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  pages <- stringr::str_split(pages, ",")[[1]]
  url <- stringr::str_subset(pages, page)
  url <- stringr::str_extract(url, url_pattern)
  url <- stringr::str_replace_all(url, "[<>;]", "")
  return(url)
}