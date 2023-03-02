#' Add an event to the course calendar
#'
#' @description See https://canvas.instructure.com/doc/api/calendar_events.html for all options
#' @param course_id ID of the course
#' @param title event title
#' @param location location, e.g room
#' @param repeats number of repeats or 0 (single event)
#' @param frequency if repeats>0, corresponding frequency (otherwise ignored)
#' @param iterator boolean, add an event number when there are repeated events
#' @param day if repeats>0, corresponding frequency (otherwise ignored)
#' @param start_time start_time
#' @param end_time end_time
#' @param timezone timezone
#' @param ... any other arguments passed to the API
#' @importFrom glue glue
#' @export
#' @example
#' add_event(title = "This is a test", day = Sys.Date(), repeats=3, frequency='daily')
#'
#' # multiple events organised as a data.frame
#' library(purrr);library(dplyr)
#' le = data.frame(title = c('a','b'), day = Sys.Date() + c(2,3))
#' responses = purrr::pmap(dplyr::rowwise(le), add_event)
add_event <- function(course_id = 16248,
                      title = "Murphy's Laws",
                      location = "MURPH101",
                      repeats = 0,
                      frequency = c("weekly","monthly","daily"),
                      iterator = FALSE,
                      day = Sys.Date(),
                      start_time = "08:00:00",
                      end_time = "10:00:00",
                      timezone="Pacific/Auckland",
                      ...) {

  frequency <- match.arg(frequency)

  url <- fs::path(canvas_url(), "calendar_events.json")

  args <- list("calendar_event[context_code]" = glue::glue("course_{course_id}"),
               "calendar_event[title]"=title,
               "calendar_event[location_name]"=location,
               "calendar_event[start_at]"=glue::glue("{day}T{start_time}"),
               "calendar_event[end_at]"=glue::glue("{day}T{end_time}"),
               "calendar_event[time_zone_edited]"=timezone, ...)
  if(repeats > 0) {
    args <- c(args, list(
      "calendar_event[duplicate][count]"=repeats - 1, # it's actually the total count
      "calendar_event[duplicate][frequency]"=frequency,
      "calendar_event[duplicate][append_iterator]",iterator
    ))
  }
  canvas_query(url, args, "POST")
}

#' @export
get_events <- function(course_id=16248, start_date = NULL, end_date = NULL) {
  if (!grepl(pattern = "course", x = course_id)) {
    course_id <- paste0("course_", course_id)
  }
  url <- fs::path(canvas_url(), "users/self/calendar_events")
  args <- list(per_page = 500, all_events = "true")
  include <- iter_args_list(course_id, "context_codes[]")
  include2 <- iter_args_list(start_date, "start_date")
  include3 <- iter_args_list(end_date, "end_date")
  args <- c(args, include, include2, include3)
  dat <- process_response(url, args)
  dat
}

#' @export
delete_event <- function(event_id, cancel_reason = 'mess') {

  url <- glue::glue("{canvas_url()}/calendar_events/{event_id}")
  args <- iter_args_list(cancel_reason, "cancel_reason")
  dat <- process_response(url, args, query_type="DELETE")
  dat
}
