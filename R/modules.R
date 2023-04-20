# https://canvas.instructure.com/doc/api/modules.html
#


# curl -H 'Authorization: Bearer <token>' \
# https://<canvas>/api/v1/courses/222/modules

#' @export
#'
list_modules <- function(course_id){
  # GET /api/v1/courses/:course_id/front_page
  url <- fs::path(canvas_url(), file.path("courses", course_id, "modules"))
  resp <- process_response(url, args = "")
  return(resp)

}

#' Create page in course
#'
#' @param course_id a valid course id
#' @param title a string.  The title for the new page.
#' @param body a string	The content for the new page.
#' @param editing_roles a string.  Which user roles are allowed to edit this page. Any combination of these roles is allowed (separated by commas).  Allowed values: teachers, students, members, public
#' @param published a boolean.	Whether the page is published (true) or draft state (false).
#'
#' @return empty
#' @export
#'
create_module <- function(course_id, module='test module', position=2, prerequisite=121){

  # curl https://<canvas>/api/v1/courses/<course_id>/modules \
  # -X POST \
  # -H 'Authorization: Bearer <token>' \
  # -d 'module[name]=module' \
  # -d 'module[position]=2' \
  # -d 'module[prerequisite_module_ids][]=121' \
  # -d 'module[prerequisite_module_ids][]=122'


  url <- fs::path(canvas_url(), file.path("courses", course_id, "modules"))
  args <- sc(list(`module[name]` = module,
                  `module[position]` = position,
                  `module[prerequisite_module_ids][]` = prerequisite
  ))

  resp <- canvas_query(url, args, "POST")

  httr::stop_for_status(resp)
  message(sprintf("Module '%s' created", module))
  return(resp)

}




#' Create item in module
#'
#' @param course_id a valid course id
#' @param title a string.  The title for the new page.
#' @param body a string	The content for the new page.
#' @param editing_roles a string.  Which user roles are allowed to edit this page. Any combination of these roles is allowed (separated by commas).  Allowed values: teachers, students, members, public
#' @param published a boolean.	Whether the page is published (true) or draft state (false).
#'
#' @return empty
#' @export
#'
create_module_item <- function(course_id, module_id, title = 'Item',
                               type=c('Page', 'SubHeader', 'ExternalUrl'),
                               indent = 1, position=2, page_url="dummy-page-url"){
  # curl https://nuku.wgtn.ac.nz/api/v1/courses/  /modules/  /items \
  # -X POST \
  # -d 'module_item[title]=module item' \
  # -d 'module_item[type]=Page' \
  # -d 'module_item[position]=2' \
  # -d 'module_item[indent]=1' \
  # -d 'module_item[page_url]=dummy-page-url'

  type <- match.arg(type)

  url <- fs::path(canvas_url(), file.path("courses", course_id, "modules", module_id, "items"))
  args <- sc(list(`module_item[title]` = title,
                  `module_item[type]` = type,
                  `module_item[position]` = position,
                  `module_item[indent]` = indent,
                  `module_item[page_url]` = page_url
  ))

  resp <- canvas_query(url, args, "POST")
  print(resp)
  httr::stop_for_status(resp)
  message(sprintf("Item '%s' created", title))
  return(resp)

}



