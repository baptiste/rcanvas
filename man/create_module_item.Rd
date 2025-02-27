% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modules.R
\name{create_module_item}
\alias{create_module_item}
\title{Create item in module}
\usage{
create_module_item(
  course_id,
  module_id,
  title = "Item",
  type = c("Page", "SubHeader", "ExternalUrl"),
  indent = 1,
  position = 2,
  page_url = "dummy-page-url"
)
}
\arguments{
\item{course_id}{a valid course id}

\item{title}{a string.  The title for the new page.}

\item{body}{a string    The content for the new page.}

\item{editing_roles}{a string.  Which user roles are allowed to edit this page. Any combination of these roles is allowed (separated by commas).  Allowed values: teachers, students, members, public}

\item{published}{a boolean.    Whether the page is published (true) or draft state (false).}
}
\value{
empty
}
\description{
Create item in module
}
