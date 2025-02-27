% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/groups.R
\name{create_group_category}
\alias{create_group_category}
\title{Create a group category}
\usage{
create_group_category(
  context_id,
  context_type = "courses",
  cat_name,
  self_signup = NULL,
  auto_leader = NULL,
  group_limit = NULL,
  create_group_count = NULL
)
}
\arguments{
\item{cat_name}{Name of the group category. Required.}

\item{self_signup}{Allow students to sign up for a group themselves (Course Only). valid values are: “enabled”, allows students to self sign up for any group in course;  “restricted” allows students to self sign up only for groups in the same section null disallows self sign up}

\item{auto_leader}{Assigns group leaders automatically when generating and allocating students to groups. Valid values are: “first” the first student to be allocated to a group is the leader; “random” a random student from all members is chosen as the leader}

\item{group_limit}{Limit the maximum number of users in each group (Course Only). Requires self signup.}

\item{create_group_count}{Create this number of groups (Course Only).}
}
\description{
Does not work yet. Returns 422. Unclear how to fix.
}
\examples{
create_group_category(1350207, "courses", "FinalProjectGroup",
"enabled", "first", 3, 48)
}
