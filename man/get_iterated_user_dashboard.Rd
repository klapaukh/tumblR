\name{get_iterated_user_dashboard}
\alias{get_iterated_user_dashboard}
\title{Get a users dashboard}
\usage{
get_iterated_user_dashboard(..., limit = 20, offset = 0)
}
\arguments{
  \item{limit}{Number of posts to be}

  \item{offset}{Post to start at}
}
\description{
Get a users dashboard for any number of posts. Requires
OAuth. May make multiple API requests. Extra parameters
passed to \code{\link{get_user_dashboard}}.
}
\seealso{
get_user_dashboard
}

