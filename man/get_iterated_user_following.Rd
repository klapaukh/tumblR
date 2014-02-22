\name{get_iterated_user_following}
\alias{get_iterated_user_following}
\title{Get blogs a user is following}
\usage{
get_iterated_user_following(limit = 20, offset = 0)
}
\arguments{
  \item{limit}{Number of posts to return}

  \item{offset}{Blog to start at}
}
\description{
Get the blogs a user if following. No limit on the number
of blogs you can request. Requires OAuth. May make multiple
requests to the API
}
\seealso{
get_user_following
}

