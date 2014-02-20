\name{get_avatar}
\alias{get_avatar}
\title{Get an blogs avatar}
\usage{
get_avatar(blog, size = 64)
}
\arguments{
  \item{blog}{The blog url}

  \item{size}{Size of the image to get (16, 24, 30, 40, 48
  ,64, 96, 128, 512)}
}
\description{
Get the avatar of a blog in a range of sizes. The allowed
sizes are 16, 24, 30, 40, 48 ,64, 96, 128, 512.
}
\examples{
get_avatar("staff.tumblr.com")
get_avatar("staff.tumblr.com",512)
}

