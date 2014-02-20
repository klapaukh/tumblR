\name{getInfo}
\alias{getInfo}
\title{Get info about a blog}
\usage{
getInfo(blog)
}
\arguments{
  \item{blog}{The blog url to get information about}
}
\description{
Gets info about a tumblr blog. This requires a Tumblr API
key. This function must be called after
\code{\link{setup_tumblr_api}}.
}
\examples{
setup_tumblr_apikey("MyApiKey")
getInfo("staff.tumblr.com")
}

