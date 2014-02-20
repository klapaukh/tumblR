\name{get_iterated_posts}
\alias{get_iterated_posts}
\title{Get more than the 20 posts at once}
\usage{
get_iterated_posts(..., limit = 20, offset = 0)
}
\arguments{
  \item{limit}{The number of posts to return}

  \item{offset}{The first post to start at}

  \item{...}{extra options to pass to get_posts}
}
\description{
This function may make multiple requests to the tumblr api.
}
\examples{
setup_tumblr_apikey("MyApiKey")
get_iterated_posts("staff.tumblr.com", limit=200,offset=0)
}
\seealso{
get_posts
}

