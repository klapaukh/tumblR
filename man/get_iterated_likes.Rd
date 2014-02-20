\name{get_iterated_likes}
\alias{get_iterated_likes}
\title{Get more than 20 likes in one call}
\usage{
get_iterated_likes(blog, limit = 20, offset = 0)
}
\arguments{
  \item{blog}{The blog url}

  \item{limit}{Number of likes to return (1-20)}

  \item{Number}{of first liked post to start at}
}
\description{
This function will return any number of likes. In doing so
it may more multiple requests to the tumblr api. This
function requires an API key. Unlike get_likes which
returns a list of liked posts and a count, this function
returns the count of total number of liked posts as the
attribute liked_count.
}
\examples{
setup_tumblr_apikey("MyApiKey")
get_iterated_likes("stuff.tumblr.com",limit=100,offset=0)
}
\seealso{
get_likes
}

