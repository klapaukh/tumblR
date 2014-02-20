\name{get_likes}
\alias{get_likes}
\title{Get a blogs likes}
\usage{
get_likes(blog, limit = 20, offset = 0)
}
\arguments{
  \item{blog}{The blog url}

  \item{limit}{Number of likes to return (1-20)}

  \item{Number}{of first liked post to start at}
}
\description{
Get up to 20 of a blogs likes. In order to get more than
20, the function must be called multiple times. This
function requires an API key.
}
\examples{
setup_tumblr_apikey("MyApiKey")
get_likes("stuff.tumblr.com",limit=2,offset=0)
get_likes("stuff.tumblr.com",limit=2,offset=2)
}
\seealso{
get_iterated_likes
}

