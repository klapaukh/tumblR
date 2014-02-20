\name{getLikes}
\alias{getLikes}
\title{Get a blogs likes}
\usage{
getLikes(blog, limit = 20, offset = 0)
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
getLikes("stuff.tumblr.com",limit=2,offset=0)
getLikes("stuff.tumblr.com",limit=2,offset=2)
}

