\name{get_followers}
\alias{get_followers}
\title{Get the followers of a blog.}
\usage{
get_followers(blog, limit = 20, offset = 0)
}
\arguments{
  \item{blog}{The blog to get followers for}

  \item{limit}{Number of followers to get (1-20)}

  \item{offset}{Number of followers to skip from the start}
}
\description{
Get the followers of a blog. Only a maximum of 20 followers
can be returned in a single call. This requires OAuth and
seems to only word for the blog the OAuth is for.
}
\examples{
setup_tumblr_apikey(myApiKey)
setup_tumblr_oauth(myConsumerSecret)
get_followers(myBlog)
}
\seealso{
get_iterated_followers
}

