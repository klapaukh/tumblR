\name{get_iterated_followers}
\alias{get_iterated_followers}
\title{Get the followers of a blog}
\usage{
get_iterated_followers(blog, limit = 20, offset = 0)
}
\arguments{
  \item{blog}{The blog to get followers for.}

  \item{limit}{Number of followers to get}

  \item{offset}{Number of followers to skip from the start}
}
\description{
Get the followers of a blog, but without an upper limit on
the number of followers that can be retrieved. This may
result in multiple calls to the tumblr API. This requires
OAuth and seems to only work for the authenticated blog.
The total number of followers for blog is not returned in
the list but rather as the "total_users" attribute on the
list.
}
\examples{
setup_tumblr_apikey(myApiKey)
setup_tumblr_oauth(myConsumerSecret)
get_iterated_followers(myBlog,limit=100)
}
\seealso{
get_followers
}

