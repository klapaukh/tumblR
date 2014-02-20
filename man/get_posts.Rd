\name{get_posts}
\alias{get_posts}
\title{Get posts posted by a blog}
\usage{
get_posts(blog, type, id, tag, limit, offset, reblog_info, notes_info, filter)
}
\arguments{
  \item{The}{type of post of get [ommit to get all]
  (text,quote,link,answer,video,audio,photo,chat)}

  \item{id}{The id of the post to get [ommit if not
  needed]}

  \item{tag}{Only get posts with specified tag [Ommit if
  not needed]}

  \item{limit}{Return up to limit many posts (1-20)}

  \item{offset}{First post to get [0 if ommitted]}

  \item{reblog_info}{Return reblog information [default:
  False] (True / False)}

  \item{notes_info}{Return \code{note count} and
  \code{note} information [default: False] (True / False)}

  \item{filter}{Post format to return [ommit for HTML]
  (text / raw). May return Markdown if raw is selected.}
}
\description{
Get the posts posted by a blog. This function requires an
API key. At most 20 posts can be returned at one time. If
more posts are required the function will need to be called
multiple times, or \code{\link{\get_iterated_posts}} can be
called.
}
\examples{
setup_tumblr_apikey("MyApiKey")
get_posts("staff.tumblr.com", limit=20,offset=0)
get_posts("staff.tumblr.com", limit=20,offset=20)
get_posts("staff.tumblr.com", id=20)
get_posts("staff.tumblr.com", filter="raw")
get_posts("staff.tumblr.com", tag="NYFW")
}
\seealso{
get_iterated_posts
}

