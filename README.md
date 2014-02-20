# tumblR
tumblR is an R package which provides access to the Tumblr API. 
At the moment only the functions which look at public information are supported.

The following functions are supported:


##### General 

  * setup_tumblr_apikey
  * setup_tumblr_oauth

#####No Authentication

  * getAvatar

#####API Key 

Note that in order to call any of these functions
setup_tumblr_apikey **must** be called first. 

  * getLikes
  * getInfo
  * getPosts
  * getTagged

#####OAuth


## Authentication

To use this library (except to get avatars) you will require a Tumblr api key. 
These can be gotten from Tumblr. In order to get an api key (consumer key) 
you:

  1. Go to the [tumblr applications page](https://www.tumblr.com/oauth/apps) (Log in may be required)
  2. Click **+ Register application**
  3. Your OAuth Consumer Key is your API Key.

## Examples

```R
library(tumblR)
library(wordcloud)

setup_tumblr_apikey("MyAPIKey")

superBowlPosts <- getTagged("superbowl")
staffInfo <- getInfo("staff.tumblr.com")

all = getPosts("staff.tumblr.com")
posts = all$posts
corpus =  unlist(sapply(posts, function(x) x$tags))
wordcloud(corpus,colors=brewer.pal(8, "Dark2")) 
```

## Installation

This package can be installed directly from Github using the devtools package. 

```r
library(devtools)

install_github("tumblR",username="klapaukh")
```
