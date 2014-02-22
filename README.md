# tumblR
tumblR is an R package which provides access to the Tumblr API. 
At the moment only API calls which get information are supported.

The following functions are supported:


##### General 

  * setup_tumblr_apikey
  * setup_tumblr_oauth

#####No Authentication

  * get_avatar

#####API Key 

Note that in order to call any of these functions
setup_tumblr_apikey **must** be called first. 

  * get_likes
  * get_iterated_likes
  * get_info
  * get_posts
  * get_iterated_posts
  * get_tagged
  * get_iterated_tagged

#####OAuth
  
Note that in order to call these methods both setup_tumblr_apikey
and setup_tumblr_oauth **must** be called first. 

  * get_followers
  * get_iterated_followers
  * get_queued_posts
  * get_draft_posts
  * get_submission_posts
  * get_user_info
  * get_user_dashboard
  * get_iterated_user_dashboard
  * get_user_likes
  * get_iterated_user_likes
  * get_user_following
  * get_iterated_user_following
  

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

superBowlPosts <- get_tagged("superbowl")
staffInfo <- get_info("staff.tumblr.com")

all = get_posts("staff.tumblr.com")
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
