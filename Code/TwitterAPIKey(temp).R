# set the NEW credentials with OAuth 2.0
APP_NAME <- "Twitter_R_juhwan"
API_KEY <- "sVzjtbhCA9NltKMySKpCMHtKX"
API_KEY_SECRET <- "SaIvBjOYmnLWCHYDKkQeC0TMnjwtb5yHpycclQf8jwAbV9nuYL"
ACCESS_TOKEN <- "1373553626027761668-Gcp3JljPBB5CtokSXKoqIirqTERzML"
ACCESS_TOKEN_SECRET <- "Z72fdpY0493Zm7rNyuGSJlfh5yVbV1dg7rg8kZsIvCAS7"

#Bearer token <- "AAAAAAAAAAAAAAAAAAAAAIBfOAEAAAAA2%2B3j%2FAkTtEanURgbFvn0M%2B1vkVo%3D24vaHbVtv06TY1uRRoNFBIyZ0Z9lYBg8y2qHe7EuVhDJMGf4H5"
# connect to twitter app
twitter_token <- create_token(app = APP_NAME, consumer_key = API_KEY, consumer_secret = API_KEY_SECRET,
                              access_token = ACCESS_TOKEN, access_secret = ACCESS_TOKEN_SECRET)
#Access token and secret should be provided due to API change in July 2018.
rm(APP_NAME, API_KEY, API_KEY_SECRET, ACCESS_TOKEN, ACCESS_TOKEN_SECRET)

