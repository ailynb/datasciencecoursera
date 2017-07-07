# ------------------------------------------------------------------------------
# CLEAN OBJECTS FROM WORKSPACE
# ------------------------------------------------------------------------------
rm(list=ls())

# ------------------------------------------------------------------------------
# LIBRARY
# ------------------------------------------------------------------------------
library(httr)

# ------------------------------------------------------------------------------
# WORKING DIRECTORY
# ------------------------------------------------------------------------------
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 3/")


# ------------------------------------------------------------------------------
# READING USING APPY
#    Register an application with the Github API here:
#           https://github.com/settings/applications.
#    Access the API to get information on your instructors repositories, hint: 
#    this is the url you want:
#           https://api.github.com/users/jtleek/repos
#    Use this data to find the time that the datasharing repo was created. 
# ------------------------------------------------------------------------------

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
github <- oauth_endpoints("github")


# 2. To make your own application, register at at
#    https://github.com/settings/applications. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below.
myapp <- oauth_app("github",
                   key = "26b69802c44991276fed",
                   secret = "b76543203e8d7a03ea9ccbda91679282aee64d23"
)


# 3. Get OAuth credentials
github_token <- oauth2.0_token(github, myapp)


# 4. Use API
gtoken <- config(token = github_token)
# OR: req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
req <- with_config(gtoken, GET("https://api.github.com/users/jtleek/repos"))
stop_for_status(req)
repo_list <- content(req)


# 5. Find the time that the datasharing repo was created
answer1 <- c() 
for (i in 1:length(repo_list)) {
    repo <- repo_list[[i]]
    if (repo$name == "datasharing") {
        answer1 = repo
        break
    }
}

# Expected output: The repository 'datasharing' was created at 2013-11-07T13:25:07Z
if (length(answer1) == 0) {
    msg("No such repository found: 'datasharing'")
} else {
    msg("The repository 'datasharing' was created at", answer1$created_at)
}