# Github API integration

#' Get user and repo name for a specified remote for the current working directory
#'
#' @param remoteName Name of the git remote. Default is "origin".
gitRemote <- function(remoteName = "origin") {
  rname <- system2("git", sprintf("remote show -n %s", remoteName),
                   stdout = TRUE)
  furl <- grep("Fetch.+github\\.com/.+\\.git$", rname, value = TRUE)

  if (length(furl) == 0)
    stop(sprintf("github url not found for remote '%s'", remoteName))

  furl2 <- gsub("^.+github\\.com/", "", furl)
  furl3 <- gsub("\\.git$", "", furl2)
  user_rep <- strsplit(furl3, "/", fixed = TRUE)[[1]]

  if (!length(user_rep) == 2)
    stop(sprintf("Invalid url for remote '%s'", remoteName))

  out <- list(user = user_rep[1], repo = user_rep[2])
  out
}

#' Create a json string for a github issue from a TodoItem.
#'
#' @param context A \code{ghcontext} object, as returned by github::create.github.context
#' @param title Title of issue.
#' @param body Body of issue
#' @param assignees List of assignees.
#' @param labels Labels to associate with this issue.
#' @param milestone (optional) Number of the milestone to associate this issue with.
#'
#' @importFrom jsonlite unbox toJSON
#' @export
item_ghi_json <- function(item) {
  itmstate <- ifelse(item$status == "incomplete", "open", "closed")
  isslist <- list(title = unbox(item$itemText),
                  state = unbox(itmstate),
                  labels = "todo")
  issue_json <- jsonlite::toJSON(isslist)

  issue_json
}

#' Create a json string for a github issue, as expected by github API.
#'
#' @param context A \code{ghcontext} object, as returned by github::create.github.context
#' @param title Title of issue.
#' @param body Body of issue
#' @param assignees List of assignees.
#' @param labels Labels to associate with this issue.
#' @param milestone (optional) Number of the milestone to associate this issue with.
#'
#' @importFrom jsonlite unbox toJSON
#' @export
ghi_json <- function(context, title) {
  issue_json <- jsonlite::toJSON(list(title = unbox(title)))

  issue_json
}

#' Get github info about yourself, for passing as \code{assignees} argument.
#'
#'
self_assign <- function(context) {
  fields <- c("login", "id", "avatar_url", "gravatar_id", "url",
              "html_url", "followers_url", "following_url", "gists_url",
              "starred_url", "subscriptions_url", "organizations_url",
              "repos_url", "events_url", "received_events_url", "type",
              "site_admin")

  out <- lapply(context$user[fields], unbox)
}

own <- function(repo)
  strsplit(repo, "/")[[1]][1]

rpo <- function(repo)
  strsplit(repo, "/")[[1]][2]

#' @param repo Repository address in "owner/repository" format.
#' @importFrom github modify.issue
mod_issue <- function(repo, todo_item) {
  repspl <- strsplit(repo, "/")[[1]]
  modify.issue(owner = repspl[1], repo = repspl[2], )
}


