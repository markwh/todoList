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
