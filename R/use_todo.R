# use_todo.R
# Inspired by functions like devtools::use_testthat.

#' Get going with TodoList package for your project.
#'
#' @export

use_todo <- function(name = "todo", file = "./todo.csv") {

  if(length(list.files(pattern = "\\.Rproj$")) == 0)
    stop("Must be called from root directory of an RStudio project.")

  if (".rprofile" %in% tolower(list.files(all.files = TRUE))) {
    if (!file.exists(".Rprofile"))
      stop(".Rprofile capitalization is off. Should be '.Rprofile'")
    txt1 <- readLines(".Rprofile")
    if (length(grep("todoList", txt1)) > 0) {
      message("It appears todoList is already integrated to this project")
      return(TRUE)
    }
  }

  # add loading to .rprofile

  profstr <- sprintf('


  # todoList package
  if (require(todoList)) {
  %s <- TodoList$new(file = "%s")

  PLACEHOLDER_FOR_LAST

  }', name, file)

  write(profstr, file = ".Rprofile", append = TRUE)

  # add saving to .Last()
  if (length(grep("\\.Last", txt1)) > 0) {

    laststr <- sprintf('

    .Last <- function() {
      %s$write.csv(file = "%s")
    }', name, file)

  }
}

# fileloc should be the location of the .RProfile file
addToLast <- function(text, fileloc) {

  # test if last is present in .profile
  txt1 <- readLines(fileloc)

  lastlog <- grepl("^\\s*\\.Last\\s*<-", txt1) |
    grepl("^\\s*\\.Last\\s*=", txt1)
  lastloc <- which(lastlog)

  if (length(lastloc) > 1)
    stop("Multiple .Last assignments appear to be present in .Rprofile.")

  if (length(lastloc) == 1) {
    # Insert text below last line
    pretext <- txt1[1:(lastloc)]
    posttext <- txt1[(lastloc + 1) : length(txt1)]

    outtext <- c(pretext, text, posttext)

  } else {
    outtext <- sprintf('

    .Last <- function() {
      %s
    }', text)
    outtext <- c(txt1, outtext, "")
  }

  writeLines(outtext, fileloc)

}
