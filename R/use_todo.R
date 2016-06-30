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
      invisible(TRUE)
    }
  } else {
    file.create(".Rprofile")
  }

  # Make empty todo list
  # browser()
  # assign(name, value = todoList::TodoList$new())
  # do.call(sprintf("%s$write.csv", name), args = list(file = file))

  # add loading to .rprofile

  profstr <- sprintf('


  # todoList package
  if (requireNamespace("todoList", quietly = TRUE)) {
    if (file.exists("%s")) {
      %s <- todoList::TodoList$new(file = "%s")
    } else {
      %s <- todoList::TodoList$new()
    }
  }\n', file, name, file, name)

  writeLines(profstr, con = ".Rprofile")

  # add saving to .Last()

  lasttxt <- sprintf('

  if (requireNamespace("todoList", quietly = TRUE))
      %s$write.csv(file = "%s")\n', name, file)

  addToLast(text = lasttxt, fileloc = ".Rprofile")
}

# fileloc should be the location of the .RProfile file
addToLast <- function(text, fileloc) {

  if (!file.exists(fileloc))
    stop(sprintf("File not found: %s", fileloc))

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

  source(".Rprofile")
}
