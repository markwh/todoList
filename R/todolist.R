
#' A Reference Class to represent a todo list item
#'
#' @field itemText Text of the todo list item
#' @field timeCreated Time item was created
#' @field timeCompleted Time item was completed
#' @field isCompleted Has the item been completed?
#' @field itemID Integer identifier of the todo item.
#' @export TodoItem
#' @exportClass TodoItem

TodoItem <- setRefClass("TodoItem",
                    fields = list(itemText = "character",
                                  timeCreated = "POSIXct",
                                  timeCompleted = "POSIXct",
                                  isCompleted = "logical",
                                  itemID = "integer",
                                  status = "factor",
                                  comment = "character"),
                    # contains = "list",
                    methods = list(
                      initialize = function(text, ID = NA_integer_,
                                  timeCreated = Sys.time(),
                                  timeCompleted = as.POSIXct(NA),
                                  isCompleted = FALSE,
                                  status = "incomplete",
                                  comment = "") {
                        itemText <<- text
                        timeCreated <<- timeCreated
                        timeCompleted <<- timeCompleted
                        isCompleted <<- isCompleted
                        status <<- factor(status,
                                          levels = c("incomplete",
                                                     "completed",
                                                     "removed"))
                        comment <<- ""
                        itemID <<- ID
                      },
                      show = function() {
                        "print a todo item"
                        cat("Text:      "); cat(methods::show(itemText))
                        cat("Created:   "); cat(methods::show(timeCreated))
                        if(isCompleted) {
                          cat("Completed: "); cat(methods::show(timeCompleted))
                        }
                        if(comment != "") {
                          cat("Comment:   "); cat(methods::show(comment))
                        }
                        cat("ID:        "); cat(methods::show(itemID))
                      },
                      setStatus = function(newStatus = c("incomplete", "completed", "removed")) {
                        "set the status of a todo item"
                        newStatus <- factor(newStatus,
                                         levels = c("incomplete",
                                                    "completed",
                                                    "removed"))
                        status <<- newStatus
                        if (newStatus == "completed")
                          .self$markComplete()
                        if (newStatus == "incomplete")
                          .self$markIncomplete()
                      },
                      addComment = function(text, erase = FALSE) {
                        "Add a comment to an item"
                        stopifnot(length(text) == 1)
                        text <- chartr(",", "_", text)
                        if (nchar(comment) == 0 || erase)
                          comment <<- text
                        else
                          comment <<- paste(comment, text, sep = "; ")
                      },
                      markComplete = function() {
                        "Mark an item as complete"
                        if (isCompleted)
                          stop(sprintf("Item already completed on %s", timeCompleted))
                        isCompleted <<- TRUE
                        timeCompleted <<- Sys.time()
                      },
                      markIncomplete = function() {
                        "Mark a completed item incomplete."
                        if (!isCompleted)
                          stop(sprintf("Item not marked as complete.", timeCompleted))
                        isCompleted <<- FALSE
                        timeCompleted <<- as.POSIXct(NA)
                      },
                      as.data.frame = function() {
                        "Convert an item to a data.frame"
                        out <- data.frame(itemText = itemText,
                                          timeCreated = timeCreated,
                                          status = status,
                                          timeCompleted = timeCompleted,
                                          isCompleted = isCompleted,
                                          itemID = itemID,
                                          comment = comment,
                                          stringsAsFactors = FALSE)
                        out
                      }
                    ))

#' Reference Class to represent todo list
#'
#' @field items List of TodoItems
#' @field nitems Number of items in list
#' @field File Associated csv file for reading and writing operations.
#' @field autowrite Logical, automatically write file upon modification? Defaults to TRUE.
#' @export TodoList
#' @exportClass TodoList

TodoList <- setRefClass("TodoList",
                    fields = list(items = "list",
                                  nitems = "integer",
                                  File = "character",
                                  autowrite = "logical"),
                    methods = list(
                      initialize = function(file = NULL, autowrite = TRUE) {
                        items <<- list()
                        nitems <<- 0L
                        File <<- NA_character_
                        autowrite <<- autowrite

                        if (!is.null(file)) {
                          .self$import.csv(file)
                          File <<- file
                        }
                      },
                      show = function(what = c("todo", "done", "removed", "all")) {
                        what = match.arg(what)

                        cmpltd <- vapply(items, `[[`, logical(1), "isCompleted")
                        rmvd <- vapply(items, function(x) x$status == "removed",
                                       logical(1))
                        # browser()
                        if (what == "todo")
                          toshow <- items[!cmpltd & !rmvd]
                        else if (what == "done")
                          toshow <- items[cmpltd & !rmvd]
                        else if (what == "removed")
                          toshow <- items[rmvd]
                        else if (what == "all")
                          toshow <- items[!rmvd]
                        cat("nitems:      "); cat(methods::show(nitems))
                        cat("items:     "); cat(methods::show(toshow))
                        },
                      add = function(text, write = autowrite) {
                        "Generate a new item to the todo list with the given text"
                        nitems <<- nitems + 1L
                        newItem <- TodoItem$new(text = text, ID = nitems)
                        items <<- stats::setNames(c(items, newItem),
                                           c(names(items), paste0("itm", nitems)))
                        if (write && file.exists(File))
                          .self$write.csv()
                      },
                      add_item = function(newItem, write = autowrite) {
                        "Add a TodoItem object to the todo list"
                        stopifnot()
                        nitems <<- nitems + 1L
                        items <<- stats::setNames(c(items, newItem),
                                           c(names(items), paste0("itm", nitems)))
                        if (write && file.exists(File))
                          .self$write.csv()
                      },
                      comment = function(ID, text, erase = FALSE, write = autowrite) {
                        "Add a comment to an item"
                        ID <- paste0("itm", ID)
                        items[[ID]]$addComment(text, erase = erase)
                        if (write && file.exists(File))
                          .self$write.csv()
                      },
                      done = function(ID, write = autowrite) {
                        ID <- paste0("itm", ID)
                        items[[ID]]$setStatus("completed")
                        if (write && file.exists(File))
                          .self$write.csv()
                      },
                      remove = function(ID, write = autowrite) {
                        ID <- paste0("itm", ID)
                        items[[ID]]$setStatus("removed")
                      },
                      to_df = function() {
                        lst <- list()
                        for (item in items) {
                          lst <- c(lst, list(item$as.data.frame()))
                        }
                        out <- dplyr::bind_rows(lst)
                        out
                      },
                      write.csv = function(file = NULL, setFile = FALSE) {
                        if (is.null(file)) {
                          if (is.na(File)) {
                            stop("file must be specified if none already associate with object")
                          } else {
                            file = File
                          }
                        }
                        if (setFile) {
                          File <<- file
                        }
                        out <- .self$to_df()
                        # print(out)
                        # browser()
                        utils::write.csv(x = out, file = file, row.names = FALSE)
                      },
                      import.csv = function(file) {
                        input <- try(utils::read.csv(file = file,
                                                     stringsAsFactors = FALSE),
                                     silent = TRUE)
                        if (is(input, "try-error"))
                          return()
                        for (i in 1:nrow(input)) {
                          itmlst <- input[i, ]
                          # browser()
                          if (is.na(itmlst$status))
                            itmlst$status <- ifelse(itmlst$isCompleted,
                                                    "completed", "incomplete")
                          newItem <- with(itmlst,
                                          TodoItem$new(text = itemText,
                                                   ID = itemID,
                                                   status = status,
                                                   timeCreated = as.POSIXct(timeCreated),
                                                   timeCompleted = as.POSIXct(timeCompleted),
                                                   isCompleted = isCompleted,
                                                   comment = comment))
                          .self$add_item(newItem = newItem, write = FALSE)
                        }
                      },
                      reread = function(){
                        ff <- File
                        aw <- autowrite
                        .self$initialize(file = ff,
                                         autowrite = aw)
                      }
                    ))
