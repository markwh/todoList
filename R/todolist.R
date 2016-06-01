
#' A Reference Class to represent a todo list item
#'
#' @field itemText Text of the todo list item
#' @field timeCreated Time item was created
#' @field timeCompleted Time item was completed
#' @field isCompleted Has the item been completed?
#' @field itemID Integer identifier of the todo item.
#' @export

TodoItem <- setRefClass("TodoItem",
                    fields = list(itemText = "character",
                                  timeCreated = "POSIXct",
                                  timeCompleted = "POSIXct",
                                  isCompleted = "logical",
                                  itemID = "integer"),
                    # contains = "list",
                    methods = list(
                      initialize = function(text, ID = NA_integer_,
                                            timeCreated = Sys.time(),
                                            timeCompleted = as.POSIXct(NA),
                                            isCompleted = FALSE) {
                        itemText <<- text
                        timeCreated <<- timeCreated
                        timeCompleted <<- timeCompleted
                        isCompleted <<- isCompleted
                        itemID <<- ID
                      },
                      show = function() {
                        "print a todo item"
                        cat("Text:      "); cat(methods::show(itemText))
                        cat("Created:   "); cat(methods::show(timeCreated))
                        if(isCompleted) {
                          cat("Completed: "); cat(methods::show(timeCompleted))
                        }
                        cat("ID:        "); cat(methods::show(itemID))
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
                                          timeCompleted = timeCompleted,
                                          isCompleted = isCompleted,
                                          itemID = itemID,
                                          stringsAsFactors = FALSE)
                        print(out)
                      }
                    ))

#' Reference Class to represent todo list
#'
#' @field items List of TodoItems
#' @field nitems Number of items in list

TodoList <- setRefClass("TodoList",
                    fields = list(items = "list",
                                  nitems = "integer"),
                    methods = list(
                      initialize = function(file = NULL) {
                        items <<- list()
                        nitems <<- 0L

                        if (!is.null(file)) {
                          .self$import.csv(file)
                        }
                      },
                      add = function(text) {
                        "Generate a new item to the todo list with the given text"
                        nitems <<- nitems + 1L
                        newItem <- TodoItem$new(text = text, ID = nitems)
                        items <<- setNames(c(items, newItem),
                                           c(names(items), paste0("itm", nitems)))
                      },
                      add_item = function(newItem) {
                        "Add a TodoItem object to the todo list"
                        stopifnot()
                        nitems <<- nitems + 1L
                        items <<- setNames(c(items, newItem),
                                           c(names(items), paste0("itm", nitems)))
                      },
                      done = function(ID) {
                        ID <- paste0("itm", ID)
                        items[[ID]]$markComplete()
                      },
                      to_df = function() {
                        lst <- list()
                        # browser()
                        for (item in items) {
                          lst <- c(lst, list(item$as.data.frame()))
                        }
                        out <- dplyr::bind_rows(lst)
                        out
                      },
                      write.csv = function(file) {
                        out <- .self$to_df()
                        # print(out)
                        # browser()
                        utils::write.csv(x = out, file = file, row.names = FALSE)
                      },
                      import.csv = function(file) {
                        input <- utils::read.csv(file = file, stringsAsFactors = FALSE)
                        for (i in 1:nrow(input)) {
                          itmlst <- input[i, ]
                          newItem <- with(itmlst,
                                          TodoItem$new(text = itemText,
                                                   ID = itemID,
                                                   timeCreated = as.POSIXct(timeCreated),
                                                   timeCompleted = as.POSIXct(timeCompleted),
                                                   isCompleted = isCompleted))
                          .self$add_item(newItem = newItem)
                        }
                      }
                    ))
