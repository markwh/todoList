
#' A Reference Class to represent a todo list item
#'
#' @field itemText Text of the todo list item
#' @field timeCreated Time item was created
#' @field timeCompleted Time item was completed
#' @field isCompleted Has the item been completed?
#' @field number Integer identifier of the todo item.
#' @export TodoItem
#' @exportClass TodoItem

TodoItem <- setRefClass("TodoItem",
    fields = list(itemText = "character",
                  timeCreated = "POSIXct",
                  timeCompleted = "POSIXct",
                  timeModified = "POSIXct",
                  isCompleted = "logical",
                  number = "integer",
                  status = "factor",
                  comment = "character"),
    # contains = "list",
    methods = list(
      initialize = function(text, number = NA_integer_,
                  timeCreated = Sys.time(),
                  timeCompleted = as.POSIXct(NA),
                  isCompleted = FALSE,
                  status = "incomplete",
                  comment = "") {
        itemText <<- text
        timeCreated <<- timeCreated
        timeModified <<- timeCreated
        timeCompleted <<- timeCompleted
        isCompleted <<- isCompleted
        status <<- factor(status,
                          levels = c("incomplete",
                                     "completed",
                                     "removed"))
        comment <<- ""
        number <<- number
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
        cat("Number:    "); cat(methods::show(number))
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
                          number = number,
                          comment = comment,
                          stringsAsFactors = FALSE)
        out
      }
    ))

#' Reference Class to represent todo list
#'
#' @field items List of TodoItems
#' @field nitems Number of items in list
#' @field syncloc Associated csv file or github repo for reading and writing operations.
#' @field autosync Logical, automatically sync file upon modification? Defaults to TRUE.
#' @export TodoList
#' @exportClass TodoList

TodoList <- setRefClass("TodoList",
    fields = list(items = "list",
                  nitems = "integer",
                  syncloc = "character",
                  ghurl = "character",
                  autosync = "logical"),
    methods = list(
      initialize = function(import = NULL, autosync = TRUE) {
        items <<- list()
        nitems <<- 0L
        autosync <<- autosync
        syncloc <<- NA_character_

        if (!is.null(import)) {
          if (file.exists(import)) {
            .self$import_csv(import)
          } else {
            .self$import_ghi(import)
            message(sprintf("This TodoList is now syncing with issues on the github \n
                            repo '%s'. Item numbering will be coerced to match issue\n
                            numbering.", import))
          }
          syncloc <<- import
        }
      },
      show = function(what = c("todo", "done", "removed", "all")) {
        what = match.arg(what)

        cmpltd <- vapply(items, function(x) x$status == "completed",
                         logical(1))
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
        shownums <- vapply(toshow, `[[`, numeric(1), "number")
        toshow <- toshow[order(shownums)]
        cat("nitems:      "); cat(methods::show(nitems))
        cat("items:     \n"); cat(methods::show(toshow))
        },
      add = function(text, sync = autosync) {
        "Generate a new item to the todo list with the given text"
        nitems <<- nitems + 1L
        newItem <- TodoItem$new(text = text, number = nitems)
        items <<- stats::setNames(c(items, newItem),
                           c(names(items), paste0("itm", newItem$number)))
        if (sync) {
          if (file.exists(syncloc)) {
            .self$write.csv()
          } else {
            .self$export_ghi()
          }
        }
      },
      add_item = function(newItem, sync = autosync) {
        "Add a TodoItem object to the todo list"
        stopifnot()
        nitems <<- nitems + 1L
        items <<- stats::setNames(c(items, newItem),
                           c(names(items), paste0("itm", newItem$number)))
        if (sync) {
          if (file.exists(syncloc)) {
            .self$write.csv()
          } else {
            .self$export_ghi()
          }
        }
      },
      get_item = function(number) {
        numbers <- vapply(items, `[[`, numeric(1), "number")
        out <- items[[match(number, numbers)]]
        if (!is(out, "TodoItem"))
          stop(sprintf("List item number %s not found.", number))
        out
      },
      comment = function(number, text, erase = FALSE, sync = autosync) {
        "Add a comment to an item"
        number <- paste0("itm", number)
        get_item(number)$addComment(text, erase = erase)
        if (sync && file.exists(syncloc))
          .self$write.csv()
      },
      done = function(number, sync = autosync) {
        get_item(number)$setStatus("completed")

        if (sync) {
          if (file.exists(syncloc)) {
            .self$write.csv()
          } else {
            .self$export_ghi()
          }
        }
      },
      remove = function(number, sync = autosync) {
        number <- paste0("itm", number)
        get_item(number)$setStatus("removed")
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
          if (is.na(syncloc)) {
            stop("file must be specified if none already associate with object")
          } else {
            file = syncloc
          }
        }
        if (setFile) {
          syncloc <<- file
        }
        out <- .self$to_df()
        # print(out)
        # browser()
        utils::write.csv(x = out, file = file, row.names = FALSE)
      },
      export_ghi = function(repo = NULL, setRepo = FALSE) {
        if (is.null(repo)) {
          if (is.na(syncloc)) {
            stop("repo must be specified if none already associate with object")
          } else {
            repo <- syncloc
          }
        }
        if (setRepo) {
          syncloc <<- repo
        }

        issues <- github::get.repository.issues(owner = own(repo),
                                                repo = rpo(repo))
        if (!issues$ok)
          stop("Github issue request failed.")
        titles <- vapply(issues$content, `[[`, character(1), "title")
        issnums <- vapply(issues$content, `[[`, integer(1), "number")

        # Rename duplicate-text items
        texts <- vapply(items, `[[`, character(1), "itemText")
        if (anyDuplicated(texts)) {
          dups <- which(duplicated(texts))
          texts <- make.unique(texts, sep = "--DUPLICATE_")
          for (i in 1:length(dups)) {
            items[[i]]$itemText <- texts[dups][i]
          }
        }

        # Retitle duplicate-titled issues
        if (anyDuplicated(titles)) {
          badones <- which(duplicated(titles))
          titles <- make.unique(titles, sep = "--DUPLICATE_")
          for (i in 1:length(badones)) {
            github::modify.issue(own(repo), rpo(repo), issnums[badones][i],
                        toJSON(list(title = unbox(titles[badones][i]))))
          }
        }

        # sync up with github
        topush <- setdiff(texts, titles)
        pushitems <- items[texts %in% topush]
        topull <- setdiff(titles, texts)
        pullissues <- issues$content[titles %in% topull]
        tosync <- intersect(titles, texts)
        syncitems <- items[match(tosync, items)]
        syncissues <- issues$content[match(tosync, issues)]

        # push from local
        for (i in 1:length(topush)) {
          github::create.issue(own(repo), rpo(repo),
                               content = item_ghi_json(pushitems[[i]]))
        }

        # pull from remote
        for (i in 1:length(topull)) {
          newItem <- with(pullissues[[i]],
            TodoItem$new(text = title,
               number = number,
               status = ifelse(state == "open", "incomplete", "completed"),
               timeCreated = as.POSIXct(created_at),
               timeCompleted = as.POSIXct(closedAt),
               isCompleted = (state == "closed"),
               comment = paste(unlist(comments), collapse = "\n")))
          .self$add_item(newItem = newItem, sync = FALSE)
        }

        # sync matching items
        for (i in 1:length(tosync)) {
          issi <- syncissues[[i]]
          itmi <- syncitems[[i]]
          if (as.POSIXct(issi$updated_at) > itmi$timeSynced)
            stop("Repo has upstream changes for issue")
        }


        # renumber locals

        for (i in 1:nitems) {
          if (texts[i] %in% titles) {
            issnum <- issnums[match(texts[i], titles)]
            oldnum
            github::modify.issue(own(repo), rpo(repo), issnum,
                                 content = item_ghi_json(items[[i]]))
          } else {
            github::create.issue(own(repo), rpo(repo),
                                 content = item_ghi_json(items[[i]]))
          }
        }

        # sync down from github


      },
      import_csv = function(file) {
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
                 number = number,
                 status = status,
                 timeCreated = as.POSIXct(timeCreated),
                 timeCompleted = as.POSIXct(timeCompleted),
                 isCompleted = isCompleted,
                 comment = comment))
          .self$add_item(newItem = newItem, sync = FALSE)
        }
      },
      import_ghi = function(repo) {
        issues <- github::get.repository.issues(owner = own(repo),
                                                repo = rpo(repo),
                                                state = "all")
        # browser()
        if (!issues$ok)
          stop("Github issue request failed.")

        for (i in 1:length(issues$content)) {

          closedAt <- ifelse(is.null(issues$content[[i]]$closed_at),
                             NA,
                             issues$content[[i]]$closed_at)
          newItem <- with(issues$content[[i]],
              TodoItem$new(text = title,
                 number = number,
                 status = ifelse(state == "open", "incomplete", "completed"),
                 timeCreated = as.POSIXct(created_at),
                 timeCompleted = as.POSIXct(closedAt),
                 isCompleted = (state == "closed"),
                 comment = paste(unlist(comments), collapse = "\n")))
          .self$add_item(newItem = newItem, sync = FALSE)
        }
      },
      reread = function(){
        ff <- syncloc
        aw <- autosync
        .self$initialize(syncloc = ff,
                         autosync = aw)
      }
    ))
