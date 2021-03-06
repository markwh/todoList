% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/todolist.R
\docType{class}
\name{TodoItem-class}
\alias{TodoItem-class}
\alias{TodoItem}
\title{A Reference Class to represent a todo list item}
\description{
A Reference Class to represent a todo list item
}
\section{Fields}{

\describe{
\item{\code{itemText}}{Text of the todo list item}

\item{\code{timeCreated}}{Time item was created}

\item{\code{timeCompleted}}{Time item was completed}

\item{\code{isCompleted}}{Has the item been completed?}

\item{\code{itemID}}{Integer identifier of the todo item.}
}}

\section{Methods}{

\describe{
\item{\code{addComment(text, erase = FALSE)}}{Add a comment to an item}

\item{\code{as.data.frame()}}{Convert an item to a data.frame}

\item{\code{markComplete()}}{Mark an item as complete}

\item{\code{markIncomplete()}}{Mark a completed item incomplete.}

\item{\code{setStatus(newStatus = c("incomplete", "completed", "removed"))}}{set the status of a todo item}
}}

