# Project profile script

# Load todo list

if (require(todoList)) {
  todo <- TodoList$new(file = "todo.csv")

  .Last <- function() {
    todo$write.csv(file = "todo.csv")
  }
}

