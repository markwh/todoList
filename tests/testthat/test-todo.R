context("todo list")

test_that("item generation works", {

  itm1 <- TodoItem("Make it rain")
  expect_is(itm1, "TodoItem")
  expect_false(itm1$isCompleted)

  Sys.sleep(0.1)
  itm1$markComplete()
  expect_true(itm1$isCompleted)

  Sys.sleep(0.1)
  itm1$markIncomplete()
  expect_false(itm1$isCompleted)

  df1 <- itm1$as.data.frame()
  expect_is(df1, "data.frame")
  expect_equal(nrow(df1), 1)

})


test_that("TodoList generation works", {
  todo1 <- TodoList()
  expect_is(todo1, "TodoList")
  expect_equal(todo1$nitems, 0)

  todo1$add("Make it rain")
  expect_equal(todo1$nitems, 1)
  expect_equal(todo1$File, NA_character_)

  todo1$done(ID = 1)

  todo1$add("Get out of bed")
  expect_equal(todo1$nitems, 2)

  expect_error(todo1$write.csv(), regexp = "ile must be specified")
  fil1 <- tempfile()
  todo1$write.csv(fil1)

  todo2 <- TodoList(file = fil1)
  expect_equal(todo2$File, fil1)
  expect_silent(todo2$write.csv()) # should write to todo2$File

  # Automatic writing
  fil3 <- tempfile()
  fil4 <- tempfile()
  # fil5 <- tempfile()

  todo3 <- TodoList(file = fil1)
  todo3$write.csv(fil3, setFile = TRUE)
  expect_equal(todo3$File, fil3)

  todo4 <- TodoList(file = fil1, autowrite = FALSE)
  todo4$write.csv(fil4, setFile = TRUE)
  expect_equal(todo4$File, fil4)

  todo3$add("an item") # autowrites to associated file
  todo4$add("an item") # does not autowrite to associated file

  todo5 <- TodoList(file = fil3)
  todo6 <- TodoList(file = fil4)

  expect_false(todo3$nitems > todo5$nitems)
  expect_true(todo4$nitems > todo6$nitems)

  todo3$add("another item", write = FALSE)
  todo7 <- TodoList(file = fil3)
  expect_true(todo3$nitems > todo7$nitems)

  # expect_equivalent(todo1, todo2) # times not equivalent on account of rounding.


})
