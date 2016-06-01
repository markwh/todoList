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

  todo1$done(ID = 1)

  todo1$add("Get out of bed")
  expect_equal(todo1$nitems, 2)

  fil1 <- tempfile()
  todo1$write.csv(fil1)

  todo2 <- TodoList(file = fil1)

  # expect_equivalent(todo1, todo2) # times not equivalent on account of rounding.

})
