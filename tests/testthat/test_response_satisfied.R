
test_that("test response_satisfied", {

  # response is satisfied: A is responded by B
  expect_true(response_satisfied(c("A", "B", "A", "B"), "A", "B", "A,B"))
  # response is satisfied: A is responded by B
  expect_true(response_satisfied(c("B", "A", "A", "B"), "A", "B", "A,B"))
  # response is satisfied: A is (eventually) responded by B
  expect_true(response_satisfied(c("B", "A", "A", "A", "C", "B"), "A", "B", "A,B"))
  # response is satisfied: no A present
  expect_true(response_satisfied(c("B", "B", "C", "B"), "A", "B", "A,B"))
  # response is not satisfied: last B not reponded by A
  expect_false(response_satisfied(c("B", "A", "B"), "B", "A", "B,A"))
  # response is not satisfied: last A not reponded by B
  expect_false(response_satisfied(c("A", "B", "A", "A", "A", "C"), "A", "B", "A,B"))
  # response is not satisfied: last B not reponded by A
  expect_false(response_satisfied(c("B", "C", "A", "A", "B", "C"), "B", "A", "B,A"))
})
