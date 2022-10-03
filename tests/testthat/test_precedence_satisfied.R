
test_that("test precedence_satisfied", {

  # precedence is satisfied: B is preceded by A
  expect_true(precedence_satisfied(c("A", "B", "A", "B"), "A", "B", "A,B"))
  # precedence is satisfied: B is preceded by A
  expect_true(precedence_satisfied(c("A", "A", "B"), "A", "B", "A,B"))
  # precedence is satisfied: B is preceded by A
  expect_true(precedence_satisfied(c("A", "B", "B"), "A", "B", "A,B"))
  # precedence is satisfied: B is (eventually) preceded by A
  expect_true(precedence_satisfied(c("A", "A", "A", "C", "B"), "A", "B", "A,B"))
  # precedence is satisfied: B is (eventually) preceded by A
  expect_true(precedence_satisfied(c("A", "C", "B", "B", "A"), "A", "B", "A,B"))
  # precedence is satisfied: B is preceded by A
  expect_true(precedence_satisfied(c("C", "A", "A", "B", "C", "B"), "A", "B", "A,B"))
  # precedence is satisfied: no B present
  expect_true(precedence_satisfied(c("A", "C", "A", "A"), "A", "B", "A,B"))
  # precedence is not satisfied: first B not preceded by A
  expect_false(precedence_satisfied(c("B", "A", "B"), "A", "B", "A,B"))
  # precedence is not satisfied: first B not preceded by A
  expect_false(precedence_satisfied(c("B", "B", "A", "A", "A", "C"), "A", "B", "A,B"))
})
