
#### eventlog ####

test_that("test contains_exactly on eventlog with arg n = 1", {

  load("./testdata/patients.rda")

  contains <- patients %>%
    check_rule(contains_exactly(activity = "check-in", n = 1))

  expect_equal(dim(contains), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(contains), c(colnames(patients), "contains_exactly_check_in_1"))

  # Only George Doe does not contain "check-in".
  expect_true(all(contains[contains$patient != "George Doe",]$contains_exactly_check_in_1))
  expect_equal(contains[contains$patient == "George Doe",]$contains_exactly_check_in_1, FALSE)
})

test_that("test contains_exactly on eventlog with arg n > 1", {

  load("./testdata/patients.rda")

  contains <- patients %>%
    check_rule(contains_exactly(activity = "treatment", n = 2))

  expect_equal(dim(contains), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(contains), c(colnames(patients), "contains_exactly_treatment_2"))

  # Only Jane Doe has twice "treatment".
  expect_true(all(contains[contains$patient == "Jane Doe",]$contains_exactly_treatment_2))
  expect_false(any(contains[contains$patient != "Jane Doe",]$contains_exactly_treatment_2))
})

test_that("test contains_exactly on eventlog fails on non-existing activity", {

  load("./testdata/patients.rda")

  expect_error(
    contains <- patients %>%
      check_rule(contains_exactly("blood sample", 1)),
    "*Activity blood sample not found in log*")
})

test_that("test contains_exactly on eventlog fails on arg n = 0", {

  load("./testdata/patients.rda")

  expect_error(
    contains <- patients %>%
      check_rule(contains_exactly("check-in", 0)),
    "*n should be greater than or equal to 1*")
})

test_that("test contains_exactly on grouped_eventlog", {

  skip("grouped_log not working yet")

  load("./testdata/patients_grouped.rda")

  contains <- patients_grouped %>%
    check_rule(contains_exactly("check-in", 1))
})

#### activitylog ####

test_that("test contains_exactly on activitylog with arg n = 1", {

  load("./testdata/patients_act.rda")

  contains <- patients_act %>%
    check_rule(contains_exactly(activity = "check-in", n = 1))

  expect_equal(dim(contains), c(nrow(patients_act), ncol(patients_act) + 1))
  expect_equal(colnames(contains), c(colnames(patients_act), "contains_exactly_check_in_1"))

  # Only George Doe does not contain "check-in".
  expect_true(all(contains[contains$patient != "George Doe",]$contains_exactly_check_in_1))
  expect_equal(contains[contains$patient == "George Doe",]$contains_exactly_check_in_1, FALSE)
})

test_that("test contains_exactly on activitylog with arg n > 1", {

  load("./testdata/patients_act.rda")

  contains <- patients_act %>%
    check_rule(contains_exactly(activity = "treatment", n = 2))

  expect_equal(dim(contains), c(nrow(patients_act), ncol(patients_act) + 1))
  expect_equal(colnames(contains), c(colnames(patients_act), "contains_exactly_treatment_2"))

  # Only Jane Doe has twice "treatment".
  expect_true(all(contains[contains$patient == "Jane Doe",]$contains_exactly_treatment_2))
  expect_false(any(contains[contains$patient != "Jane Doe",]$contains_exactly_treatment_2))
})

test_that("test contains_exactly on grouped_activitylog", {

  skip("grouped_log not working yet")

  load("./testdata/patients_act_grouped.rda")

  contains <- patients_act_grouped %>%
    check_rule(contains_exactly("check-in", 1))
})
