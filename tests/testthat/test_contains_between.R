
#### eventlog ####

test_that("test contains_between on eventlog with args min = 1, max = 1", {

  load("./testdata/patients.rda")

  contains <- patients %>%
    check_rule(contains_between(activity = "check-in", min = 1, max = 1))

  expect_s3_class(contains, "eventlog")

  expect_equal(dim(contains), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(contains), c(colnames(patients), "contains_between_check_in_1_1"))

  # Only George Doe does not contain "check-in".
  expect_true(all(contains[contains$patient != "George Doe",]$contains_between_check_in_1_1))
  expect_equal(contains[contains$patient == "George Doe",]$contains_between_check_in_1_1, FALSE)
})

test_that("test contains on eventlog with args min > 1, max > 1", {

  load("./testdata/patients.rda")

  contains <- patients %>%
    check_rule(contains_between(activity = "surgery", min = 2, max = 3))

  expect_s3_class(contains, "eventlog")

  expect_equal(dim(contains), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(contains), c(colnames(patients), "contains_between_surgery_2_3"))

  # Only John Doe has twice "surgery".
  expect_true(all(contains[contains$patient == "John Doe",]$contains_between_surgery_2_3))
  expect_false(any(contains[contains$patient != "John Doe",]$contains_between_surgery_2_3))
})

test_that("test contains on eventlog with args min = 0, max = 1", {

  load("./testdata/patients.rda")

  contains <- patients %>%
    check_rule(contains_between(activity = "check-in", min = 0, max = 1))

  expect_s3_class(contains, "eventlog")

  expect_equal(dim(contains), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(contains), c(colnames(patients), "contains_between_check_in_0_1"))

  # Every patients has at most once "check-in".
  expect_true(all(contains$contains_between_check_in_0_1))
})

test_that("test contains on eventlog with args min = 0, max = 0", {

  load("./testdata/patients.rda")

  contains <- patients %>%
    check_rule(contains_between(activity = "check-in", min = 0, max = 0))

  expect_s3_class(contains, "eventlog")

  expect_equal(dim(contains), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(contains), c(colnames(patients), "contains_between_check_in_0_0"))

  # Only George Doe absents "check-in".
  expect_false(any(contains[contains$patient != "George Doe",]$contains_between_check_in_0_0))
  expect_equal(contains[contains$patient == "George Doe",]$contains_between_check_in_0_0, TRUE)
})

test_that("test contains_between on eventlog fails on non-existing activity", {

  load("./testdata/patients.rda")

  expect_error(
    contains <- patients %>%
      check_rule(contains_between(activity = "blood sample", min = 1, max = 1)),
    "*Activity blood sample not found in log*")
})

test_that("test contains_between on eventlog fails on args min > max", {

  load("./testdata/patients.rda")

  expect_error(
    contains <- patients %>%
      check_rule(contains_between(activity = "check-in", min = 2, max = 1)),
    "*max should be greater or equal than min*")
})

test_that("test contains_between on grouped_eventlog", {

  skip("grouped_log not working yet")

  load("./testdata/patients_grouped.rda")

  contains <- patients_grouped %>%
    check_rule(contains_between(activity = "check-in", min = 1, max = 2))
})


#### activitylog ####

test_that("test contains_between on activitylog with args min = 1, max = 1", {

  load("./testdata/patients_act.rda")

  contains <- patients_act %>%
    check_rule(contains_between(activity = "check-in", min = 1, max = 1))

  expect_s3_class(contains, "activitylog")

  expect_equal(dim(contains), c(nrow(patients_act), ncol(patients_act) + 1))
  expect_equal(colnames(contains), c(colnames(patients_act), "contains_between_check_in_1_1"))

  # Only George Doe does not contain "check-in".
  expect_true(all(contains[contains$patient != "George Doe",]$contains_between_check_in_1_1))
  expect_equal(contains[contains$patient == "George Doe",]$contains_between_check_in_1_1, FALSE)
})

test_that("test contains on activitylog with args min > 1, max > 1", {

  load("./testdata/patients_act.rda")

  contains <- patients_act %>%
    check_rule(contains_between(activity = "surgery", min = 2, max = 3))

  expect_s3_class(contains, "activitylog")

  expect_equal(dim(contains), c(nrow(patients_act), ncol(patients_act) + 1))
  expect_equal(colnames(contains), c(colnames(patients_act), "contains_between_surgery_2_3"))

  # Only John Doe has twice "surgery".
  expect_true(all(contains[contains$patient == "John Doe",]$contains_between_surgery_2_3))
  expect_false(any(contains[contains$patient != "John Doe",]$contains_between_surgery_2_3))
})