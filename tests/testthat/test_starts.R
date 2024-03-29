
#### eventlog ####

test_that("test starts on eventlog", {

  load("./testdata/patients.rda")

  starts <- patients %>%
    check_rule(starts("check-in"))

  expect_s3_class(starts, "eventlog")

  expect_equal(dim(starts), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(starts), c(colnames(patients), "starts_with_check_in"))

  # Only George Doe doesn't start with "check-in".
  expect_true(all(starts[starts$patient != "George Doe",]$starts_with_check_in))
  expect_equal(starts[starts$patient == "George Doe",]$starts_with_check_in, FALSE)
})

test_that("test starts on eventlog fails on non-existing activity", {

  load("./testdata/patients.rda")

  expect_error(
    starts <- patients %>%
      check_rule(starts("blood sample")),
    "*Activity blood sample not found in log*")
})

test_that("test starts on grouped_eventlog", {

  load("./testdata/patients_grouped_resource.rda")

  starts <- patients_grouped_resource %>%
    check_rule(starts("check-in"))

  expect_s3_class(starts, "grouped_eventlog")

  expect_equal(dim(starts), c(nrow(patients_grouped_resource), ncol(patients_grouped_resource) + 1))
  expect_equal(colnames(starts), c(colnames(patients_grouped_resource), "starts_with_check_in"))
  expect_equal(groups(starts), groups(patients_grouped_resource))

  # Only George Doe doesn't start with "check-in".
  expect_true(all(starts[starts$patient != "George Doe",]$starts_with_check_in))
  expect_equal(starts[starts$patient == "George Doe",]$starts_with_check_in, FALSE)
})


#### activitylog ####

test_that("test starts on activitylog", {

  load("./testdata/patients_act.rda")

  starts <- patients_act %>%
    check_rule(starts("check-in"))

  expect_s3_class(starts, "activitylog")

  expect_equal(dim(starts), c(nrow(patients_act), ncol(patients_act) + 1))
  expect_true(compare::compareIgnoreOrder(colnames(starts), c(colnames(patients_act), "starts_with_check_in"))$result)

  # Only George Doe doesn't start with "check-in".
  expect_true(all(starts[starts$patient != "George Doe",]$starts_with_check_in))
  expect_equal(starts[starts$patient == "George Doe",]$starts_with_check_in, FALSE)
})

test_that("test starts on grouped_activitylog", {

  load("./testdata/patients_act_grouped_resource.rda")

  starts <- patients_act_grouped_resource %>%
    check_rule(starts("check-in"))

  expect_s3_class(starts, "grouped_activitylog")

  expect_equal(dim(starts), c(nrow(patients_act_grouped_resource), ncol(patients_act_grouped_resource) + 1))
  expect_true(compare::compareIgnoreOrder(colnames(starts), c(colnames(patients_act_grouped_resource), "starts_with_check_in"))$result)
  expect_equal(groups(starts), groups(patients_act_grouped_resource))

  # Only George Doe doesn't start with "check-in".
  expect_true(all(starts[starts$patient != "George Doe",]$starts_with_check_in))
  expect_equal(starts[starts$patient == "George Doe",]$starts_with_check_in, FALSE)
})