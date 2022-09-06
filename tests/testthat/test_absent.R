
#### eventlog ####

test_that("test absent on eventlog with arg n = 0", {

  load("./testdata/patients.rda")

  absent <- patients %>%
    check_rule(absent("check-in", 0))

  expect_s3_class(absent, "eventlog")

  expect_equal(dim(absent), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(absent), c(colnames(patients), "absent_check_in_0"))

  # Only George Doe absents "check-in".
  expect_true(all(absent[absent$patient != "George Doe",]$absent_check_in_0))
  expect_equal(absent[absent$patient == "George Doe",]$absent_check_in_0, FALSE)
})

test_that("test absent on eventlog with arg n > 1", {

  load("./testdata/patients.rda")

  absent <- patients %>%
    check_rule(absent(activity = "surgery", n = 2))

  expect_s3_class(absent, "eventlog")

  expect_equal(dim(absent), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(absent), c(colnames(patients), "absent_surgery_2"))

  # Only John Doe has twice "surgery".
  expect_true(all(absent[absent$patient == "John Doe",]$absent_surgery_2))
  expect_false(any(absent[absent$patient != "John Doe",]$absent_surgery_2))
})

test_that("test absent on eventlog fails on non-existing activity", {

  load("./testdata/patients.rda")

  expect_error(
    absent <- patients %>%
      check_rule(absent("blood sample")),
    "*Activity blood sample not found in log*")
})

test_that("test absent on grouped_eventlog", {

  load("./testdata/patients_grouped_resource.rda")

  absent <- patients_grouped_resource %>%
    check_rule(absent(activity = "check-in", n = 0))

  expect_s3_class(absent, "grouped_eventlog")

  expect_equal(dim(absent), c(nrow(patients_grouped_resource), ncol(patients_grouped_resource) + 1))
  expect_equal(colnames(absent), c(colnames(patients_grouped_resource), "absent_check_in_0"))
  expect_equal(groups(absent), groups(patients_grouped_resource))

  # Only George Doe absents "check-in".
  expect_true(all(absent[absent$patient != "George Doe",]$absent_check_in_0))
  expect_equal(absent[absent$patient == "George Doe",]$absent_check_in_0, FALSE)
})


#### activitylog ####

test_that("test absent on activitylog with arg n = 0", {

  load("./testdata/patients_act.rda")

  absent <- patients_act %>%
    check_rule(absent("check-in", 0))

  expect_s3_class(absent, "activitylog")

  expect_equal(dim(absent), c(nrow(patients_act), ncol(patients_act) + 1))
  expect_equal(colnames(absent), c(colnames(patients_act), "absent_check_in_0"))

  # Only George Doe absents "check-in".
  expect_true(all(absent[absent$patient != "George Doe",]$absent_check_in_0))
  expect_equal(absent[absent$patient == "George Doe",]$absent_check_in_0, FALSE)
})

test_that("test absent on activitylog with arg n > 1", {

  load("./testdata/patients_act.rda")

  absent <- patients_act %>%
    check_rule(absent(activity = "surgery", n = 2))

  expect_s3_class(absent, "activitylog")

  expect_equal(dim(absent), c(nrow(patients_act), ncol(patients_act) + 1))
  expect_equal(colnames(absent), c(colnames(patients_act), "absent_surgery_2"))

  # Only John Doe has twice "surgery".
  expect_true(all(absent[absent$patient == "John Doe",]$absent_surgery_2))
  expect_false(any(absent[absent$patient != "John Doe",]$absent_surgery_2))
})

test_that("test absent on grouped_activitylog", {

  load("./testdata/patients_act_grouped_resource.rda")

  absent <- patients_act_grouped_resource %>%
    check_rule(absent(activity = "check-in", n = 0))

  expect_s3_class(absent, "grouped_activitylog")

  expect_equal(dim(absent), c(nrow(patients_act_grouped_resource), ncol(patients_act_grouped_resource) + 1))
  expect_equal(colnames(absent), c(colnames(patients_act_grouped_resource), "absent_check_in_0"))
  expect_equal(groups(absent), groups(patients_act_grouped_resource))

  # Only George Doe absents "check-in".
  expect_true(all(absent[absent$patient != "George Doe",]$absent_check_in_0))
  expect_equal(absent[absent$patient == "George Doe",]$absent_check_in_0, FALSE)
})
