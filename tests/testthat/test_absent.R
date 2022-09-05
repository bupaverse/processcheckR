
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

  skip("grouped_log not working yet")

  load("./testdata/patients_grouped.rda")

  absent <- patients_grouped %>%
    check_rule(absent("check-in"))
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

test_that("test absent on activitylog with arg n > 2", {

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

  skip("grouped_log not working yet")

  load("./testdata/patients_act_grouped.rda")

  absent <- patients_act_grouped %>%
    check_rule(absent("check-in"))
})
