
#### eventlog ####

test_that("test ends on eventlog", {

  load("./testdata/patients.rda")

  ends <- patients %>%
    check_rule(ends("check-out"))

  expect_s3_class(ends, "eventlog")

  expect_equal(dim(ends), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(ends), c(colnames(patients), "ends_with_check_out"))

  # Only George Doe doesn't end with "check-out".
  expect_true(all(ends[ends$patient != "George Doe",]$ends_with_check_out))
  expect_equal(ends[ends$patient == "George Doe",]$ends_with_check_out, FALSE)
})

test_that("test ends on eventlog fails on non-existing activity", {

  load("./testdata/patients.rda")

  expect_error(
    ends <- patients %>%
      check_rule(ends("blood sample")),
    "*Activity blood sample not found in log*")
})

test_that("test ends on grouped_eventlog", {

  skip("grouped_log not working yet")

  load("./testdata/patients_grouped.rda")

  ends <- patients_grouped %>%
    check_rule(ends("check-out"))

  expect_s3_class(ends, "grouped_eventlog")

  expect_equal(dim(ends), c(nrow(patients_grouped), ncol(patients_grouped) + 1))
  expect_equal(colnames(ends), c(colnames(patients_grouped), "ends_with_check_out"))

  # Only George Doe doesn't end with "check-out".
  expect_true(all(ends[ends$patient != "George Doe",]$ends_with_check_out))
  expect_equal(ends[ends$patient == "George Doe",]$ends_with_check_out, FALSE)
})


#### activitylog ####

test_that("test ends on activitylog", {

  load("./testdata/patients_act.rda")

  ends <- patients_act %>%
    check_rule(ends("check-out"))

  expect_s3_class(ends, "activitylog")

  expect_equal(dim(ends), c(nrow(patients_act), ncol(patients_act) + 1))
  expect_true(compare::compareIgnoreOrder(colnames(ends), c(colnames(patients_act), "ends_with_check_out"))$result)

  # Only George Doe doesn't end with "check-out".
  expect_true(all(ends[ends$patient != "George Doe",]$ends_with_check_out))
  expect_equal(ends[ends$patient == "George Doe",]$ends_with_check_out, FALSE)
})

test_that("test ends on grouped_activitylog", {

  skip("grouped_log not working yet")

  load("./testdata/patients_act_grouped.rda")

  ends <- patients_act_grouped %>%
    check_rule(ends("check-out"))

  expect_s3_class(ends, "grouped_activitylog")

  expect_equal(dim(ends), c(nrow(patients_act_grouped), ncol(patients_act_grouped) + 1))
  expect_true(compare::compareIgnoreOrder(colnames(ends), c(colnames(patients_act_grouped), "ends_with_check_out"))$result)

  # Only George Doe doesn't end with "check-out".
  expect_true(all(ends[ends$patient != "George Doe",]$ends_with_check_out))
  expect_equal(ends[ends$patient == "George Doe",]$ends_with_check_out, FALSE)
})