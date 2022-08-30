
#### eventlog ####

test_that("test and on eventlog", {

  load("./testdata/patients.rda")

  and <- patients %>%
    filter(!(patient == "Jane Doe" & activity == "check-out")) %>%
    check_rule(and("check-in", "check-out"))

  expect_equal(dim(and), c(nrow(patients) - 1, ncol(patients) + 1))
  expect_equal(colnames(and), c(colnames(patients), "and_check_in_check_out"))

  # Jane Doe lacks "check-out", George Doe lacks both "check-in" and "check-out".
  expect_true(all(and[and$patient != "Jane Doe",]$and_check_in_check_out))
  expect_false(any(and[and$patient == "Jane Doe",]$and_check_in_check_out))
})

test_that("test and on eventlog fails on non-existing activity", {

  load("./testdata/patients.rda")

  load("./testdata/patients.rda")

  expect_error(
    and <- patients %>%
      check_rule(and("blood sample", "administer medication")),
    "*Activity blood sample not found in log*")

  expect_error(
    and <- patients %>%
      check_rule(and("check-in", "administer medication")),
    "*Activity administer medication not found in log*")
})

test_that("test and on grouped_eventlog", {

  skip("grouped_log not working yet")

  load("./testdata/patients_grouped.rda")
})

#### activitylog ####

test_that("test and on activitylog", {

  load("./testdata/patients_act.rda")

  and <- patients_act %>%
    filter(!(patient == "Jane Doe" & activity == "check-out")) %>%
    check_rule(and("check-in", "check-out"))

  expect_equal(dim(and), c(nrow(patients_act) - 1, ncol(patients_act) + 1))
  expect_equal(colnames(and), c(colnames(patients_act), "and_check_in_check_out"))

  # Jane Doe lacks "check-out", George Doe lacks both "check-in" and "check-out" (result = TRUE).
  expect_true(all(and[and$patient != "Jane Doe",]$and_check_in_check_out))
  expect_false(any(and[and$patient == "Jane Doe",]$and_check_in_check_out))
})

test_that("test and on grouped_activitylog", {

  skip("grouped_log not working yet")

  load("./testdata/patients_act_grouped.rda")
})