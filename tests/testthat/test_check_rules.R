
#### eventlog ####

test_that("test check_rules on eventlog with multiple rules", {

  load("./testdata/patients.rda")

  check <- patients %>%
    check_rules(start = starts(activity = "check-in"),
                end = ends(activity = "check-out"))

  expect_s3_class(check, "eventlog")

  expect_equal(dim(check), c(nrow(patients), ncol(patients) + 2))
  expect_equal(colnames(check), c(colnames(patients), "start", "end"))

  # Only George Doe doesn't start with "check-in" and end with "check-out".
  expect_true(all(check[check$patient != "George Doe",]$start))
  expect_true(all(check[check$patient != "George Doe",]$end))
  expect_equal(check[check$patient == "George Doe",]$start, FALSE)
  expect_equal(check[check$patient == "George Doe",]$end, FALSE)
})

test_that("test check_rules on eventlog with 1 rules", {

  load("./testdata/patients.rda")

  check <- patients %>%
    check_rules(start = starts(activity = "check-in"))

  expect_s3_class(check, "eventlog")

  expect_equal(dim(check), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(check), c(colnames(patients), "start"))

  # Only George Doe doesn't start with "check-in".
  expect_true(all(check[check$patient != "George Doe",]$start))
  expect_equal(check[check$patient == "George Doe",]$start, FALSE)
})

test_that("test check_rules on grouped_eventlog", {

  skip("grouped_log not working yet")

  load("./testdata/patients_grouped.rda")

})


#### activitylog ####

test_that("test check_rules on activitylog with multiple rules", {

  load("./testdata/patients_act.rda")

  check <- patients_act %>%
    check_rules(start_check_in = starts(activity = "check-in"),
                end_check_out = ends(activity = "check-out"))

  expect_s3_class(check, "activitylog")

  expect_equal(dim(check), c(nrow(patients_act), ncol(patients_act) + 2))
  expect_true(compare::compareIgnoreOrder(colnames(check), c(colnames(patients_act), "start_check_in", "end_check_out"))$result)

  # Only George Doe doesn't start with "check-in" and end with "check-out".
  expect_true(all(check[check$patient != "George Doe",]$start_check_in))
  expect_true(all(check[check$patient != "George Doe",]$end_check_out))
  expect_equal(check[check$patient == "George Doe",]$start_check_in, FALSE)
  expect_equal(check[check$patient == "George Doe",]$end_check_out, FALSE)
})

test_that("test check_rules on grouped_activitylog", {

  skip("grouped_log not working yet")

  load("./testdata/patients_act_grouped.rda")

})
