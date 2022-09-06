
#### eventlog ####

test_that("test check_rule on eventlog", {

  load("./testdata/patients.rda")

  check <- patients %>%
    check_rule(starts(activity = "check-in"))

  expect_s3_class(check, "eventlog")

  expect_equal(dim(check), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(check), c(colnames(patients), "starts_with_check_in"))

  # Only George Doe doesn't start with "check-in".
  expect_true(all(check[check$patient != "George Doe",]$starts_with_check_in))
  expect_equal(check[check$patient == "George Doe",]$starts_with_check_in, FALSE)
})

test_that("test check_rule on eventlog with arg label", {

  load("./testdata/patients.rda")

  check <- patients %>%
    check_rule(starts(activity = "check-in"), label = "rule")

  expect_s3_class(check, "eventlog")

  expect_equal(dim(check), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(check), c(colnames(patients), "rule"))

  # Only George Doe doesn't start with "check-in".
  expect_true(all(check[check$patient != "George Doe",]$rule))
  expect_equal(check[check$patient == "George Doe",]$rule, FALSE)
})

test_that("test check_rule on grouped_eventlog", {

  load("./testdata/patients_grouped_resource.rda")

  check <- patients_grouped_resource %>%
    check_rule(starts(activity = "check-in"))

  expect_s3_class(check, "grouped_eventlog")

  expect_equal(dim(check), c(nrow(patients_grouped_resource), ncol(patients_grouped_resource) + 1))
  expect_equal(colnames(check), c(colnames(patients_grouped_resource), "starts_with_check_in"))
  expect_equal(groups(check), groups(patients_grouped_resource))

  # Only George Doe doesn't start with "check-in".
  expect_true(all(check[check$patient != "George Doe",]$starts_with_check_in))
  expect_equal(check[check$patient == "George Doe",]$starts_with_check_in, FALSE)
})


#### activitylog ####

test_that("test check_rule on activitylog", {

  load("./testdata/patients_act.rda")

  check <- patients_act %>%
    check_rule(starts(activity = "check-in"))

  expect_s3_class(check, "activitylog")

  expect_equal(dim(check), c(nrow(patients_act), ncol(patients_act) + 1))
  expect_true(compare::compareIgnoreOrder(colnames(check), c(colnames(patients_act), "starts_with_check_in"))$result)

  # Only George Doe doesn't start with "check-in".
  expect_true(all(check[check$patient != "George Doe",]$starts_with_check_in))
  expect_equal(check[check$patient == "George Doe",]$starts_with_check_in, FALSE)
})

test_that("test check_rule on grouped_activitylog", {

  load("./testdata/patients_act_grouped_resource.rda")

  check <- patients_act_grouped_resource %>%
    check_rule(starts(activity = "check-in"))

  expect_s3_class(check, "grouped_activitylog")

  expect_equal(dim(check), c(nrow(patients_act_grouped_resource), ncol(patients_act_grouped_resource) + 1))
  expect_true(compare::compareIgnoreOrder(colnames(check), c(colnames(patients_act_grouped_resource), "starts_with_check_in"))$result)
  expect_equal(groups(check), groups(patients_act_grouped_resource))

  # Only George Doe doesn't start with "check-in".
  expect_true(all(check[check$patient != "George Doe",]$starts_with_check_in))
  expect_equal(check[check$patient == "George Doe",]$starts_with_check_in, FALSE)
})
