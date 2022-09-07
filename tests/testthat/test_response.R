
#### eventlog ####

test_that("test response on eventlog", {

  load("./testdata/patients.rda")

  res <- patients %>%
    check_rule(response(activity_a = "treatment", activity_b = "surgery"))

  expect_s3_class(res, "eventlog")

  expect_equal(dim(res), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(res), c(colnames(patients), "response_treatment_surgery"))

  # Jane Doe's "treatment" is not (eventually) followed by "surgery".
  # George Doe lacks both "treatment" and "surgery", so rule is satisfied.
  expect_true(all(res[res$patient != "Jane Doe",]$response_treatment_surgery))
  expect_false(any(res[res$patient == "Jane Doe",]$response_treatment_surgery))
})

test_that("test response on eventlog when activity_a is not present", {

  load("./testdata/patients.rda")

  res <- patients %>%
    check_rule(response(activity_a = "register", activity_b = "check-out"))

  expect_s3_class(res, "eventlog")

  expect_equal(dim(res), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(res), c(colnames(patients), "response_register_check_out"))

  # George Doe lacks "check-out".
  # John and Jane Doe lack "register", so rule is satisfied.
  expect_true(all(res[res$patient != "George Doe",]$response_register_check_out))
  expect_false(any(res[res$patient == "George Doe",]$response_register_check_out))
})

test_that("test response on eventlog fails on non-existing activity", {

  load("./testdata/patients.rda")

  expect_error(
    res <- patients %>%
      check_rule(response("blood sample", "administer medication")),
    "*Activity blood sample not found in log*")

  expect_error(
    res <- patients %>%
      check_rule(response("check-in", "administer medication")),
    "*Activity administer medication not found in log*")
})

test_that("test response on grouped_eventlog", {

  load("./testdata/patients_grouped_resource.rda")

  res <- patients_grouped_resource %>%
    check_rule(response(activity_a = "treatment", activity_b = "surgery"))

  expect_s3_class(res, "grouped_eventlog")

  expect_equal(dim(res), c(nrow(patients_grouped_resource), ncol(patients_grouped_resource) + 1))
  expect_equal(colnames(res), c(colnames(patients_grouped_resource), "response_treatment_surgery"))
  expect_equal(groups(res), groups(patients_grouped_resource))

  # Jane Doe's "treatment" is not (eventually) followed by "surgery".
  # George Doe lacks both "treatment" and "surgery", so rule is satisfied.
  expect_true(all(res[res$patient != "Jane Doe",]$response_treatment_surgery))
  expect_false(any(res[res$patient == "Jane Doe",]$response_treatment_surgery))
})


#### activitylog ####

test_that("test response on activitylog", {

  load("./testdata/patients_act.rda")

  res <- patients_act %>%
    check_rule(response(activity_a = "treatment", activity_b = "surgery"))

  expect_s3_class(res, "activitylog")

  expect_equal(dim(res), c(nrow(patients_act), ncol(patients_act) + 1))
  expect_equal(colnames(res), c(colnames(patients_act), "response_treatment_surgery"))

  # Jane Doe's "treatment" is not (eventually) followed by "surgery".
  # George Doe lacks both "treatment" and "surgery", so rule is satisfied.
  expect_true(all(res[res$patient != "Jane Doe",]$response_treatment_surgery))
  expect_false(any(res[res$patient == "Jane Doe",]$response_treatment_surgery))
})

test_that("test response on grouped_activitylog", {

  load("./testdata/patients_act_grouped_resource.rda")

  res <- patients_act_grouped_resource %>%
    check_rule(response(activity_a = "treatment", activity_b = "surgery"))

  expect_s3_class(res, "grouped_activitylog")

  expect_equal(dim(res), c(nrow(patients_act_grouped_resource), ncol(patients_act_grouped_resource) + 1))
  expect_equal(colnames(res), c(colnames(patients_act_grouped_resource), "response_treatment_surgery"))
  expect_equal(groups(res), groups(patients_act_grouped_resource))

  # Jane Doe's "treatment" is not (eventually) followed by "surgery".
  # George Doe lacks both "treatment" and "surgery", so rule is satisfied.
  expect_true(all(res[res$patient != "Jane Doe",]$response_treatment_surgery))
  expect_false(any(res[res$patient == "Jane Doe",]$response_treatment_surgery))
})