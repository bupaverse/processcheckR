
#### eventlog ####

test_that("test precedence on eventlog", {

  load("./testdata/patients.rda")

  pre <- patients %>%
    check_rule(precedence(activity_a = "surgery", activity_b = "treatment"))

  expect_s3_class(pre, "eventlog")

  expect_equal(dim(pre), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(pre), c(colnames(patients), "precedence_surgery_treatment"))

  # All "treatment"s were preceded by "surgery".
  # George Doe lacks "treatment", so rule is satisfied.
  expect_true(all(pre$precedence_surgery_treatment))
})

test_that("test precedence on eventlog", {

  load("./testdata/patients.rda")

  pre <- patients %>%
    filter(!(patient == "Jane Doe" & activity == "surgery")) %>%
    check_rule(precedence(activity_a = "surgery", activity_b = "treatment"))

  expect_s3_class(pre, "eventlog")

  expect_equal(dim(pre), c(nrow(patients) - 2, ncol(patients) + 1))
  expect_equal(colnames(pre), c(colnames(patients), "precedence_surgery_treatment"))

  # Jane Doe's "treatment" was not preceded by "surgery".
  # George Doe lacks "treatment", so rule is satisfied.
  expect_true(all(pre[pre$patient != "Jane Doe",]$precedence_surgery_treatment))
  expect_false(any(pre[pre$patient == "Jane Doe",]$precedence_surgery_treatment))
})

test_that("test precedence on eventlog fails on non-existing activity", {

  load("./testdata/patients.rda")

  expect_error(
    pre <- patients %>%
      check_rule(precedence("blood sample", "administer medication")),
    "*Activity blood sample not found in log*")

  expect_error(
    pre <- patients %>%
      check_rule(precedence("check-in", "administer medication")),
    "*Activity administer medication not found in log*")
})

test_that("test precedence on grouped_eventlog", {

  load("./testdata/patients_grouped_resource.rda")

  pre <- patients_grouped_resource %>%
    check_rule(precedence(activity_a = "surgery", activity_b = "treatment"))

  expect_s3_class(pre, "grouped_eventlog")

  expect_equal(dim(pre), c(nrow(patients_grouped_resource), ncol(patients_grouped_resource) + 1))
  expect_equal(colnames(pre), c(colnames(patients_grouped_resource), "precedence_surgery_treatment"))
  expect_equal(groups(pre), groups(patients_grouped_resource))

  # All "treatment"s were preceded by "surgery".
  # George Doe lacks "treatment", so rule is satisfied.
  expect_true(all(pre$precedence_surgery_treatment))
})


#### activitylog ####

test_that("test precedence on activitylog", {

  load("./testdata/patients_act.rda")

  pre <- patients_act %>%
    check_rule(precedence(activity_a = "surgery", activity_b = "treatment"))

  expect_s3_class(pre, "activitylog")

  expect_equal(dim(pre), c(nrow(patients_act), ncol(patients_act) + 1))
  expect_equal(colnames(pre), c(colnames(patients_act), "precedence_surgery_treatment"))

  # All "treatment"s were preceded by "surgery".
  # George Doe lacks "treatment", so rule is satisfied.
  expect_true(all(pre$precedence_surgery_treatment))
})

test_that("test precedence on grouped_activitylog", {

  load("./testdata/patients_act_grouped_resource.rda")

  pre <- patients_act_grouped_resource %>%
    check_rule(precedence(activity_a = "surgery", activity_b = "treatment"))

  expect_s3_class(pre, "grouped_activitylog")

  expect_equal(dim(pre), c(nrow(patients_act_grouped_resource), ncol(patients_act_grouped_resource) + 1))
  expect_equal(colnames(pre), c(colnames(patients_act_grouped_resource), "precedence_surgery_treatment"))
  expect_equal(groups(pre), groups(patients_act_grouped_resource))

  # All "treatment"s were preceded by "surgery".
  # George Doe lacks "treatment", so rule is satisfied.
  expect_true(all(pre$precedence_surgery_treatment))
})
