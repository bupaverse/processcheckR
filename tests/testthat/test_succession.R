
#### eventlog ####

test_that("test succession on eventlog", {

  load("./testdata/patients.rda")

  suc <- patients %>%
    check_rule(succession(activity_a = "surgery", activity_b = "treatment"))

  expect_s3_class(suc, "eventlog")

  expect_equal(dim(suc), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(suc), c(colnames(patients), "succession_surgery_treatment"))

  # John Doe's 2nd "surgery" is not (eventually) followed by "treatment".
  # George Doe lacks both "treatment" and "surgery", so rule is satisfied.
  expect_true(all(suc[suc$patient != "John Doe",]$succession_surgery_treatment))
  expect_false(any(suc[suc$patient == "John Doe",]$succession_surgery_treatment))
})

test_that("test succession on eventlog fails on non-existing activity", {

  load("./testdata/patients.rda")

  expect_error(
    suc <- patients %>%
      check_rule(succession("blood sample", "administer medication")),
    "*Activity blood sample not found in log*")

  expect_error(
    suc <- patients %>%
      check_rule(succession("check-in", "administer medication")),
    "*Activity administer medication not found in log*")
})

test_that("test succession on grouped_eventlog", {

  load("./testdata/patients_grouped_resource.rda")

  suc <- patients_grouped_resource %>%
    check_rule(succession(activity_a = "surgery", activity_b = "treatment"))

  expect_s3_class(suc, "grouped_eventlog")

  expect_equal(dim(suc), c(nrow(patients_grouped_resource), ncol(patients_grouped_resource) + 1))
  expect_equal(colnames(suc), c(colnames(patients_grouped_resource), "succession_surgery_treatment"))

  # John Doe's 2nd "surgery" is not (eventually) followed by "treatment".
  # George Doe lacks both "treatment" and "surgery", so rule is satisfied.
  expect_true(all(suc[suc$patient != "John Doe",]$succession_surgery_treatment))
  expect_false(any(suc[suc$patient == "John Doe",]$succession_surgery_treatment))
})


#### activitylog ####

test_that("test succession on activitylog", {

  load("./testdata/patients_act.rda")

  suc <- patients_act %>%
    check_rule(succession(activity_a = "surgery", activity_b = "treatment"))

  expect_s3_class(suc, "activitylog")

  expect_equal(dim(suc), c(nrow(patients_act), ncol(patients_act) + 1))
  expect_equal(colnames(suc), c(colnames(patients_act), "succession_surgery_treatment"))

  # John Doe's 2nd "surgery" is not (eventually) followed by "treatment".
  # George Doe lacks both "treatment" and "surgery", so rule is satisfied.
  expect_true(all(suc[suc$patient != "John Doe",]$succession_surgery_treatment))
  expect_false(any(suc[suc$patient == "John Doe",]$succession_surgery_treatment))
})

test_that("test succession on grouped_activitylog", {

  load("./testdata/patients_act_grouped_resource.rda")

  suc <- patients_act_grouped_resource %>%
    check_rule(succession(activity_a = "surgery", activity_b = "treatment"))

  expect_s3_class(suc, "grouped_activitylog")

  expect_equal(dim(suc), c(nrow(patients_act_grouped_resource), ncol(patients_act_grouped_resource) + 1))
  expect_equal(colnames(suc), c(colnames(patients_act_grouped_resource), "succession_surgery_treatment"))

  # John Doe's 2nd "surgery" is not (eventually) followed by "treatment".
  # George Doe lacks both "treatment" and "surgery", so rule is satisfied.
  expect_true(all(suc[suc$patient != "John Doe",]$succession_surgery_treatment))
  expect_false(any(suc[suc$patient == "John Doe",]$succession_surgery_treatment))
})