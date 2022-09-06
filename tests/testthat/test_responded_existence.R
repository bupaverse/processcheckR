
#### eventlog ####

test_that("test responded_existence on eventlog", {

  load("./testdata/patients.rda")

  res <- patients %>%
    filter(!(patient == "Jane Doe" & activity == "check-out")) %>%
    check_rule(responded_existence(activity_a = "check-in", activity_b = "check-out"))

  expect_s3_class(res, "eventlog")

  expect_equal(dim(res), c(nrow(patients) - 1, ncol(patients) + 1))
  expect_equal(colnames(res), c(colnames(patients), "responded_existence_check_in_check_out"))

  # Jane Doe lacks "check-out".
  # George Doe lacks both "check-in" and "check-out", so rule is satisfied.
  expect_true(all(res[res$patient != "Jane Doe",]$responded_existence_check_in_check_out))
  expect_false(any(res[res$patient == "Jane Doe",]$responded_existence_check_in_check_out))
})

test_that("test responded_existence on eventlog when activity_a is not present", {

  load("./testdata/patients.rda")

  res <- patients %>%
    check_rule(responded_existence(activity_a = "register", activity_b = "check-out"))

  expect_s3_class(res, "eventlog")

  expect_equal(dim(res), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(res), c(colnames(patients), "responded_existence_register_check_out"))

  # George Doe lacks "check-out".
  # John and Jane Doe lack "register", so rule is satisfied.
  expect_true(all(res[res$patient != "George Doe",]$responded_existence_register_check_out))
  expect_false(any(res[res$patient == "George Doe",]$responded_existence_register_check_out))
})

test_that("test responded_existance on eventlog fails on non-existing activity", {

  load("./testdata/patients.rda")

  expect_error(
    res <- patients %>%
      check_rule(responded_existence("blood sample", "administer medication")),
    "*Activity blood sample not found in log*")

  expect_error(
    res <- patients %>%
      check_rule(responded_existence("check-in", "administer medication")),
    "*Activity administer medication not found in log*")
})

test_that("test responded_existence on grouped_eventlog", {

  load("./testdata/patients_grouped_resource.rda")

  res <- patients_grouped_resource %>%
    filter(!(patient == "Jane Doe" & activity == "check-out")) %>%
    check_rule(responded_existence(activity_a = "check-in", activity_b = "check-out"))

  expect_s3_class(res, "grouped_eventlog")

  expect_equal(dim(res), c(nrow(patients_grouped_resource) - 1, ncol(patients_grouped_resource) + 1))
  expect_true(compare::compareIgnoreOrder(colnames(res), c(colnames(patients_grouped_resource), "responded_existence_check_in_check_out"))$result)
  expect_equal(groups(res), groups(patients_grouped_resource))

  # Jane Doe lacks "check-out".
  # George Doe lacks both "check-in" and "check-out", so rule is satisfied.
  expect_true(all(res[res$patient != "Jane Doe",]$responded_existence_check_in_check_out))
  expect_false(any(res[res$patient == "Jane Doe",]$responded_existence_check_in_check_out))
})


#### activitylog ####

test_that("test responded_existence on activitylog", {

  load("./testdata/patients_act.rda")

  res <- patients_act %>%
    filter(!(patient == "Jane Doe" & activity == "check-out")) %>%
    check_rule(responded_existence(activity_a = "check-in", activity_b = "check-out"))

  expect_s3_class(res, "activitylog")

  expect_equal(dim(res), c(nrow(patients_act) - 1, ncol(patients_act) + 1))
  expect_equal(colnames(res), c(colnames(patients_act), "responded_existence_check_in_check_out"))

  # Jane Doe lacks "check-out".
  # George Doe lacks both "check-in" and "check-out", so rule is satisfied.
  expect_true(all(res[res$patient != "Jane Doe",]$responded_existence_check_in_check_out))
  expect_false(any(res[res$patient == "Jane Doe",]$responded_existence_check_in_check_out))
})

test_that("test responded_existence on grouped_activitylog", {

  load("./testdata/patients_act_grouped_resource.rda")

  res <- patients_act_grouped_resource %>%
    filter(!(patient == "Jane Doe" & activity == "check-out")) %>%
    check_rule(responded_existence(activity_a = "check-in", activity_b = "check-out"))

  expect_s3_class(res, "grouped_activitylog")

  expect_equal(dim(res), c(nrow(patients_act_grouped_resource) - 1, ncol(patients_act_grouped_resource) + 1))
  expect_true(compare::compareIgnoreOrder(colnames(res), c(colnames(patients_act_grouped_resource), "responded_existence_check_in_check_out"))$result)
  expect_equal(groups(res), groups(patients_act_grouped_resource))

  # Jane Doe lacks "check-out".
  # George Doe lacks both "check-in" and "check-out", so rule is satisfied.
  expect_true(all(res[res$patient != "Jane Doe",]$responded_existence_check_in_check_out))
  expect_false(any(res[res$patient == "Jane Doe",]$responded_existence_check_in_check_out))
})
