
#### eventlog ####

test_that("test xor on eventlog", {

  load("./testdata/patients.rda")

  xor <- patients %>%
    filter(!(patient == "Jane Doe" & activity == "check-out")) %>%
    check_rule(xor("check-in", "check-out"))

  expect_s3_class(xor, "eventlog")

  expect_equal(dim(xor), c(nrow(patients) - 1, ncol(patients) + 1))
  expect_equal(colnames(xor), c(colnames(patients), "xor_check_in_check_out"))

  # John Doe contains both "check-in" and "check-out", so rule is not satisfied.
  # Jane Doe contains "check-in", but lacks "check-out", so rule is satisfied.
  # George Doe lacks both "check-in" and "check-out", so rule is satisfied.
  expect_true(all(xor[xor$patient != "John Doe",]$xor_check_in_check_out))
  expect_false(any(xor[xor$patient == "John Doe",]$xor_check_in_check_out))
})

test_that("test xor on eventlog", {

  load("./testdata/patients.rda")

  xor <- patients %>%
    filter(!(patient == "Jane Doe" & activity == "check-in")) %>%
    check_rule(xor("check-in", "check-out"))

  expect_s3_class(xor, "eventlog")

  expect_equal(dim(xor), c(nrow(patients) - 1, ncol(patients) + 1))
  expect_equal(colnames(xor), c(colnames(patients), "xor_check_in_check_out"))

  # John Doe contains both "check-in" and "check-out", so rule is not satisfied.
  # Jane Doe contains "check-out", but lacks "check-in", so rule is satisfied.
  # George Doe lacks both "check-in" and "check-out", so rule is satisfied.
  expect_true(all(xor[xor$patient != "John Doe",]$xor_check_in_check_out))
  expect_false(any(xor[xor$patient == "John Doe",]$xor_check_in_check_out))
})

test_that("test xor on eventlog fails on non-existing activity", {

  load("./testdata/patients.rda")

  expect_error(
    xor <- patients %>%
      check_rule(xor("blood sample", "administer medication")),
    "*Activity blood sample not found in log*")

  expect_error(
    xor <- patients %>%
      check_rule(xor("check-in", "administer medication")),
    "*Activity administer medication not found in log*")
})

test_that("test xor on grouped_eventlog", {

  load("./testdata/patients_grouped_resource.rda")

  xor <- patients_grouped_resource %>%
    filter(!(patient == "Jane Doe" & activity == "check-out")) %>%
    check_rule(xor("check-in", "check-out"))

  expect_s3_class(xor, "grouped_eventlog")

  expect_equal(dim(xor), c(nrow(patients_grouped_resource) - 1, ncol(patients_grouped_resource) + 1))
  expect_true(compare::compareIgnoreOrder(colnames(xor), c(colnames(patients_grouped_resource), "xor_check_in_check_out"))$result)
  expect_equal(groups(xor), groups(patients_grouped_resource))

  # John Doe contains both "check-in" and "check-out", so rule is not satisfied.
  # Jane Doe contains "check-in", but lacks "check-out", so rule is satisfied.
  # George Doe lacks both "check-in" and "check-out", so rule is satisfied.
  expect_true(all(xor[xor$patient != "John Doe",]$xor_check_in_check_out))
  expect_false(any(xor[xor$patient == "John Doe",]$xor_check_in_check_out))
})


#### activitylog ####

test_that("test xor on activitylog", {

  load("./testdata/patients_act.rda")

  xor <- patients_act %>%
    filter(!(patient == "Jane Doe" & activity == "check-out")) %>%
    check_rule(xor("check-in", "check-out"))

  expect_s3_class(xor, "activitylog")

  expect_equal(dim(xor), c(nrow(patients_act) - 1, ncol(patients_act) + 1))
  expect_equal(colnames(xor), c(colnames(patients_act), "xor_check_in_check_out"))

  # John Doe contains both "check-in" and "check-out", so rule is not satisfied.
  # Jane Doe contains "check-in", but lacks "check-out", so rule is satisfied.
  # George Doe lacks both "check-in" and "check-out", so rule is satisfied.
  expect_true(all(xor[xor$patient != "John Doe",]$xor_check_in_check_out))
  expect_false(any(xor[xor$patient == "John Doe",]$xor_check_in_check_out))
})

test_that("test xor on grouped_activityloglog", {

  load("./testdata/patients_act_grouped_resource.rda")

  xor <- patients_act_grouped_resource %>%
    filter(!(patient == "Jane Doe" & activity == "check-out")) %>%
    check_rule(xor("check-in", "check-out"))

  expect_s3_class(xor, "grouped_activitylog")

  expect_equal(dim(xor), c(nrow(patients_act_grouped_resource) - 1, ncol(patients_act_grouped_resource) + 1))
  expect_true(compare::compareIgnoreOrder(colnames(xor), c(colnames(patients_act_grouped_resource), "xor_check_in_check_out"))$result)
  expect_equal(groups(xor), groups(patients_act_grouped_resource))

  # John Doe contains both "check-in" and "check-out", so rule is not satisfied.
  # Jane Doe contains "check-in", but lacks "check-out", so rule is satisfied.
  # George Doe lacks both "check-in" and "check-out", so rule is satisfied.
  expect_true(all(xor[xor$patient != "John Doe",]$xor_check_in_check_out))
  expect_false(any(xor[xor$patient == "John Doe",]$xor_check_in_check_out))
})
