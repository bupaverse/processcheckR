
#### eventlog ####

test_that("test filter_rules on eventlog", {

  load("./testdata/patients.rda")

  filter <- patients %>%
    filter(!(patient == "Jane Doe" & activity == "surgery")) %>%
    filter_rules(start = starts(activity = "check-in"),
                 prec = precedence("surgery", "treatment"))

  expected <- patients %>%
    edeaR::filter_case("John Doe")

  expect_s3_class(filter, "eventlog")

  expect_equal(dim(filter), c(nrow(expected), ncol(expected)))
  expect_equal(colnames(filter), colnames(expected))

  # Only John Doe does starts with "check-in" and has "surgery" before "treatment".
  expect_true(all(filter[[case_id(filter)]] == "John Doe"))
})

test_that("test filter_rules on eventlog without named rules", {

  load("./testdata/patients.rda")

  filter <- patients %>%
    filter(!(patient == "Jane Doe" & activity == "surgery")) %>%
    filter_rules(starts(activity = "check-in"),
                 precedence("surgery", "treatment"))

  expected <- patients %>%
    edeaR::filter_case("John Doe")

  expect_s3_class(filter, "eventlog")

  expect_equal(dim(filter), c(nrow(expected), ncol(expected)))
  expect_equal(colnames(filter), colnames(expected))

  # Only John Doe does starts with "check-in" and has "surgery" before "treatment".
  expect_true(all(filter[[case_id(filter)]] == "John Doe"))
})

test_that("test filter_rules on eventlog fails when no rules supplied", {

  load("./testdata/patients.rda")

  expect_error(
    filter <- patients %>%
      filter_rules(),
    "*At least one filtering rule should be supplied*")
})

test_that("test filter_rules on grouped_eventlog", {

  load("./testdata/patients_grouped_resource.rda")

  filter <- patients_grouped_resource %>%
    filter(!(patient == "Jane Doe" & activity == "surgery")) %>%
    filter_rules(start = starts(activity = "check-in"),
                 prec = precedence("surgery", "treatment"))

  expected <- patients_grouped_resource %>%
    edeaR::filter_case("John Doe")

  expect_s3_class(filter, "grouped_eventlog")

  expect_equal(dim(filter), c(nrow(expected), ncol(expected)))
  expect_true(compare::compareIgnoreOrder(colnames(expected), colnames(filter))$result)
  expect_equal(groups(filter), groups(patients_grouped_resource))

  # Only John Doe does starts with "check-in" and has "surgery" before "treatment".
  expect_true(all(filter[[case_id(filter)]] == "John Doe"))
})


#### activitylog ####

test_that("test filter_rules on activitylog", {

  load("./testdata/patients_act.rda")

  filter <- patients_act %>%
    filter(!(patient == "Jane Doe" & activity == "surgery")) %>%
    filter_rules(start = starts(activity = "check-in"),
                 prec = precedence("surgery", "treatment"))

  expected <- patients_act %>%
    edeaR::filter_case("John Doe")

  expect_s3_class(filter, "activitylog")

  expect_equal(dim(filter), c(nrow(expected), ncol(expected)))
  expect_true(compare::compareIgnoreOrder(colnames(expected), colnames(filter))$result)

  # Only John Doe does starts with "check-in" and has "surgery" before "treatment".
  expect_true(all(filter[[case_id(filter)]] == "John Doe"))
})

test_that("test filter_rules on grouped_activitylog", {

  load("./testdata/patients_act_grouped_resource.rda")

  filter <- patients_act_grouped_resource %>%
    filter(!(patient == "Jane Doe" & activity == "surgery")) %>%
    filter_rules(start = starts(activity = "check-in"),
                 prec = precedence("surgery", "treatment"))

  expected <- patients_act_grouped_resource %>%
    edeaR::filter_case("John Doe")

  expect_s3_class(filter, "grouped_activitylog")

  expect_equal(dim(filter), c(nrow(expected), ncol(expected)))
  expect_true(compare::compareIgnoreOrder(colnames(expected), colnames(filter))$result)
  expect_equal(groups(filter), groups(patients_act_grouped_resource))

  # Only John Doe does starts with "check-in" and has "surgery" before "treatment".
  expect_true(all(filter[[case_id(filter)]] == "John Doe"))
})
