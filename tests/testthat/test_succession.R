
#### eventlog ####

test_that("test succession on eventlog", {

  skip("succession not correctly implemented")

  load("./testdata/patients.rda")

  suc <- patients %>%
    check_rule(succession(activity_a = "surgery", activity_b = "treatment"))

  expect_s3_class(suc, "eventlog")

  expect_equal(dim(suc), c(nrow(patients), ncol(patients) + 1))
  expect_equal(colnames(suc), c(colnames(patients), "response_treatment_surgery"))

  # Jane Doe's "treatment" is not (eventually) followed by "surgery".
  # George Doe lacks both "treatment" and "surgery", so rule is satisfied.
  expect_true(all(suc[suc$patient != "Jane Doe",]$response_treatment_surgery))
  expect_false(any(suc[suc$patient == "Jane Doe",]$response_treatment_surgery))
})
