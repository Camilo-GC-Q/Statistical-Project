library(shinytest2)

test_csv = testthat::test_path("../../McMillanAcheArmadillo.csv")

# Start app and upload data

start_app = function() {
    app = AppDriver$new(
        app_dir = testthat::test_path("../.."),
        timeout = 15000
    )
    app$upload_file(file = test_csv)
    app$wait_for_idle()
    app
}

# App load test
test_that("app loads", {
    app = AppDriver$new(
        app_dir = testthat::test_path("../.."),
        timeout = 15000
    )
    app$upload_file(file = test_csv)
    app$wait_for_idle()
    output = app$get_values()$output
    expect_false(is.null(output$preview))
    app$stop()
})

# Poisson fitting with multiple predictors
test_that("Poisson fits with multiple predictors", {
    app = start_app()
    app$set_inputs(response = "Armadillos")
    app$wait_for_idle()
    app$set_inputs(predictors = c("Age", "Treks"), model_type = "Poisson")
    app$wait_for_idle()
    app$click("fit_model")
    app$wait_for_idle()
    expect_false(is.null(app$get_values()$output$dynamic_irr_table))
    app$stop()
})


# Negative Binomial fit
test_that("Negative Binomial fits", {
    app = start_app()
    app$set_inputs(response = "Armadillos")
    app$wait_for_idle()
    app$set_inputs(predictors = c("Age", "Treks"), model_type = "Negative Binomial")
    app$wait_for_idle()
    app$click("fit_model")
    app$wait_for_idle()
    expect_false(is.null(app$get_values()$output$dynamic_irr_table))
    app$stop()
})



# Modeling with interactions
test_that("Interaction term doesn't crash", {
    app = start_app()
    app$set_inputs(response = "Armadillos")
    app$wait_for_idle()
    app$set_inputs(predictors = c("Age", "Treks", "Age:Treks"), model_type = "Poisson")
    app$wait_for_idle()
    app$click("fit_model")
    app$wait_for_idle()
    expect_false(is.null(app$get_values()$output$dynamic_irr_table))
    app$stop()
})


# Check ZIP model fits
test_that("ZIP model fits", {
    app = start_app()
    app$set_inputs(response = "Armadillos")
    app$wait_for_idle()
    app$set_inputs(predictors = c("Age", "Treks"), model_type = "Zero-Inflated Poisson")
    app$wait_for_idle()
    app$click("fit_model")
    app$wait_for_idle()
    expect_false(is.null(app$get_values()$output$dynamic_irr_table))
    app$stop()
})


# Quasipoisson
test_that("Quasipoisson fits", {
    app = start_app()
    app$set_inputs(response = "Armadillos")
    app$wait_for_idle()
    app$set_inputs(predictors = c("Age", "Treks"), model_type = "Quasipoisson")
    app$wait_for_idle()
    app$click("fit_model")
    app$wait_for_idle()
    expect_false(is.null(app$get_values()$output$dynamic_irr_table))
    app$stop()
})


# ZINB
test_that("ZINB fits", {
    app = start_app()
    app$set_inputs(response = "Armadillos")
    app$wait_for_idle()
    app$set_inputs(predictors = c("Age", "Treks"), model_type = "Zero-Inflated Negative Binomial")
    app$wait_for_idle()
    app$click("fit_model")
    app$wait_for_idle()
    expect_false(is.null(app$get_values()$output$dynamic_irr_table))
    app$stop()
})


# Single predictor
test_that("single predictor works", {
    app = start_app()
    app$set_inputs(response = "Armadillos")
    app$wait_for_idle()
    app$set_inputs(predictors = "Age", model_type = "Poisson")
    app$wait_for_idle()
    app$click("fit_model")
    app$wait_for_idle()
    expect_false(is.null(app$get_values()$output$dynamic_irr_table))
    app$stop()
})


# Interaction without main effect
test_that("interaction without main effect doesn't crash", {
    app = start_app()
    app$set_inputs(response = "Armadillos")
    app$wait_for_idle()
    app$set_inputs(predictors = c("Age", "Age:Treks"), model_type = "Poisson")
    app$wait_for_idle()
    app$click("fit_model")
    app$wait_for_idle()
    expect_false(is.null(app$get_values()$output$dynamic_irr_table))
    app$stop()
})
