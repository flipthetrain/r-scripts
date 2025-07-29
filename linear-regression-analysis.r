# nolint
# Introduction ---
#################################################################
#################################################################
# This is a generic R script template for Linear Regression and
# the corresponding statistical analysis.
#
# Documentation needs to go here
# ------------------------------------------------------------------------------
# USAGE INSTRUCTIONS:
#
# 1. Place this script in your working directory.
# 2. At the top of the script, set the global variables:
#    - 'output_file': the file where all analysis output will be saved.
#    - 'sig': the significance level for statistical tests (default is 0.05).
# 3. Select and configure your data source:
#    - Use the appropriate section to load your data:
#         "Load R dataset"
#         "Load CSV data"
#         "Load JSON data"
#         "Load SQLite data"
#         "Load ODBC data"
#    - For your own data, place a CSV, JSON, SQLite, or ODBC file in the same directory, named after your dataset (e.g., mydata.csv, mydata.json, mydata.sqlite, mydata.dsn).
#    - Only enable (uncomment) the section that matches your data source.
# 4. Configure your model(s):
#    - Set up your regression model(s) as needed in the script.
#    - You can define multiple models for comparison or diagnostics.
# 5. Run the analysis and plotting functions:
#    - Use analyze_all(model, model_name, model_dir) to perform all statistical analyses for a model.
#    - Use plot_all(model, model_name, png_dir) to generate and save all diagnostic plots for a model.
#    - Replace 'model', 'model_name', and 'model_dir' (or 'png_dir') with your actual model object and desired output names/paths.
# 6. Run the script in R or RStudio. Output will be saved to the file specified by 'output_file', and plots will be saved to the 'build/png' folder.
# 7. Review the output file for analysis results, diagnostics, and interpretation.
# 8. Review the 'build/png' folder for diagnostic plots for each model.
#
# FUNCTION DESCRIPTIONS:
#
# Analysis Functions (analyze_*):
#   analyze_rainbow: Tests for linearity in the regression function using the Rainbow test.
#   analyze_residual_normality: Assesses normality of residuals using Shapiro-Wilk and Anderson-Darling tests.
#   analyze_cooks_distance: Identifies influential observations using Cook's distance.
#   analyze_principal_components: Performs principal component analysis (PCA) on selected variables.
#   analyze_colinearity: Computes and displays the correlation matrix to assess collinearity among variables.
#   analyze_r2: Reports R-squared and adjusted R-squared (or pseudo R-squared for GLMs).
#   analyze_goodness_of_fit: Tests overall model adequacy using F-test (lm) or likelihood ratio test (glm).
#   analyze_homoscedasticity: Tests for constant error variance using Breusch-Pagan and White's tests.
#   analyze_autocorrelation: Tests for autocorrelation in residuals using Durbin-Watson and Box-Ljung tests.
#   analyze_coefficients: Tests the statistical significance of each model coefficient.
#   analyze_vif: Assesses multicollinearity using the Variance Inflation Factor (VIF).
#   analyze_all: Runs all major analysis functions for a model in logical order.
#
# Output Functions (output_*):
#   output_regression_equation: Prints the regression equation for the fitted model in a readable format.
#
# Plotting Functions (plot_*):
#   plot_acf: Plots the autocorrelation function (ACF) of model residuals.
#   plot_residuals_vs_leverage: Plots residuals against leverage to identify influential points.
#   plot_cooks_distance_scatter: Plots Cook's distance for each observation.
#   plot_cooks_distance_vs_leverage: Plots Cook's distance versus leverage for all observations.
#   plot_all: Runs all major plotting functions for a model and saves plots to disk.
# ------------------------------------------------------------------------------
#################################################################
#################################################################

# Github Repository:
#################################################################
#################################################################
#   https://github.com/flipthetrain/r-scripts/blob/main/linear-regression-analysis.r
#################################################################
#################################################################

# License ---
#################################################################
#################################################################
#
# This is free and unencumbered software released into the public domain.
#
# Anyone is free to copy, modify, publish, use, compile, sell, or
# distribute this software, either in source code form or as a compiled
# binary, for any purpose, commercial or non-commercial, and by any
# means.
#
# In jurisdictions that recognize copyright laws, the author or authors
# of this software dedicate any and all copyright interest in the
# software to the public domain. We make this dedication for the benefit
# of the public at large and to the detriment of our heirs and
# successors. We intend this dedication to be an overt act of
# relinquishment in perpetuity of all present and future rights to this
# software under copyright law.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
# OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.
#
# For more information, please refer to <https://unlicense.org/>
#################################################################
#################################################################

# wrap the analysis in a tryCatch block to handle errors gracefully
tryCatch({
  # Session Setup ----
  # Close all open View windows at the start of the script
  if (exists(".rs.api.closeAllViewers")) .rs.api.closeAllViewers()
  # start with a clean environment
  rm(list = ls())
  # Load core packages ----
  # other packages may be loaded later as needed
  if (!require(car)) install.packages("car")
  library(car)
  if (!require(gtools)) install.packages("gtools")
  library(gtools)
  if (!require(knitr)) install.packages("knitr")
  library(knitr)
  if (!require(leaps)) install.packages("leaps")
  library(leaps)
  if (!require(lmtest)) install.packages("lmtest")
  library(lmtest)
  if (!require(nortest)) install.packages("nortest")
  library(nortest)
  if (!require(tseries)) install.packages("tseries")
  library(tseries)
  if (!require(strucchange)) install.packages("strucchange")
  library(strucchange)
  # Global Variables ----
  sig <- 0.05
  output_file <- "Assignment 6.r.out.txt"
  # Files setup ----
  # Get the path of the current file as the base for png_dir
  script_path <- normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE)
  if (is.na(script_path)) script_path <- normalizePath(".", winslash = "/")
  base_dir <- dirname(script_path)
  png_dir <- file.path(base_dir, "build", "png")
  data_dir <- file.path(base_dir, "data")
  # Remove the png_dir folder and all its contents if it exists, then create it
  if (dir.exists(png_dir)) {
    unlink(png_dir, recursive = TRUE, force = TRUE)
    dir.create(png_dir, recursive = TRUE)
    # close all graphics devices
    if (length(dev.list()) > 0) {
      graphics.off()
    }
    # Remove previous output file if it exists
    out_txt <- file.path(base_dir, output_file)
    if (file.exists(out_txt)) {
      file.remove(out_txt)
    }
    # Output setup ----
    # Set output width for printing to 120 columns
    options(width = 120)
    # Save all console output to a text file
    sink(out_txt, split = TRUE)
    # Load Data ----
    cat("#####################################################################################\n")
    cat("Load the dataset:\n")
    cat("#####################################################################################\n")
    # playing around with a ridiculous number of ways to load data
    # load R dataset
    if (!require(faraway)) install.packages("faraway")
    library(faraway)
    full_dataname <- "gala"
    package_name <- "faraway"
    cat(sprintf("Loading data from R package: %s (dataset: %s)\n", package_name, full_dataname))
    data(list = full_dataname, package = package_name)
    full_data <- get(full_dataname)
    #
    # # load CSV dataset
    # full_dataname <- "gala"
    # csv_path <- file.path(data_dir, paste0(full_dataname, ".csv"))
    # cat(sprintf("Loading data from CSV: %s\n", csv_path))
    # full_data <- read.csv(csv_path, stringsAsFactors = TRUE)
    #
    # # load JSON dataset
    # if (!require(jsonlite)) install.packages("jsonlite")
    # library(jsonlite)
    # json_path <- file.path(data_dir, paste0(full_dataname, ".json"))
    # cat(sprintf("Loading data from JSON: %s\n", json_path))
    # full_data <- jsonlite::fromJSON(json_path)
    # if (!is.data.frame(full_data)) {
    #   full_data <- as.data.frame(full_data)
    # }
    #
    # # load SQLITE query
    # if (!require(DBI)) install.packages("DBI")
    # library(DBI)
    # if (!require(RSQLite)) install.packages("RSQLite")
    # library(RSQLite)
    # sql_db_path <- file.path(data_dir, paste0(full_dataname, ".sqlite"))
    # sql_query_path <- file.path(data_dir, paste0(full_dataname, ".sql"))
    # cat(sprintf("Loading data from SQL database: %s using query: %s\n", sql_db_path, sql_query_path))
    # con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sql_db_path)
    # sql_query <- paste(readLines(sql_query_path, warn = FALSE), collapse = "\n")
    # full_data <- DBI::dbGetQuery(con, sql_query)
    # DBI::dbDisconnect(con)
    #
    # # load ODBC query
    # if (!require(DBI)) install.packages("DBI")
    # library(DBI)
    # if (!require(odbc)) install.packages("odbc")
    # library(odbc)
    # odbc_dsn_path <- file.path(data_dir, paste0(full_dataname, ".dsn"))
    # odbc_query_path <- file.path(data_dir, paste0(full_dataname, ".sql"))
    # dsn_name <- readLines(odbc_dsn_path, warn = FALSE)[1]
    # sql_query <- paste(readLines(odbc_query_path, warn = FALSE), collapse = "\n")
    # cat(sprintf("Loading data from ODBC DSN: %s using query: %s\n", dsn_name, odbc_query_path))
    # con <- DBI::dbConnect(odbc::odbc(), dsn = dsn_name)
    # full_data <- DBI::dbGetQuery(con, sql_query)
    # DBI::dbDisconnect(con)
    # List Initial Variables ----
    cat("#####################################################################################\n")
    cat("List initial variables\n")
    cat("#####################################################################################\n")
    cat("Initial variable names are:\n")
    allVars <- names(full_data)
    cat(paste(allVars, collapse = ", "))
    cat("\n\n")
    # Explore Initial Data ----
    cat("#####################################################################################\n")
    cat("Explore initial data\n")
    cat("#####################################################################################\n")
    cat("First several rows of full_data (", full_dataname, " dataset):\n")
    cat(paste(capture.output(print(kable(head(full_data), format = "simple"))), collapse = "\n"), "\n", sep = "")
    cat("\n\n")
    # Variable Identification ----
    cat("#####################################################################################\n")
    cat("Identify analysis variables\n")
    cat("#####################################################################################\n")
    dep_var <- "Species"
    predictors <- setdiff(names(full_data), dep_var)
    cat("Dependent variable\n")
    cat(dep_var, "\n")
    cat("Predictor variables \n")
    cat(paste(predictors, collapse = ", "), "\n")
    cat("\n\n")
    # Clean and Format Data ----
    cat("#####################################################################################\n")
    cat("Clean and format data\n")
    cat("#####################################################################################\n")
    # nothing to do here
    # Create Dummy Variables ----
    cat("#####################################################################################\n")
    cat("Create dummy variables for categorial predictors\n")
    cat("#####################################################################################\n")
    cat_vars <- predictors[sapply(full_data[predictors], function(x) is.factor(x) || is.character(x))]
    cat("Appending explicitly identified predictor variables to convert to 0-1 dummy variables...\n")
    cat_vars <- append(cat_vars, "")
    if (length(cat_vars) > 0) {
      cat("Categorical predictor variables found:", paste(cat_vars, collapse = ", "), "\n")
      for (cat_var in cat_vars) {
        unique_vals <- unique(full_data[[cat_var]])
        for (val in unique_vals) {
          val_clean <- gsub("[^A-Za-z0-9]", "_", as.character(val))
          dummy_name <- paste0("is_", cat_var, "_", val_clean)
          full_data[[dummy_name]] <- ifelse(full_data[[cat_var]] == val, 1, 0)
        }
        full_data[[cat_var]] <- NULL
      }
      cat("Dummy variables for predictors created and added to full_data.\n")
      predictors <- setdiff(names(full_data), dep_var)
    } else {
      cat("No categorical predictor variables found. No dummy variables created.\n")
    }
    cat("\n\n")
    # Partition Data ----
    cat("#####################################################################################\n")
    cat("Partition Data into Model Data and Test Data\n")
    cat("#####################################################################################\n")
    set.seed(123) # for reproducibility
    train_data_pct <- 1.0
    test_data_pct <- 1 - train_data_pct
    sample_size <- floor(train_data_pct * nrow(full_data))
    train_indices <- sample(seq_len(nrow(full_data)), size = sample_size)
    train_data <- full_data[train_indices, ]
    test_data <- full_data[-train_indices, ]
    cat(sprintf("Original data rows: %d\n", nrow(full_data)))
    cat(sprintf("Train data rows (%d%%): %d\n", train_data_pct * 100, nrow(train_data)))
    cat(sprintf("Test data rows (%d%%): %d\n", test_data_pct * 100, nrow(test_data)))
    cat("\n\n")
    # List Final Variables ----
    cat("#####################################################################################\n")
    cat("List final variables\n")
    cat("#####################################################################################\n")
    cat("Dependent variable name is:\n")
    cat(dep_var, "\n")
    cat("Predictor variable names are:\n")
    cat(paste(predictors, collapse = ", "), "\n")
    cat("\n\n")
    # Explore Final Data ----
    cat("#####################################################################################\n")
    cat("Explore final data\n")
    cat("#####################################################################################\n")
    cat("First several rows of full_data (with any dummy variables):\n")
    cat(paste(capture.output(print(kable(head(full_data), format = "simple"))), collapse = "\n"), "\n", sep = "")
    cat("\n\n")
    # Output Function Definitions ----
    output_regression_equation <- function(model, label = "") {
      cat(paste0("\n", label, " -- Regression Equation\n"))
      coefs <- coef(model)
      response <- all.vars(formula(model))[1]
      predictors <- names(coefs)[-1]
      intercept <- coefs[1]
      eqn <- sprintf("%s = %.4f", response, intercept)
      if (length(predictors) > 0) {
        for (i in seq_along(predictors)) {
          coef_val <- coefs[predictors[i]]
          sign <- ifelse(coef_val >= 0, " + ", " - ")
          eqn <- paste0(eqn, sign, sprintf("%.4f*%s", abs(coef_val), predictors[i]))
        }
      }
      cat(eqn, "\n\n")
    }
    # Analysis Function Definitions ----
    analyze_rainbow <- function(model, label = "Rainbow Test for Linearity", sig = 0.05) {
      cat(paste0("\n", label, "\n"))
      if (is.null(model)) {
        cat("  No model provided for rainbow test.\n\n")
        cat("Test Statistics:\n  Not available.\n")
        cat("Interpretation:\n  Not available.\n\n")
        return()
      }
      model_for_test <- model
      if (inherits(model, "glm")) {
        model_for_test <- tryCatch(
          {
            lm(formula(model), data = model$data)
          },
          error = function(e) NULL
        )
        if (is.null(model_for_test)) {
          cat("  Could not extract lm equivalent from glm model for rainbow test.\n\n")
          cat("Test Statistics:\n  Not available.\n")
          cat("Interpretation:\n  Not available.\n\n")
          return()
        }
      }
      cat("Hypotheses:\n")
      cat("  H0: E(y|X) = Xβ (The regression function is linear in the parameters)\n")
      cat("  H1: E(y|X) ≠ Xβ (The regression function is not linear in the parameters)\n")
      cat("Distribution:\n")
      cat("  The rainbow test statistic follows an F distribution under H0.\n")
      result <- tryCatch(
        {
          strucchange::rainbow.test(model_for_test)
        },
        error = function(e) NULL
      )
      cat("Test Statistics:\n")
      if (is.null(result)) {
        cat("  Not available.\n")
        cat("Interpretation:\n  Rainbow test could not be performed.\n\n")
        return()
      }
      cat(sprintf("  Statistic = %.4f\n  df1 = %d\n  df2 = %d\n  p-value = %.4f\n", result$statistic, result$parameter[1], result$parameter[2], result$p.value))
      cat("Interpretation:\n")
      if (result$p.value < sig) {
        cat("  Reject H0. There is evidence that the regression function is not linear in the parameters.\n")
      } else {
        cat("  Fail to reject H0. No evidence against linearity of the regression function.\n")
      }
      cat("\n")
    }
    analyze_residual_normality <- function(model, label = "", sig = 0.05) {
      cat(paste0("\n", label, " -- Residual Normality (Shapiro-Wilk and Anderson-Darling Tests)\n"))
      if (is.null(model)) {
        cat("No model provided for residual normality analysis.\n\n")
        return()
      }
      resids <- resid(model)
      resids <- resids[!is.na(resids)]
      if (length(resids) < 3) {
        cat("Not enough residuals to perform normality tests.\n\n")
        return()
      }
      sw <- shapiro.test(resids)
      cat("Shapiro-Wilk Test:\n")
      cat("  Hypotheses:\n")
      cat("    H0: eᵢ ~ N(0, σ²) (Residuals are normally distributed)\n")
      cat("    H1: eᵢ ≁ N(0, σ²) (Residuals are not normally distributed)\n")
      cat("  Distribution:\n")
      cat("    The Shapiro-Wilk W statistic follows a known distribution under H0 (for n <= 5000).\n")
      cat("  Test Statistics:\n")
      cat(sprintf("    W = %.4f\n    p-value = %.4f\n", sw$statistic, sw$p.value))
      cat("  Interpretation:\n")
      if (sw$p.value < sig) {
        cat("    Reject H0. There is evidence that the residuals are not normally distributed.\n")
      } else {
        cat("    Fail to reject H0. No evidence against normality of residuals.\n")
      }
      cat("\n")
      ad <- nortest::ad.test(resids)
      cat("Anderson-Darling Test:\n")
      cat("  Hypotheses:\n")
      cat("    H0: eᵢ ~ N(0, σ²) (Residuals are normally distributed)\n")
      cat("    H1: eᵢ ≁ N(0, σ²) (Residuals are not normally distributed)\n")
      cat("  Distribution:\n")
      cat("    The Anderson-Darling A^2 statistic is compared to critical values from its own distribution under H0.\n")
      cat("  Test Statistics:\n")
      cat(sprintf("    A^2 = %.4f\n    p-value = %.4f\n", ad$statistic, ad$p.value))
      cat("  Interpretation:\n")
      if (ad$p.value < sig) {
        cat("    Reject H0. There is evidence that the residuals are not normally distributed.\n")
      } else {
        cat("    Fail to reject H0. No evidence against normality of residuals.\n")
      }
      cat("\n")
    }
    analyze_cooks_distance <- function(model, label = "") {
      cat("\nCook's Distance Test", ifelse(label != "", paste0(" (", label, ")"), ""), ":\n")
      cooks <- cooks.distance(model)
      n <- length(cooks)
      threshold <- 4 / n
      influential <- which(cooks > threshold)
      cat("Hypotheses:\n")
      cat(paste0("  H0: Dᵢ ≤ ", sprintf("%.4f", threshold), " ∀ i (No influential observations; all Cook's D values are less than or equal to the threshold)\n"))
      cat(paste0("  H1: ∃ i: Dᵢ > ", sprintf("%.4f", threshold), " (At least one influential observation; at least one Cook's D value exceeds the threshold)\n"))
      cat("Distribution:\n")
      cat("  Cook's distance is compared to a threshold (commonly 4/n) to flag influential points.\n")
      cat(sprintf("Threshold for influential points: 4/n = %.4f\n", threshold))
      cat(sprintf("Max Cook's distance: %.4f\n", max(cooks)))
      cat("Test Statistics:\n")
      if (length(influential) > 0) {
        cat("Influential observations (index, Cook's D > threshold):\n")
        for (i in influential) {
          cat(sprintf("  Obs %d: Cook's D = %.4f\n", i, cooks[i]))
        }
        cat("Interpretation:\n")
        cat("  Reject H0. At least one influential observation detected.\n")
      } else {
        cat("No influential observations detected (all Cook's D <= threshold).\n")
        cat("Interpretation:\n")
        cat("  Fail to reject H0. No influential observations detected.\n")
      }
      cat("\n")
    }
    analyze_principal_components <- function(data, vars, label = "Principal Component Analysis of Selected Variables") {
      cat(paste0("\n", label, "\n"))
      cat("Hypotheses:\n")
      cat("  H0: λ₁ = λ₂ = ... = λₚ (All principal components explain equal or no meaningful variance; no principal component explains substantially more variance than others)\n")
      cat("  H1: ∃ j, k: λⱼ > λₖ (At least one principal component explains substantially more variance; dimensionality reduction is possible)\n")
      cat("Distribution:\n")
      cat("  The variance explained by each principal component is determined by the eigenvalues of the covariance/correlation matrix.\n")
      num_data <- data[, vars, drop = FALSE]
      num_data <- num_data[sapply(num_data, is.numeric)]
      if (ncol(num_data) < 2) {
        cat("Not enough numeric variables to perform PCA.\n\n")
        return(NULL)
      }
      num_data <- num_data[, apply(num_data, 2, function(x) sd(x, na.rm = TRUE) > 0), drop = FALSE]
      if (ncol(num_data) < 2) {
        cat("Not enough numeric variables with variance to perform PCA.\n\n")
        return(NULL)
      }
      pca <- prcomp(num_data, center = TRUE, scale. = TRUE)
      var_explained <- summary(pca)$importance[2, ]
      cat("Test Statistics:\n")
      cat("Proportion of Variance Explained by Each Principal Component:\n")
      cat(paste(capture.output(print(kable(as.matrix(var_explained), format = "simple"))), collapse = "\n"), "\n\n", sep = "")
      cat("Principal Component Loadings:\n")
      cat(paste(capture.output(print(kable(pca$rotation, format = "simple"))), collapse = "\n"), "\n\n", sep = "")
      cat("Interpretation:\n")
      if (any(var_explained > 0.1)) {
        cat("  Reject H0. At least one principal component explains a substantial proportion of variance.\n")
      } else {
        cat("  Fail to reject H0. No principal component explains a substantial proportion of variance.\n")
      }
      return(pca)
    }
    analyze_colinearity <- function(data, label = "Correlation Matrix of Finalized Data") {
      cat(paste0("\n", label, "\n"))
      cat("Hypotheses:\n")
      cat("  H0: ρᵢⱼ = 0 ∀ i ≠ j (All pairs of variables are uncorrelated; no linear correlation between any pair of variables)\n")
      cat("  H1: ∃ (i, j): ρᵢⱼ ≠ 0 (At least one pair of variables is correlated; at least one pair of variables is linearly correlated)\n")
      cat("Distribution:\n")
      cat("  The sample correlation coefficient is used to assess linear relationships between pairs of variables.\n")
      num_data <- data[sapply(data, is.numeric)]
      if (ncol(num_data) < 2) {
        cat("Not enough numeric variables to compute a correlation matrix.\n\n")
        return()
      }
      cor_mat <- cor(num_data, use = "pairwise.complete.obs")
      cat("Test Statistics:\n")
      cat(paste(capture.output(print(kable(cor_mat, format = "simple"))), collapse = "\n"), "\n\n", sep = "")
      cat("Interpretation:\n")
      if (any(abs(cor_mat[upper.tri(cor_mat)]) > 0.7)) {
        cat("  Reject H0. At least one pair of variables is strongly correlated.\n")
      } else {
        cat("  Fail to reject H0. No strong correlations detected.\n")
      }
    }
    analyze_r2 <- function(model, label = "", sig = 0.05) {
      cat(paste0("\n", label, " -- R^2 Analysis\n"))
      if (inherits(model, "glm")) {
        null_deviance <- model$null.deviance
        residual_deviance <- model$deviance
        pseudo_r2 <- 1 - (residual_deviance / null_deviance)
        cat("Pseudo R^2 (1 - Residual Deviance / Null Deviance):\n")
        cat(sprintf("  Null Deviance: %.4f\n  Residual Deviance: %.4f\n  Pseudo R^2: %.4f\n", null_deviance, residual_deviance, pseudo_r2))
        cat("Hypotheses:\n")
        cat("  H0: R²ₚₛₑᵤdₒ = 0 (Model does not explain variance in the dependent variable)\n")
        cat("  H1: R²ₚₛₑᵤdₒ > 0 (Model explains variance in the dependent variable)\n")
        cat("Distribution:\n")
        cat("  Deviance difference (null - residual) is approximately chi-squared distributed with df = df_null - df_resid.\n")
        cat("Test Statistics:\n")
        cat(sprintf("  Null Deviance = %.4f\n  Residual Deviance = %.4f\n  Pseudo R^2 = %.4f\n", null_deviance, residual_deviance, pseudo_r2))
        cat("Interpretation:\n")
        if (pseudo_r2 > 0) {
          cat("  Reject H0. The model explains some variance in the dependent variable.\n")
        } else {
          cat("  Fail to reject H0. The model does not explain variance in the dependent variable.\n")
        }
      } else {
        r2 <- summary(model)$r.squared
        adjr2 <- summary(model)$adj.r.squared
        cat(sprintf("R^2: %.4f\nAdjusted R^2: %.4f\n", r2, adjr2))
        cat("Hypotheses:\n")
        cat("  H0: R² = 0 (Model does not explain variance in the dependent variable)\n")
        cat("  H1: R² > 0 (Model explains variance in the dependent variable)\n")
        cat("Distribution:\n")
        cat("  F-distribution with df1 = number of predictors, df2 = n - number of predictors - 1.\n")
        cat("Test Statistics:\n")
        cat(sprintf("  R^2 = %.4f\n  Adjusted R^2 = %.4f\n", r2, adjr2))
        cat("Interpretation:\n")
        if (r2 > 0) {
          cat("  Reject H0. The model explains some variance in the dependent variable.\n")
        } else {
          cat("  Fail to reject H0. The model does not explain variance in the dependent variable.\n")
        }
      }
      cat("\n")
    }
    analyze_goodness_of_fit <- function(model, label = "", sig = 0.05) {
      cat(paste0("\n", label, " -- Model Adequacy Test\n"))
      if (inherits(model, "glm")) {
        null_deviance <- model$null.deviance
        residual_deviance <- model$deviance
        df_null <- model$df.null
        df_resid <- model$df.residual
        lr_stat <- null_deviance - residual_deviance
        df_diff <- df_null - df_resid
        pval <- pchisq(lr_stat, df_diff, lower.tail = FALSE)
        cat("Likelihood Ratio Test (LRT) for Model Adequacy:\n")
        cat("Hypotheses:\n")
        cat("  H0: β₁ = β₂ = ... = βₚ = 0 (All regression coefficients except intercept are zero; model is not adequate)\n")
        cat("  H1: ∃ j: βⱼ ≠ 0 (At least one regression coefficient is nonzero; model is adequate)\n")
        cat("Distribution:\n")
        cat(sprintf("  Chi-squared distribution with df = %d.\n", df_diff))
        cat("Test Statistics:\n")
        cat(sprintf("  LR = %.4f\n  p-value = %.4f\n", lr_stat, pval))
        cat("Interpretation:\n")
        if (pval < sig) {
          cat("  Reject H0. The model is adequate; it explains a significant amount of variance in the dependent variable.\n")
        } else {
          cat("  Fail to reject H0. The model is not adequate; it does not explain a significant amount of variance in the dependent variable.\n")
        }
      } else {
        fstat <- summary(model)$fstatistic[1]
        df1 <- summary(model)$fstatistic[2]
        df2 <- summary(model)$fstatistic[3]
        pval <- pf(fstat, df1, df2, lower.tail = FALSE)
        cat("F-test for Model Adequacy:\n")
        cat("Hypotheses:\n")
        cat("  H0: β₁ = β₂ = ... = βₚ = 0 (All regression coefficients except intercept are zero; model is not adequate)\n")
        cat("  H1: ∃ j: βⱼ ≠ 0 (At least one regression coefficient is nonzero; model is adequate)\n")
        cat("Distribution:\n")
        cat(sprintf("  F-distribution with df1 = %d, df2 = %d.\n", df1, df2))
        cat("Test Statistics:\n")
        cat(sprintf("  F = %.4f\n  p-value = %.4f\n", fstat, pval))
        cat("Interpretation:\n")
        if (pval < sig) {
          cat("  Reject H0. The model is adequate; it explains a significant amount of variance in the dependent variable.\n")
        } else {
          cat("  Fail to reject H0. The model is not adequate; it does not explain a significant amount of variance in the dependent variable.\n")
        }
      }
      cat("\n")
    }
    analyze_homoscedasticity <- function(model, label = "", sig = 0.05) {
      cat("Jarque-Bera Test for Residual Normality:\n")
      if (is.null(model)) {
        cat("  No model provided for Jarque-Bera test.\n\n")
      } else {
        resids <- resid(model)
        resids <- resids[!is.na(resids)]
        if (length(resids) < 3) {
          cat("  Not enough residuals to perform Jarque-Bera test.\n\n")
        } else {
          jb <- tryCatch(
            {
              tseries::jarque.bera.test(resids)
            },
            error = function(e) NULL
          )
          if (is.null(jb)) {
            cat("  Jarque-Bera test could not be performed due to insufficient data or other error.\n\n")
          } else {
            cat("  Hypotheses:\n")
            cat("    H0: e_i ~ N(0, σ²) (Residuals are normally distributed; skewness = 0, kurtosis = 3)\n")
            cat("    H1: e_i ≁ N(0, σ²) (Residuals are not normally distributed; skewness ≠ 0 and/or kurtosis ≠ 3)\n")
            cat("  Distribution:\n")
            cat("    Chi-squared distribution with 2 degrees of freedom.\n")
            cat("  Test Statistics:\n")
            cat(sprintf("    JB = %.4f\n    p-value = %.4f\n", jb$statistic, jb$p.value))
            cat("  Interpretation:\n")
            if (jb$p.value < sig) {
              cat("    Reject H0. There is evidence that the residuals are not normally distributed.\n")
            } else {
              cat("    Fail to reject H0. No evidence against normality of residuals.\n")
            }
            cat("\n")
          }
        }
      }
      cat(paste0("\n", label, " -- Error Term Homoscedasticity (Breusch-Pagan and White's Tests)\n"))
      if (inherits(model, "lm") || inherits(model, "glm")) {
        bp <- lmtest::bptest(model)
        cat("Breusch-Pagan Test:\n")
        cat("  Hypotheses:\n")
        cat("    H0: σ²₁ = σ²₂ = ... = σ²ₙ (Homoscedasticity; constant variance)\n")
        cat("    H1: ∃ i, j: σ²ᵢ ≠ σ²ⱼ (Heteroscedasticity; non-constant variance)\n")
        cat("  Distribution:\n")
        cat(sprintf("    Chi-squared distribution with df = %d (number of predictors).\n", length(coef(model)) - 1))
        cat("  Test Statistics:\n")
        cat(sprintf("    Breusch-Pagan statistic = %.4f\n    p-value = %.4f\n", bp$statistic, bp$p.value))
        cat("  Interpretation:\n")
        if (bp$p.value < sig) {
          cat("    Reject H0. Evidence of heteroscedasticity in residuals.\n")
        } else {
          cat("    Fail to reject H0. No evidence of heteroscedasticity in residuals.\n")
        }
        cat("\n")
        whites_formula <- as.formula(paste("~ fitted(model) + I(fitted(model)^2)"))
        fitted_vals <- fitted(model)
        non_na_fitted <- sum(!is.na(fitted_vals))
        if (non_na_fitted < 3) {
          cat("White's Test:\n")
          cat("  Not enough non-NA fitted values to perform White's test.\n\n")
        } else {
          wt <- tryCatch(
            {
              lmtest::bptest(model, varformula = whites_formula)
            },
            error = function(e) {
              NULL
            }
          )
          cat("White's Test:\n")
          if (is.null(wt)) {
            cat("  White's test could not be performed due to insufficient data or other error.\n\n")
          } else {
            cat("  Hypotheses:\n")
            cat("    H0: σ²ᵢ = σ² ∀ i (Homoscedasticity; constant variance)\n")
            cat("    H1: ∃ i: σ²ᵢ ≠ σ² (Heteroscedasticity; variance depends on fitted values)\n")
            cat("  Distribution:\n")
            cat(sprintf("    Chi-squared distribution with df = %d (number of fitted terms: fitted + fitted^2).\n", wt$parameter))
            cat("  Test Statistics:\n")
            cat(sprintf("    White's statistic = %.4f\n    p-value = %.4f\n", wt$statistic, wt$p.value))
            cat("  Interpretation:\n")
            if (wt$p.value < sig) {
              cat("    Reject H0. Evidence of heteroscedasticity in residuals (variance depends on fitted values).\n")
            } else {
              cat("    Fail to reject H0. No evidence of heteroscedasticity in residuals (variance does not depend on fitted values).\n")
            }
            cat("\n")
          }
        }
      } else {
        cat("Breusch-Pagan and White's tests not applicable for this model type.\n")
      }
      cat("\n")
    }
    analyze_autocorrelation <- function(model, label = "", sig = 0.05) {
      cat(paste0("\n", label, " -- Error Term Autocorrelation\n"))
      if (inherits(model, "lm") || inherits(model, "glm")) {
        dw <- lmtest::dwtest(model)
        cat("Durbin-Watson Test:\n")
        cat("  Hypotheses:\n")
        cat("    H0: ρ = 0 (No autocorrelation in residuals)\n")
        cat("    H1: ρ ≠ 0 (Autocorrelation present in residuals)\n")
        cat("  Distribution:\n")
        cat("    Durbin-Watson distribution (depends on sample size and regressors).\n")
        cat("  Test Statistics:\n")
        cat(sprintf("    Statistic = %.4f\n    p-value = %.4f\n", dw$statistic, dw$p.value))
        cat("  Interpretation:\n")
        if (dw$p.value < sig) {
          cat("    Reject H0. Evidence of autocorrelation in residuals.\n")
        } else {
          cat("    Fail to reject H0. No evidence of autocorrelation in residuals.\n")
        }
        n <- length(residuals(model))
        lag <- min(10, floor(n / 5))
        bl <- Box.test(residuals(model), lag = lag, type = "Ljung-Box")
        cat("Box-Ljung Test:\n")
        cat("  Hypotheses:\n")
        cat("    H0: ρ₁ = ρ₂ = ... = ρₘ = 0 (No autocorrelation up to lag m; residuals are independent)\n")
        cat("    H1: ∃ j ∈ [1, m]: ρⱼ ≠ 0 (Autocorrelation present up to lag m; residuals are not independent)\n")
        cat("  Distribution:\n")
        cat(sprintf("    Chi-squared distribution with df = %d (lag).\n", lag))
        cat("  Test Statistics:\n")
        cat(sprintf("    Statistic = %.4f\n    p-value = %.4f\n", bl$statistic, bl$p.value))
        cat("  Interpretation:\n")
        if (bl$p.value < sig) {
          cat("    Reject H0. Evidence of autocorrelation in residuals up to lag m.\n")
        } else {
          cat("    Fail to reject H0. No evidence of autocorrelation in residuals up to lag m.\n")
        }
      } else {
        cat("Durbin-Watson and Box-Ljung tests not applicable for this model type.\n")
      }
      cat("\n")
    }
    analyze_coefficients <- function(model, label = "", sig = 0.05) {
      cat(paste0("\n", label, " -- Coefficient Significance Analysis\n"))
      coefs <- summary(model)$coefficients
      for (i in 1:nrow(coefs)) {
        coef_name <- rownames(coefs)[i]
        est <- coefs[i, 1]
        se <- coefs[i, 2]
        stat <- coefs[i, 3]
        pval <- coefs[i, 4]
        cat(sprintf("Coefficient: %s\n", coef_name))
        cat("  Hypotheses:\n")
        cat(sprintf("    H0: β_%s = 0 (No effect of %s on the dependent variable)\n", coef_name, coef_name))
        cat(sprintf("    H1: β_%s ≠ 0 (There is an effect of %s on the dependent variable)\n", coef_name, coef_name))
        cat("  Distribution:\n")
        if (inherits(model, "glm")) {
          cat("    Standard normal (z) distribution under H0.\n")
        } else {
          cat("    t-distribution with df = n - p - 1 under H0.\n")
        }
        cat("  Test Statistics:\n")
        if (inherits(model, "glm")) {
          cat(sprintf("    z = %.4f\n    p-value = %.4f\n", stat, pval))
        } else {
          cat(sprintf("    t = %.4f\n    p-value = %.4f\n", stat, pval))
        }
        cat("  Interpretation:\n")
        if (pval < sig) {
          cat(sprintf("    Reject H0. %s is a significant predictor.\n", coef_name))
        } else {
          cat(sprintf("    Fail to reject H0. %s is not a significant predictor.\n", coef_name))
        }
        cat("\n")
      }
    }
    analyze_vif <- function(model, label = "Variance Inflation Factor (VIF) Test", threshold = 5) {
      cat(paste0("\n", label, "\n"))
      if (!requireNamespace("car", quietly = TRUE)) {
        cat("  Package 'car' is required for VIF calculation. Please install it.\n\n")
        return()
      }
      if (is.null(model)) {
        cat("  No model provided for VIF analysis.\n\n")
        return()
      }
      if (!(inherits(model, "lm") || inherits(model, "glm"))) {
        cat("  VIF analysis only supports lm or glm models.\n\n")
        return()
      }
      cat("Hypotheses:\n")
      cat("  H0: VIF_j ≤ ", threshold, " for all j (No multicollinearity; all variance inflation factors are less than or equal to the threshold)\n")
      cat("  H1: ∃ j: VIF_j > ", threshold, " (Multicollinearity present; at least one variance inflation factor exceeds the threshold)\n")
      cat("Distribution:\n")
      cat("  VIF is a descriptive statistic; no formal distribution.\n")
      vif_vals <- tryCatch(
        {
          car::vif(model)
        },
        error = function(e) NULL
      )
      cat("Test Statistics:\n")
      if (is.null(vif_vals)) {
        cat("  VIF could not be calculated.\n")
        cat("Interpretation:\n  VIF calculation failed.\n\n")
        return()
      }
      if (is.matrix(vif_vals)) {
        vif_out <- vif_vals[, 1]
        names(vif_out) <- rownames(vif_vals)
      } else {
        vif_out <- vif_vals
      }
      for (j in names(vif_out)) {
        cat(sprintf("  %s: VIF = %.3f\n", j, vif_out[j]))
      }
      cat("Interpretation:\n")
      if (any(vif_out > threshold)) {
        cat("  Reject H0. At least one predictor shows evidence of multicollinearity (VIF exceeds threshold).\n")
      } else {
        cat("  Fail to reject H0. No evidence of problematic multicollinearity (all VIFs below threshold).\n")
      }
      cat("\n")
    }
    analyze_all <- function(model, model_name, sig = 0.05) {
      # these functions are ordered according to a logical flow of analysis
      output_regression_equation(model, model_name)
      analyze_r2(model, model_name, sig)
      analyze_goodness_of_fit(model, model_name, sig)
      analyze_coefficients(model, model_name, sig)
      analyze_vif(model, label = paste0(model_name, " -- Variance Inflation Factor (VIF) Test"))
      analyze_homoscedasticity(model, model_name, sig)
      analyze_rainbow(model, label = paste0(model_name, " -- Rainbow Test for Linearity"), sig = sig)
      analyze_autocorrelation(model, model_name, sig)
      analyze_residual_normality(model, model_name, sig)
      analyze_cooks_distance(model, label = model_name)
    }
    # Plots ----
    plot_acf <- function(model, model_name, png_dir) {
      if (is.null(model)) {
        cat("No model provided for ACF plot.\n")
        return()
      }
      if (!(inherits(model, "lm") || inherits(model, "glm"))) {
        cat("plot_acf only supports lm or glm models.\n")
        return()
      }
      plot_dir <- file.path(png_dir, model_name)
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      plot_path <- file.path(plot_dir, "acf_plot.png")
      png(filename = plot_path, width = 800, height = 600)
      acf(resid(model), main = sprintf("ACF of Residuals: %s", model_name))
      coefs <- coef(model)
      response <- all.vars(formula(model))[1]
      predictors <- names(coefs)[-1]
      intercept <- coefs[1]
      eqn_str <- sprintf("%s = %.4f", response, intercept)
      if (length(predictors) > 0) {
        for (i in seq_along(predictors)) {
          coef_val <- coefs[predictors[i]]
          sign <- ifelse(coef_val >= 0, " + ", " - ")
          eqn_str <- paste0(eqn_str, sign, sprintf("%.4f*%s", abs(coef_val), predictors[i]))
        }
      }
      mtext(eqn_str, side = 3, line = 0.5, cex = 1, font = 3)
      dev.off()
      cat(sprintf("ACF plot saved to: %s\n", plot_path))
    }
    plot_residuals_vs_leverage <- function(model, model_name, png_dir) {
      if (is.null(model)) {
        cat("No model provided for residuals vs leverage plot.\n")
        return()
      }
      if (!(inherits(model, "lm") || inherits(model, "glm"))) {
        cat("plot_residuals_vs_leverage only supports lm or glm models.\n")
        return()
      }
      plot_dir <- file.path(png_dir, model_name)
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      plot_path <- file.path(plot_dir, "residuals_vs_leverage.png")
      png(filename = plot_path, width = 800, height = 600)
      leverage <- hatvalues(model)
      resids <- resid(model)
      n <- length(leverage)
      p <- length(coef(model)) - 1
      lev_thresh <- 2 * (p + 1) / n
      plot(leverage, resids,
        xlab = "Leverage (Hat Values)",
        ylab = "Residuals",
        main = sprintf("Residuals vs Leverage: %s", model_name),
        pch = 19, col = "blue"
      )
      abline(h = 0, col = "gray", lty = 2)
      abline(v = lev_thresh, col = "orange", lty = 2)
      legend("topright", legend = sprintf("Orange line: Leverage = %.3f", lev_thresh), col = "orange", lty = 2, bty = "n", cex = 0.9)
      coefs <- coef(model)
      response <- all.vars(formula(model))[1]
      predictors <- names(coefs)[-1]
      intercept <- coefs[1]
      eqn_str <- sprintf("%s = %.4f", response, intercept)
      if (length(predictors) > 0) {
        for (i in seq_along(predictors)) {
          coef_val <- coefs[predictors[i]]
          sign <- ifelse(coef_val >= 0, " + ", " - ")
          eqn_str <- paste0(eqn_str, sign, sprintf("%.4f*%s", abs(coef_val), predictors[i]))
        }
      }
      mtext(eqn_str, side = 3, line = 0.5, cex = 1, font = 3)
      dev.off()
      cat(sprintf("Residuals vs leverage plot saved to: %s\n", plot_path))
    }
    plot_cooks_distance_scatter <- function(model, model_name, png_dir) {
      if (is.null(model)) {
        cat("No model provided for Cook's distance scatter plot.\n")
        return()
      }
      if (!(inherits(model, "lm") || inherits(model, "glm"))) {
        cat("plot_cooks_distance_scatter only supports lm or glm models.\n")
        return()
      }
      plot_dir <- file.path(png_dir, model_name)
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      plot_path <- file.path(plot_dir, "cooks_distance_scatter.png")
      png(filename = plot_path, width = 800, height = 600)
      cooks <- cooks.distance(model)
      plot(seq_along(cooks), cooks,
        xlab = "Observation Index",
        ylab = "Cook's Distance",
        main = sprintf("Cook's Distance Scatter: %s", model_name),
        pch = 19, col = "blue"
      )
      abline(h = 4 / length(cooks), col = "red", lty = 2)
      coefs <- coef(model)
      response <- all.vars(formula(model))[1]
      predictors <- names(coefs)[-1]
      intercept <- coefs[1]
      eqn_str <- sprintf("%s = %.4f", response, intercept)
      if (length(predictors) > 0) {
        for (i in seq_along(predictors)) {
          coef_val <- coefs[predictors[i]]
          sign <- ifelse(coef_val >= 0, " + ", " - ")
          eqn_str <- paste0(eqn_str, sign, sprintf("%.4f*%s", abs(coef_val), predictors[i]))
        }
      }
      mtext(eqn_str, side = 3, line = 0.5, cex = 1, font = 3)
      legend("topright", legend = sprintf("Red line: Cook's D = %.3f", 4 / length(cooks)), col = "red", lty = 2, bty = "n", cex = 0.9)
      dev.off()
      cat(sprintf("Cook's distance scatter plot saved to: %s\n", plot_path))
    }
    plot_cooks_distance_vs_leverage <- function(model, model_name, png_dir) {
      if (is.null(model)) {
        cat("No model provided for Cook's distance vs leverage plot.\n")
        return()
      }
      if (!(inherits(model, "lm") || inherits(model, "glm"))) {
        cat("plot_cooks_distance_vs_leverage only supports lm or glm models.\n")
        return()
      }
      plot_dir <- file.path(png_dir, model_name)
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      plot_path <- file.path(plot_dir, "cooks_distance_vs_leverage.png")
      png(filename = plot_path, width = 800, height = 600)
      leverage <- hatvalues(model)
      cooks <- cooks.distance(model)
      n <- length(leverage)
      p <- length(coef(model)) - 1
      lev_thresh <- 2 * (p + 1) / n
      plot(leverage, cooks,
        xlab = "Leverage (Hat Values)",
        ylab = "Cook's Distance",
        main = sprintf("Cook's Distance vs Leverage: %s", model_name),
        pch = 19, col = "blue"
      )
      coefs <- coef(model)
      response <- all.vars(formula(model))[1]
      predictors <- names(coefs)[-1]
      intercept <- coefs[1]
      eqn_str <- sprintf("%s = %.4f", response, intercept)
      if (length(predictors) > 0) {
        for (i in seq_along(predictors)) {
          coef_val <- coefs[predictors[i]]
          sign <- ifelse(coef_val >= 0, " + ", " - ")
          eqn_str <- paste0(eqn_str, sign, sprintf("%.4f*%s", abs(coef_val), predictors[i]))
        }
      }
      mtext(eqn_str, side = 3, line = 0.5, cex = 1, font = 3)
      abline(h = 4 / length(cooks), col = "red", lty = 2)
      abline(v = lev_thresh, col = "orange", lty = 2)
      text_legend <- c(
        sprintf("Red line: Cook's D = %.3f", 4 / length(cooks)),
        sprintf("Orange line: Leverage = %.3f", lev_thresh)
      )
      legend("topright", legend = text_legend, col = c("red", "orange"), lty = 2, bty = "n", cex = 0.9)
      dev.off()
      cat(sprintf("Cook's distance vs leverage plot saved to: %s\n", plot_path))
    }
    plot_pca_scree <- function(pca, dataset_name, png_dir) {
      if (is.null(pca)) {
        cat("No PCA results to plot.\n")
        return()
      }
      plot_dir <- file.path(png_dir, "GeneralData")
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      plot_path <- file.path(plot_dir, "pca_scree_plot.png")
      png(filename = plot_path, width = 800, height = 600)
      variances <- pca$sdev^2
      prop_var <- variances / sum(variances)
      cum_var <- cumsum(prop_var)
      ylim_max <- max(prop_var) + 0.12
      bp <- barplot(prop_var,
        names.arg = paste0("PC", seq_along(prop_var)),
        main = "PCA Scree Plot",
        xlab = "Principal Component",
        ylab = "Proportion of Variance Explained",
        col = "lightgreen",
        border = NA,
        ylim = c(0, ylim_max)
      )
      lines(x = bp, y = prop_var, type = "b", pch = 19, col = "darkgreen")
      for (i in seq_along(bp)) {
        label <- sprintf("%d%%\nCum: %d%%", round(prop_var[i] * 100), round(cum_var[i] * 100))
        text(x = bp[i], y = prop_var[i] + 0.03, labels = label, cex = 0.8, col = "black")
      }
      thresholds <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
      for (thresh in thresholds) {
        idx <- which(cum_var >= thresh)[1]
        if (!is.na(idx)) {
          x_pos <- bp[idx]
          y_pos <- thresh
          abline(h = thresh, col = "red", lty = 2)
          text(
            x = bp[length(bp)], y = thresh, labels = paste0(round(thresh * 100), "% Cumulative Variance"),
            pos = 4, col = "red", cex = 0.9, font = 2, offset = 0.2
          )
        }
      }
      dev.off()
      cat(sprintf("PCA scree plot saved to: %s\n", plot_path))
    }
    plot_combined_scatter <- function(data, dep_var, predictors, model_name, png_dir) {
      plot_dir <- file.path(png_dir, model_name)
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      plot_path <- file.path(plot_dir, "combined_scatter_plots.png")
      num_predictors <- predictors[sapply(data[predictors], is.numeric)]
      n_pred <- length(num_predictors)
      if (n_pred == 0) {
        cat("No numeric predictors to plot.\n")
        return()
      }
      ncol <- 3
      nrow <- ceiling(n_pred / ncol)
      png(filename = plot_path, width = 300 * ncol, height = 300 * nrow)
      par(mfrow = c(nrow, ncol), mar = c(4, 4, 2, 1), oma = c(0, 0, 4, 0))
      for (xvar in num_predictors) {
        plot(data[[xvar]], data[[dep_var]],
          xlab = xvar, ylab = dep_var,
          main = sprintf("%s vs %s", dep_var, xvar),
          pch = 19, col = "blue"
        )
        xvals <- data[[xvar]]
        yvals <- data[[dep_var]]
        ok <- !is.na(xvals) & !is.na(yvals)
        if (sum(ok) >= 2) {
          fit <- lm(yvals[ok] ~ xvals[ok])
          abline(fit, col = "red", lwd = 2)
        }
      }
      if (n_pred < nrow * ncol) {
        for (i in 1:(nrow * ncol - n_pred)) plot.new()
      }
      main_title <- sprintf("Combined Scatter Plots: %s vs Each Numeric Predictor", dep_var)
      mtext(main_title, outer = TRUE, line = 1, cex = 1.5, font = 2)
      dev.off()
      cat(sprintf("Combined Scatter plots saved to: %s\n", plot_path))
    }
    pca_biplot <- function(pca, dataset_name, png_dir) {
      if (is.null(pca)) {
        cat("No PCA results to plot biplot.\n")
        return()
      }
      plot_dir <- file.path(png_dir, "GeneralData")
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      plot_path <- file.path(plot_dir, "pca_biplot.png")
      png(plot_path, width = 800, height = 800)
      biplot(pca, main = sprintf("PCA Biplot: %s", dataset_name))
      dev.off()
      cat(sprintf("PCA biplot saved to: %s\n", plot_path))
    }
    plot_factor_variance <- function(model, model_name, png_dir) {
      if (is.null(model)) {
        cat("No model provided for factor variance plot.\n")
        return()
      }
      if (!(inherits(model, "lm") || inherits(model, "glm"))) {
        cat("plot_factor_variance only supports lm or glm models.\n")
        return()
      }
      aov_tab <- tryCatch(
        {
          if (inherits(model, "glm")) {
            if (requireNamespace("car", quietly = TRUE)) {
              car::Anova(model, type = 2)
            } else {
              anova(model, test = "Chisq")
            }
          } else {
            anova(model)
          }
        },
        error = function(e) {
          cat("Could not compute ANOVA table for this model.\n")
          return(NULL)
        }
      )
      if (is.null(aov_tab)) {
        return()
      }
      if ("(Intercept)" %in% rownames(aov_tab)) {
        aov_tab <- aov_tab[rownames(aov_tab) != "(Intercept)", , drop = FALSE]
      }
      ss_col <- NULL
      if ("Sum Sq" %in% colnames(aov_tab)) {
        ss_col <- "Sum Sq"
      } else if ("LR Chisq" %in% colnames(aov_tab)) {
        ss_col <- "LR Chisq"
      } else if ("Deviance" %in% colnames(aov_tab)) {
        ss_col <- "Deviance"
      }
      if (is.null(ss_col)) {
        cat("No sum of squares or deviance column found in ANOVA table.\n")
        return()
      }
      ss_vals <- aov_tab[[ss_col]]
      if (nrow(aov_tab) == 0 || all(ss_vals == 0) || sum(ss_vals, na.rm = TRUE) == 0) {
        cat("No valid factors with nonzero variance to plot for this model.\n")
        return()
      }
      percent_var <- ss_vals / sum(ss_vals) * 100
      factor_names <- rownames(aov_tab)
      ord <- order(percent_var, decreasing = TRUE)
      percent_var_sorted <- percent_var[ord]
      factor_names_sorted <- factor_names[ord]
      plot_dir <- file.path(png_dir, model_name)
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      plot_path <- file.path(plot_dir, "factor_variance.png")
      png(filename = plot_path, width = 800, height = 600)
      bp <- barplot(percent_var_sorted,
        names.arg = factor_names_sorted,
        main = sprintf("Percent of Variance by Factor: %s", model_name),
        xlab = "Factor",
        ylab = "Percent of Variance Explained",
        col = "forestgreen",
        border = NA,
        ylim = c(0, max(percent_var_sorted) + 10),
        las = 1
      )
      cum_var <- cumsum(percent_var_sorted)
      for (i in seq_along(bp)) {
        label <- sprintf("%d%%\nCum: %d%%", round(percent_var_sorted[i]), round(cum_var[i]))
        text(x = bp[i], y = percent_var_sorted[i] + 2, labels = label, cex = 0.8, col = "black")
      }
      thresholds <- seq(10, 100, by = 10)
      for (thresh in thresholds) {
        abline(h = thresh, col = "red", lty = 2)
        text(
          x = bp[length(bp)], y = thresh, labels = paste0(thresh, "%"),
          pos = 4, col = "red", cex = 0.8, font = 2, offset = 0.2
        )
      }
      coefs <- coef(model)
      response <- all.vars(formula(model))[1]
      predictors <- names(coefs)[-1]
      intercept <- coefs[1]
      eqn_str <- sprintf("%s = %.4f", response, intercept)
      if (length(predictors) > 0) {
        for (i in seq_along(predictors)) {
          coef_val <- coefs[predictors[i]]
          sign <- ifelse(coef_val >= 0, " + ", " - ")
          eqn_str <- paste0(eqn_str, sign, sprintf("%.4f*%s", abs(coef_val), predictors[i]))
        }
      }
      mtext(eqn_str, side = 3, line = 0.5, cex = 1, font = 3)
      dev.off()
      cat(sprintf("Factor variance plot saved to: %s\n", plot_path))
    }
    plot_residuals_vs_fitted <- function(model, model_name, png_dir) {
      plot_dir <- file.path(png_dir, model_name)
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      plot_path <- file.path(plot_dir, "residuals_vs_fitted_plot.png")
      png(filename = plot_path, width = 800, height = 600)
      plot(fitted(model), resid(model),
        xlab = "Fitted Values",
        ylab = "Residuals",
        main = sprintf("Residuals vs Fitted: %s", model_name),
        pch = 19, col = "darkred"
      )
      coefs <- coef(model)
      response <- all.vars(formula(model))[1]
      predictors <- names(coefs)[-1]
      intercept <- coefs[1]
      eqn_str <- sprintf("%s = %.4f", response, intercept)
      if (length(predictors) > 0) {
        for (i in seq_along(predictors)) {
          coef_val <- coefs[predictors[i]]
          sign <- ifelse(coef_val >= 0, " + ", " - ")
          eqn_str <- paste0(eqn_str, sign, sprintf("%.4f*%s", abs(coef_val), predictors[i]))
        }
      }
      mtext(eqn_str, side = 3, line = 0.5, cex = 1, font = 3)
      abline(h = 0, lty = 2, col = "gray")
      dev.off()
      cat(sprintf("Residuals vs Fitted plot saved to: %s\n", plot_path))
    }
    plot_residuals_qq <- function(model, model_name, png_dir) {
      plot_dir <- file.path(png_dir, model_name)
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      plot_path <- file.path(plot_dir, "residuals_qq_plot.png")
      png(filename = plot_path, width = 800, height = 600)
      qqnorm(resid(model), main = sprintf("QQ-Plot of Residuals: %s", model_name), pch = 19, col = "purple")
      qqline(resid(model), col = "red", lwd = 2)
      coefs <- coef(model)
      response <- all.vars(formula(model))[1]
      predictors <- names(coefs)[-1]
      intercept <- coefs[1]
      eqn_str <- sprintf("%s = %.4f", response, intercept)
      if (length(predictors) > 0) {
        for (i in seq_along(predictors)) {
          coef_val <- coefs[predictors[i]]
          sign <- ifelse(coef_val >= 0, " + ", " - ")
          eqn_str <- paste0(eqn_str, sign, sprintf("%.4f*%s", abs(coef_val), predictors[i]))
        }
      }
      mtext(eqn_str, side = 3, line = 0.5, cex = 1, font = 3)
      dev.off()
      cat(sprintf("Residuals QQ-plot saved to: %s\n", plot_path))
    }
    plot_residuals_histogram <- function(model, model_name, png_dir) {
      plot_dir <- file.path(png_dir, model_name)
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      plot_path <- file.path(plot_dir, "residuals_histogram.png")
      png(filename = plot_path, width = 800, height = 600)
      resids <- resid(model)
      xfit <- seq(min(resids, na.rm = TRUE), max(resids, na.rm = TRUE), length = 200)
      yfit <- dnorm(xfit, mean = mean(resids, na.rm = TRUE), sd = sd(resids, na.rm = TRUE))
      ymax <- max(c(hist(resids, breaks = "FD", plot = FALSE, freq = FALSE)$density, yfit), na.rm = TRUE) * 1.08
      h <- hist(resids,
        breaks = "FD",
        main = sprintf("Histogram of Residuals: %s", model_name),
        xlab = "Residuals",
        col = "skyblue",
        border = "white",
        freq = FALSE,
        ylim = c(0, ymax)
      )
      lines(xfit, yfit, col = "red", lwd = 2)
      coefs <- coef(model)
      response <- all.vars(formula(model))[1]
      predictors <- names(coefs)[-1]
      intercept <- coefs[1]
      eqn_str <- sprintf("%s = %.4f", response, intercept)
      if (length(predictors) > 0) {
        for (i in seq_along(predictors)) {
          coef_val <- coefs[predictors[i]]
          sign <- ifelse(coef_val >= 0, " + ", " - ")
          eqn_str <- paste0(eqn_str, sign, sprintf("%.4f*%s", abs(coef_val), predictors[i]))
        }
      }
      mtext(eqn_str, side = 3, line = 0.5, cex = 1, font = 3)
      dev.off()
      cat(sprintf("Residuals histogram saved to: %s\n", plot_path))
    }
    plot_residual_vs_predictor <- function(model, model_name, png_dir) {
      if (is.null(model)) {
        cat("No model provided for residual vs predictor plot.\n")
        return()
      }
      if (!(inherits(model, "lm") || inherits(model, "glm"))) {
        cat("plot_residual_vs_predictor only supports lm or glm models.\n")
        return()
      }
      predictors <- names(coef(model))[-1]
      data <- model.frame(model)
      resids <- resid(model)
      n_pred <- length(predictors)
      if (n_pred == 0) {
        cat("No predictors to plot.\n")
        return()
      }
      ncol <- 3
      nrow <- ceiling(n_pred / ncol)
      plot_dir <- file.path(png_dir, model_name)
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      plot_path <- file.path(plot_dir, "residual_vs_predictor.png")
      png(filename = plot_path, width = 300 * ncol, height = 300 * nrow)
      par(mfrow = c(nrow, ncol), mar = c(4, 4, 2, 1), oma = c(0, 0, 4, 0))
      for (xvar in predictors) {
        xvals <- data[[xvar]]
        plot(xvals, resids,
          xlab = xvar, ylab = "Residuals",
          main = sprintf("Residuals vs %s", xvar),
          pch = 19, col = "blue"
        )
        abline(h = 0, col = "gray", lty = 2)
      }
      if (n_pred < nrow * ncol) {
        for (i in 1:(nrow * ncol - n_pred)) plot.new()
      }
      main_title <- sprintf("Residuals vs Each Predictor: %s", model_name)
      mtext(main_title, outer = TRUE, line = 1, cex = 1.5, font = 2)
      dev.off()
      cat(sprintf("Residuals vs predictor plots saved to: %s\n", plot_path))
    }
  }
  plot_all <- function(model, model_name, png_dir) {
    # these functions are ordered according to a logical flow of analysis
    plot_residuals_vs_fitted(model, model_name, png_dir)
    plot_residuals_histogram(model, model_name, png_dir)
    plot_residual_vs_predictor(model, model_name, png_dir)
    plot_residuals_qq(model, model_name, png_dir)
    plot_residuals_vs_leverage(model, model_name, png_dir)
    plot_acf(model, model_name, png_dir)
    plot_factor_variance(model, model_name, png_dir)
    plot_cooks_distance_scatter(model, model_name, png_dir)
    plot_cooks_distance_vs_leverage(model, model_name, png_dir)
  }
  # General Data Analyses ----
  cat("#####################################################################################\n")
  cat("General Data Anlyses\n")
  cat("#####################################################################################\n")
  analyze_colinearity(full_data, "Correlation Matrix of Finalized Data")
  plot_combined_scatter(full_data, dep_var, predictors, model_name = "GeneralData", png_dir = png_dir)
  pca_results <- analyze_principal_components(full_data, predictors, "Principal Component Analysis of Predictor Variables")
  plot_pca_scree(pca_results, dataset_name = full_dataname, png_dir = png_dir)
  pca_biplot(pca_results, dataset_name = full_dataname, png_dir = png_dir)
  # Analysis of Specified Models ----
  cat("#####################################################################################\n")
  cat("Analysis of Specified Models\n")
  cat("#####################################################################################\n")
  # Model 1: Linear model
  cat("\n--- Model 1: Linear Model ---\n\n")
  model_name <- "Model 1: Linear Model"
  model_dir <- "Model1"
  Model1 <- lm(Species ~ Area + Elevation + Nearest + I(Scruz + 0.4) + Adjacent, data = gala[, -2])
  print(summary(Model1))
  analyze_all(Model1, model_name, sig)
  plot_all(Model1, model_dir, png_dir)
  cat("\n")
  # Model 2: Linear model with log-transformed predictors
  cat("\n--- Model 2: Linear Model with Log Transformations ---\n\n")
  model_name <- "Model 2: Linear Model with Log Transformations"
  model_dir <- "Model2"
  Model2 <- lm(Species ~ log(Area) + log(Elevation) + log(Nearest) + I(log(Scruz + 0.4)) + log(Adjacent), data = gala[, -2])
  print(summary(Model2))
  analyze_all(Model2, model_name, sig)
  plot_all(Model2, model_dir, png_dir)
  cat("\n")
  # Model 3: Linear model with sqrt-transformed DV and log-transformed predictors
  cat("\n--- Model 3: Sqrt-transformed DV and Log Predictors ---\n\n")
  model_name <- "Model 3: Sqrt-transformed DV and Log Predictors"
  model_dir <- "Model3"
  Model3 <- lm(sqrt(Species) ~ log(Area) + log(Elevation) + log(Nearest) + I(log(Scruz + 0.4)) + log(Adjacent), data = gala[, -2])
  print(summary(Model3))
  analyze_all(Model3, model_name, sig)
  plot_all(Model3, model_dir, png_dir)
  cat("\n")
  # Model 4: Poisson GLM
  cat("\n--- Model 4: Poisson GLM ---\n\n")
  model_name <- "Model 4: Poisson GLM"
  model_dir <- "Model4"
  Model4 <- glm(Species ~ Area + Elevation + Nearest + I(Scruz + 0.4) + Adjacent, data = gala, family = poisson)
  print(summary(Model4))
  analyze_all(Model4, model_name, sig)
  plot_all(Model4, model_dir, png_dir)
  cat("\n")
  # Model 5: Poisson GLM with log-transformed predictors
  cat("\n--- Model 5: Poisson GLM with Log Predictors ---\n\n")
  model_name <- "Model 5: Poisson GLM with Log Predictors"
  model_dir <- "Model5"
  Model5 <- glm(Species ~ log(Area) + log(Elevation) + log(Nearest) + I(log(Scruz + 0.4)) + log(Adjacent), data = gala, family = poisson)
  print(summary(Model5))
  analyze_all(Model5, model_name, sig)
  plot_all(Model5, model_dir, png_dir)
  cat("\n")
}, error = function(e) {
  cat("Error: ", e$message, "\n")
}, finally = {
  cat("\n\n")
  sink() # close the output file
})
