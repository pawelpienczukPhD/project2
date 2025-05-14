# Funnction 2:
library(ggplot2)
library(dplyr)
library(broom)
library(scales)
library(testthat)

#' Analyzes results from the `parts_distribution' function, calculating descriptive statistics,
#' normality tests, fitting polynomial models, calculating probabilities
#' for the technological distribution, and modifying the original graph by adding a
#' fitting to the normal distribution for the technological data.
#'

analyze_parts_distribution <- function(parts_results, poly_degree = 3) {
  #' Input parameters validation according to requirements:
  stopifnot(
    "Argument `parts_results` musi być listą." = is.list(parts_results),
    "Lista `parts_results` musi zawierać element `data_frame`." = "data_frame" %in% names(parts_results),
    "Element `data_frame` w `parts_results` musi być ramką danych." = is.data.frame(parts_results$data_frame),
    "Lista `parts_results` musi zawierać element `plot`." = "plot" %in% names(parts_results),
    "Element `plot` w `parts_results` musi być obiektem ggplot." = inherits(parts_results$plot, "ggplot"),
    "Ramka danych musi zawierać kolumny `part_number`, `theorethic`, `integer`, `technological`." =
      all(c("part_number", "theorethic", "integer", "technological") %in% colnames(parts_results$data_frame))
  )
  # Download input data for analysis
  df <- parts_results$data_frame

  # Placeholder for polynomial fitting
  fitted_models_list <- list()

  cols_to_analyze <- c("theorethic", "integer", "technological")

  # Initialize data frames for results
  summary_stats_df <- data.frame(Statistic = c("Mean", "Median", "SD", "Min", "Max", "Sum"))
  normality_analysis_df <- data.frame(Measure = c("Shapiro-Wilk Statistic", "Shapiro-Wilk P-value", "Method / Reason"))


  # Loop for each column
  for (col_name in cols_to_analyze) {
    data_vector <- df[[col_name]]
    valid_data_vector <- data_vector[!is.na(data_vector)]

    # 1. Basic statistics
    if (length(valid_data_vector) > 0) {
      summary_stats_df[[col_name]] <- c(
        mean(valid_data_vector),
        median(valid_data_vector),
        sd(valid_data_vector),
        min(valid_data_vector),
        max(valid_data_vector),
        sum(valid_data_vector)
      )
    } else {
      summary_stats_df[[col_name]] <- NA_real_
      summary_stats_df[summary_stats_df$Statistic == "Sum", col_name] <- 0
      warning(paste("No valid data for stats:", col_name))
    }

    # 2. Shapiro-Wilk test
    shapiro_stat <- NA_real_
    shapiro_p_value <- NA_real_
    shapiro_method <- "Shapiro-Wilk (skipped)"

    if (length(valid_data_vector) >= 3 && length(valid_data_vector) <= 5000 && length(unique(valid_data_vector)) > 1) {
      shapiro_result <- tryCatch(
        shapiro.test(valid_data_vector),
        error = function(e) {
          list(statistic = NA_real_, p.value = NA_real_, method = paste("Shapiro-Wilk (error:", e$message, ")"))
        }
      )
      shapiro_stat <- shapiro_result$statistic
      shapiro_p_value <- shapiro_result$p.value
      shapiro_method <- shapiro_result$method
    }

    # Add normality results as a column to the data frame
    normality_analysis_df[[col_name]] <- c(shapiro_stat, shapiro_p_value, shapiro_method)

  } # End column loop


  # 3. Polynomial model (placeholder)

  # 4. Technological probabilities
  total_technological <- sum(df$technological, na.rm = TRUE)
  if (total_technological > 0 && !is.na(total_technological)) {
    technological_probabilities_df <- df %>%
      select(part_number, technological) %>%
      mutate(probability = ifelse(is.na(technological) | technological == 0, 0, technological / total_technological))
  } else {
    technological_probabilities_df <- df %>%
      select(part_number, technological) %>%
      mutate(probability = NA_real_)
    if (nrow(df) > 0) {
      warning("Technological probabilities cannot be calculated.")
    }
  }

  # 5. Modify plot: add normal fit for technological data
  # Recreate plot layers with color aesthetics for unified legend
  plot_with_normal_fit <- ggplot(df, aes(x = part_number)) +
    geom_line(aes(y = theorethic, color = "Theoretical")) +
    geom_line(aes(y = integer, color = "Integer (rounded)")) +
    geom_line(aes(y = technological, color = "Technological")) +
    # Configure axes
    scale_x_continuous(breaks = seq(1, max(df$part_number, na.rm = TRUE), by = 1)) +
    scale_y_continuous(breaks = seq(0, max(df$technological, na.rm = TRUE), by = max(1, floor(max(df$technological, na.rm = TRUE) / 10)))) +
    labs(
      x = "Number of parts",
      y = "Part quantity",
      title = "Distribution of batch with Normal Fit"
    )

  # Add normal fit for technological data if possible
  # Get stats from the summary_stats_df table
  mean_tech <- summary_stats_df %>% filter(Statistic == "Mean") %>% pull(technological)
  sd_tech <- summary_stats_df %>% filter(Statistic == "SD") %>% pull(technological)
  sum_tech <- summary_stats_df %>% filter(Statistic == "Sum") %>% pull(technological)


  # Check for valid normal distribution parameters
  if (!is.na(mean_tech) && !is.na(sd_tech) && sd_tech > 0 && !is.na(sum_tech) && sum_tech > 0 && length(unique(df$technological[!is.na(df$technological)])) > 1) {

    # Add normal density curve (scaled)
    plot_with_normal_fit <- plot_with_normal_fit +
      stat_function(
        fun = function(x, mean, sd, n, binwidth) {
          dnorm(x, mean = mean, sd = sd) * n * binwidth
        },
        args = list(mean = mean_tech, sd = sd_tech, n = sum_tech, binwidth = 1),
        aes(color = "Normal Fit (Technological)"),
        linetype = "dashed",
        linewidth = 1
      ) +
      # Update color scale for all elements
      scale_color_manual(
        values = c(
          "Theoretical" = "grey50",
          "Total (rounded)" = "blue",
          "Technological" = "red",
          "Normal Fit (Technological)" = "purple"
        ),
        name = "Distribution Type / Fitting:"
      )

  } else {
    warning("Normal fit not added for 'technological'. Insufficient data or zero SD/sum.")
    # Define color scale for original lines if normal fit is skipped
    plot_with_normal_fit <- plot_with_normal_fit +
      scale_color_manual(
        values = c(
          "Theoretical" = "grey50",
          "Total (rounded)" = "blue",
          "Technological" = "red"
        ),
        name = "Distribution Type:"
      )
  }

  # Prepare the list to be returned
  result_list <- list(
    summary_stats = summary_stats_df,
    normality_analysis = normality_analysis_df,
    fitted_models_summary = fitted_models_list,
    technological_probabilities = technological_probabilities_df,
    plot_with_normal_fit = plot_with_normal_fit
  )

  # Add attributes to the result list
  attr(result_list, "timestamp") <- Sys.time()
  attr(result_list, "generator_function") <- "analyze_parts_distribution"

  # Return the list with attributes
  return(result_list)
}


test_that ("analyze_parts_distribution returns expected output structure and types", {
  parts_results_input <- parts_distribution(no_parts = 10, batch = 100, missing_rate = 1.1)
  results_analysis <- analyze_parts_distribution(parts_results_input)
  expect_type(results_analysis, "list")
  expected_names <- c("summary_stats", "normality_analysis", "fitted_models_summary", "technological_probabilities", "plot_with_normal_fit")
  expect_true(all(expected_names %in% names(results_analysis)))
  expect_s3_class(results_analysis$summary_stats, "data.frame")
  expect_s3_class(results_analysis$plot_with_normal_fit, "ggplot")
})


test1 <- parts_distribution(29,1500,1.2)
results <- analyze_parts_distribution(test1)
results
