library(datasets)
library(ggplot2)
library(tidyr)
library(dplyr)

#' example:
#' no_parts=13
#' batch=500
#' missing_rate=1.3
#'
#' Function that calculates and visualizes the distribution of part variants in a batch.
#' For instance, one dimension is subject to change in order to ensure set clearance at the same level across all machines.
#' Returns a ggplot2 plot, a data frame with results, and the total in a process batch.
#'
#' @param no_parts Number of parts types (must be an integer).
#' @param batch Total number of parts in a batch (must be an integer).
#' @param missing_rate Missing rate (must be >= 1).
#' @return List containing:
#' - plot: ggplot2 plot object with three rows and a legend.
#' - data_frame: Data frame with calculation results (theoretical, total, technological).
#' - sum_in_batch: The sum of the batches in the technological distribution.
#'
#' @examples
#' # Example of use:
#' # results_parts <- parts_distribution(no_parts = 20, batch = 1000, missing_rate = 1.05)
#' # print(results_parts$plot)
#' # print(results_parts$data_frame)
#' # print(results_parts$sum_in_batch)
#'
#'
parts_distribution <- function (no_parts, batch, missing_rate){

  #' Input parameters validation according to requirements:
  stopifnot(
    "Błąd: Liczba części w szeregu 'no_parts' musi być pojedynczą, dodatnią liczbą całkowitą." =
      is.numeric(no_parts) && length(no_parts) == 1 && !is.na(no_parts) &&
      floor(no_parts) == no_parts && no_parts > 0,

    "Błąd: Wielkość partii części 'batch' musi być pojedynczą, dodatnią liczbą całkowitą." =
      is.numeric(batch) && length(batch) == 1 && !is.na(batch) &&
      floor(batch) == batch && batch > 0,

    "Błąd: Współczynnik braków 'missing_rate' musi być pojedynczą wartością numeryczną >= 1." =
      is.numeric(missing_rate) && length(missing_rate) == 1 && !is.na(missing_rate) &&
      missing_rate >= 1
  )

  #' This is an essential part of the code:
  #' The distribution of parts was calculated by approximating the density of the normal distribution
  #' using an eighth-degree polynomial, with a given number of parts in the series substituted for.
  #'
  #' In light of the substitution, it was necessary to implement a correction factor.
  #' This factor yielded a summation of the density of the distribution that approximated 1.
  #'
  #'The algorithm distinguishes between even and odd cases
  if(no_parts%%2==0){
    range <- c((-no_parts/2):(no_parts/2))
    range <- range[-c(1+(no_parts/2))]
    range <-range*(3/(no_parts/2))
    correction_coef <- (0.3975 - 0.1897 * range^2 + 0.0392 * range^4 - 0.00393 * range^6 + 0.000153 * range^8)
    fill <- batch * missing_rate * (0.3975 - 0.1897 * range^2 + 0.0392 * range^4 - 0.00393 * range^6 + 0.000153 * range^8)/sum(correction_coef)
    tmp_floor <- floor(fill)
  }
  else{
    range <-c(ceiling(-no_parts/2):floor(no_parts/2))
    range <-range*(3/(floor(no_parts/2)))
    correction_coef <- (0.3975 - 0.1897 * range^2 + 0.0392 * range^4 - 0.00393 * range^6 + 0.000153 * range^8)
    fill <- batch * missing_rate * (0.3975 - 0.1897 * range^2 + 0.0392 * range^4 - 0.00393 * range^6 + 0.000153 * range^8)/sum(correction_coef)
    tmp_floor <- floor(fill)
  }

  #'
  #' Technological distribution - integer and technological modulo use to prevent 0 pieces in some variants:
  distro <-tmp_floor
  distro[distro<2] <-2
  indexes_by5 <- which(distro>13)
  distro[indexes_by5] <- ceiling(distro[indexes_by5]/5)*5

  #' Creating of data frame and plot using ggplot2 to show differences between results:
  df1 <- data.frame (part_number=c(1:no_parts), theorethic = fill, integer = tmp_floor, technological = distro)
  g <- ggplot (df1, aes (x = part_number)) +
    geom_line(aes (y=theorethic), color="grey") +
    geom_line(aes (y=integer), color="blue") +
    geom_line(aes (y=technological), color="red") +
    #' Configure the X-axis scale: breaks correspond to each part number.
    scale_x_continuous(breaks = seq(1,no_parts,by=1))+
    #' Configure the Y-axis scale: breaks are set based on the maximum value in the reshaped data.
    scale_y_continuous(breaks = seq(0,max(df1$technological),by=(floor(max(df1$technological)/10))))+
    labs(x="Number of parts",
         y="Part number",
         title = "Distibution of batch")
  #' Returning of the results:
  return(list(plot = g, data_frame = df1, sum_in_batch = sum(df1$technological)))
}


# test_that("Input validation", {
#   # Testy błędów dla `no_parts`
#   expect_error(parts_distribution("a", 100, 1.1), "Błąd: Liczba części w szeregu 'no_parts' musi być pojedynczą, dodatnią liczbą całkowitą.")
#   expect_error(parts_distribution(c(10, 20), 100, 1.1), "Błąd: Liczba części w szeregu 'no_parts' musi być pojedynczą, dodatnią liczbą całkowitą.")
#   expect_error(parts_distribution(10.5, 100, 1.1), "Błąd: Liczba części w szeregu 'no_parts' musi być pojedynczą, dodatnią liczbą całkowitą.")
#   expect_error(parts_distribution(0, 100, 1.1), "Błąd: Liczba części w szeregu 'no_parts' musi być pojedynczą, dodatnią liczbą całkowitą.")
#   expect_error(parts_distribution(-5, 100, 1.1), "Błąd: Liczba części w szeregu 'no_parts' musi być pojedynczą, dodatnią liczbą całkowitą.")
#
#   # Testy błędów dla `batch`
#   expect_error(parts_distribution(10, "a", 1.1), "Błąd: Wielkość partii części 'batch' musi być pojedynczą, dodatnią liczbą całkowitą.")
#   expect_error(parts_distribution(10, c(100, 200), 1.1), "Błąd: Wielkość partii części 'batch' musi być pojedynczą, dodatnią liczbą całkowitą.")
#   expect_error(parts_distribution(10, 100.5, 1.1), "Błąd: Wielkość partii części 'batch' musi być pojedynczą, dodatnią liczbą całkowitą.")
#   expect_error(parts_distribution(10, 0, 1.1), "Błąd: Wielkość partii części 'batch' musi być pojedynczą, dodatnią liczbą całkowitą.")
#   expect_error(parts_distribution(10, -100, 1.1), "Błąd: Wielkość partii części 'batch' musi być pojedynczą, dodatnią liczbą całkowitą.")
#
#   # Testy błędów dla `missing_rate`
#   expect_error(parts_distribution(10, 100, "1.1"), "Błąd: Współczynnik braków 'missing_rate' musi być pojedynczą wartością numeryczną >= 1.")
#   expect_error(parts_distribution(10, 100, c(1.1, 1.2)), "Błąd: Współczynnik braków 'missing_rate' musi być pojedynczą wartością numeryczną >= 1.")
#   expect_error(parts_distribution(10, 100, 0.99), "Błąd: Współczynnik braków 'missing_rate' musi być pojedynczą wartością numeryczną >= 1.")
#   expect_error(parts_distribution(10, 100, -1), "Błąd: Współczynnik braków 'missing_rate' musi być pojedynczą wartością numeryczną >= 1.")
#
#   #test klas/typów danych
#   result_even <- parts_distribution(no_parts = 6, batch = 120, missing_rate = 1.1)
#   expect_type(result_even, "list")
#   expect_s3_class(result_even$plot, "ggplot")
#   expect_s3_class(result_even$data_frame, "data.frame")
#   expect_type(result_even$sum_in_batch, "double")
#
#
#   #test dla wartości granicznych (przewidywalność)
#   n_parts <- 10
#   batch_size <- 500
#   m_rate <- 1.05
#   result <- parts_distribution(no_parts = n_parts, batch = batch_size, missing_rate = m_rate)
#   df <- result$data_frame
#   sum_batch <- result$sum_in_batch
#   expect_equal(sum_batch, sum(df$technological))
#   expect_gte(sum_batch, batch_size)
#
#   # Suma teoretyczna powinna być bliska batch * missing_rate (z dokładnością do błędów numerycznych),
#   # w przeciwnym razie zaokrąglenia przestają mieć sens (wyjściowe sum_in_batch również mogło zostać użyte)
#   expect_equal(sum(df$theorethic), batch_size * m_rate, tolerance = 1e-9)
# })
