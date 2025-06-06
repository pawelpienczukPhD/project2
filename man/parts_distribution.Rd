% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun2.R
\name{parts_distribution}
\alias{parts_distribution}
\title{example:
no_parts=13
batch=500
missing_rate=1.3}
\usage{
parts_distribution(no_parts, batch, missing_rate)
}
\arguments{
\item{no_parts}{Number of parts types (must be an integer).}

\item{batch}{Total number of parts in a batch (must be an integer).}

\item{missing_rate}{Missing rate (must be >= 1).}
}
\value{
List containing:
\itemize{
\item plot: ggplot2 plot object with three rows and a legend.
\item data_frame: Data frame with calculation results (theoretical, total, technological).
\item sum_in_batch: The sum of the batches in the technological distribution.
}
}
\description{
Function that calculates and visualizes the distribution of part variants in a batch.
For instance, one dimension is subject to change in order to ensure set clearance at the same level across all machines.
Returns a ggplot2 plot, a data frame with results, and the total in a process batch.
}
\examples{
# Example of use:
# results_parts <- parts_distribution(no_parts = 20, batch = 1000, missing_rate = 1.05)
# print(results_parts$plot)
# print(results_parts$data_frame)
# print(results_parts$sum_in_batch)


Input parameters validation according to requirements:
This is an essential part of the code:
The distribution of parts was calculated by approximating the density of the normal distribution
using an eighth-degree polynomial, with a given number of parts in the series substituted for.

In light of the substitution, it was necessary to implement a correction factor.
This factor yielded a summation of the density of the distribution that approximated 1.

The algorithm distinguishes between even and odd cases

Technological distribution - integer and technological modulo use to prevent 0 pieces in some variants:
Creating of data frame and plot using ggplot2 to show differences between results:
Configure the X-axis scale: breaks correspond to each part number.
Configure the Y-axis scale: breaks are set based on the maximum value in the reshaped data.
Returning of the results:
}
