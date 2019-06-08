#' Test simulation environment for the SEPM package
#'
#' This package provides functions for testing the \code{\link{SEPM}} package
#' which is concerned with
#' the Statistical Evaluation of Prediction Models. Two main functions are exported, one for data
#' generation and model fitting and one to emulate the process of conducting a model evaluation study.
#' The primary goal is the assessment of characteristics (final model performance, estimation bias,
#' type 1 and 2 error rates) of different model evaluation strategies
#' under realistic conditions.
#'
#' @section Data generation:
#' The function \code{\link{generate_data}} generate all required datasets (learning, evaluation, population),
#' train prediction models and calculates the "true" performances of these models.
#'
#' @section Study execution:
#' The function \code{\link{study_accuracy}} takes the output of \code{\link{generate_data}} and conducts
#' a model evaluation study for the performance measure accuracy. This is done after selection suitable
#' models via a selection rule.
#'
#' @section Author(s):
#' \strong{Maintainer:} XXX YYY \email{XXX.YYY@ZZZ.com}
#'
#' @section Reference:
#' .....
#'
#' @section Repository:
#' XYZ
#'
#' @docType package
#' @name SEPM.MLE
NULL
