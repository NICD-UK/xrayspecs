#' Title
#'
#' @param data
#' @param j
#'
#' @return
#' @export
#'
#' @examples
shuffle_column <- function(data, j) {
  n <- nrow(data)
  selected_values <- sample.int(n = n, replace = F, size = n)
  data[, j] <- data[selected_values, j]
  data
}

#' Title
#'
#' @param model
#' @param new_data
#' @param truth
#' @param metric
#'
#' @return
#' @export
#'
#' @examples
calculate_metric <- function(model, new_data, truth, metric) {
  predict(model, new_data = new_data) %>%
    dplyr::bind_cols(new_data) %>%
    metric(truth = !!rlang::ensym(truth), estimate = .pred)
}

#' Title
#'
#' @param base_perf
#' @param new_perf
#'
#' @return
#' @export
#'
#' @examples
calculate_importance <- function(base_perf, new_perf) {
  if (base_perf$.metric %in% c("rmse", "huber_loss_pseudo", "huber_loss", "mae", "mape", "mase", "smape")) {
    new_perf$.estimate - base_perf$.estimate
  } else {
    base_perf$.estimate - new_perf$.estimate
  }
}

#' Run Permutation Importance
#'
#' Permutation importance works by first performing
#' predictions using the pre-trained model and `new_data` then calculcating a base `metric`.
#' Next, a single feature column of `new_data` is randomly shuffled,
#' predictions are calculated again and the `metric` is calculated.
#' The difference between the base metric and the metric using the each shuffled column is
#' calculated. The larger the difference, the more the feature has an impact.
#'
#' @param model a  model generated using parsnip
#' @param new_data new data to use for predictions
#' @param truth the outcome column
#' @param metric a metric from the yarstick package suitable for measuring the
#' performance of the model
#' @param n repeat the permutations `n` times to account for random sampling error
#'
#' @return a dataframe containing the change in metric for each feature
#' @export
#'
#' @examples
#' model <- parsnip::linear_reg(mixture = 0, penalty = 0.1) %>%
#'   parsnip::set_engine("lm") %>%
#'   parsnip::fit(mpg ~ ., data = mtcars)
#'
#' permutation_importance(model, mtcars, mpg, yardstick::rmse)
permutation_importance <- function(model, new_data, truth, metric, n = 1) {
  base_performance <- calculate_metric(model, new_data, !!rlang::ensym(truth), metric)

  # Remove the outcome column
  without_outcome <- new_data %>%
    dplyr::select(-!!enquo(truth))
  m <- ncol(without_outcome)
  importance <- matrix(NA_real_, nrow = m, ncol = n)

  # for each column in the predictors shuffle and calculate the metric
  for (j in seq_len(m)) {
    importance[j, ] <- replicate(n, expr = {
      shuffled_data <- dplyr::bind_cols(shuffle_column(without_outcome, j),
                                 new_data %>% dplyr::select(!!enquo(truth)))
      new_performance <- calculate_metric(model, shuffled_data, !!rlang::ensym(truth), metric)
      calculate_importance(base_performance, new_performance)
    })
  }

  if (n == 1) {
    x <- list(df = tibble::tibble(
      feature = colnames(without_outcome),
      importance = importance[, 1]
    ))
  } else {
    x <- list(df = tibble::tibble(
      feature = colnames(without_outcome),
      mean_importance = apply(importance, 1, mean),
      lower_importance = apply(importance, 1, quantile, probs = 0.05),
      upper_importance = apply(importance, 1, quantile, probs = 0.95)
    ))
  }

  class(x) <- "importance"
  return(x)
}

#' Plot the importance of each feature
#'
#' @param importance
#'
#' @return
#' @export
#'
#' @examples
plot.importance <- function(importance) {
  if (ncol(importance$df) == 4) {
    importance$df %>%
      dplyr::mutate(feature = forcats::fct_reorder(feature, -mean_importance)) %>%
      ggplot2::ggplot(ggplot2::aes(x = feature, y = mean_importance)) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbarh(aes(xmin = lower_importance, xmax = upper_importance))
  } else {
    importance$df %>%
      dplyr::mutate(feature = forcats::fct_reorder(feature, importance)) %>%
      ggplot2::ggplot(ggplot2::aes(x = feature, weight = importance)) +
      ggplot2::geom_bar() +
      ggplot2::coord_flip()
  }
}
