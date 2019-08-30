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
  data[, j] = data[selected_values, j]
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
    bind_cols(new_data) %>%
    metric(truth = !!ensym(truth), estimate = .pred)
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
#' @param model
#' @param new_data
#' @param metric
#'
#' @return
#' @export
#'
#' @examples
permutation_importance <- function(model, new_data, truth, metric, n = 1) {
  base_performance <- calculate_metric(model, new_data, !!ensym(truth), metric)

  # Remove the outcome column
  without_outcome <- new_data %>%
    select(-!!enquo(truth))
  m <- ncol(without_outcome)
  importance <- matrix(NA_real_, nrow = m, ncol = n)

  # for each column in the predictors shuffle and calculate the metric
  for (j in seq_len(m)) {
    for (i in seq_len(n)) {
      shuffled_data <- bind_cols(shuffle_column(without_outcome, j), new_data %>% select(!!enquo(truth)))
      new_performance <- calculate_metric(model, shuffled_data, !!ensym(truth), metric)
      importance[j, i] <- calculate_importance(base_performance, new_performance)
    }
  }

  if (n == 1) {
    tibble(
      feature = colnames(without_outcome),
      importance = importance[, 1]
    )
  } else {
    tibble(
      feature = colnames(without_outcome),
      mean_importance = apply(importance, 1, mean),
      lower_importance = apply(importance, 1, quantile, probs = 0.05),
      upper_importance = apply(importance, 1, quantile, probs = 0.95)
    )
  }

}

#' Title
#'
#' @param importance
#'
#' @return
#' @export
#'
#' @examples
plot_importance <- function(importance) {
  importance %>%
    mutate(feature = forcats::fct_reorder(feature, -importance)) %>%
    ggplot(aes(x = feature, weight = importance)) +
    geom_bar()

}
