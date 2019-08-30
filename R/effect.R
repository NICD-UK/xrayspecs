#' Feature sequence
#'
#' @param feature
#' @param length
#'
#' @return
feature_seq <- function(feature, length = 100) {
  if (typeof(feature) == "double") {
    seq(min(feature), max(feature), length.out = length)
  } else {
    factor(levels(feature))
  }
}

#' Feature replace
#'
#' @param data
#' @param feature_name
#' @param feature_value
#'
#' @return
feature_replace <- function(data, feature_name, feature_value) {
  dplyr::mutate(data, !!ensym(feature_name) := feature_value)
}

#' Mean predict
#'
#' @param object
#' @param data
#'
#' @return
mean_predict <- function(object, data) {
  predict(object, data) %>%
    dplyr::summarise(.mean_pred = mean(.pred))
}

#' Effect
#'
#' @param object
#' @param new_data
#' @param feature_name
#'
#' @return
#' @export
effect <- function(object, data, feature_name) {
  feature_name <- ensym(feature_name)
  fseq <- feature_seq(dplyr::pull(data, !!feature_name))
  purrr::map(fseq, feature_replace, data = data, feature_name = !!feature_name) %>%
    purrr::map_dfr(mean_predict, object = object) %>%
    dplyr::bind_cols(!!feature_name := fseq)
}


#' Effect plot
#'
#' @param object
#' @param data
#' @param feature_name
#'
#' @return
#' @export
effect_plot <- function(object, data, feature_name) {
  feature_name <- ensym(feature_name)
  if (typeof(dplyr::pull(data, !!feature_name)) == "double") {
    effect(object, data, !!feature_name) %>%
      ggplot2::ggplot(ggplot2::aes(x = !!feature_name, y = .mean_pred)) +
      ggplot2::geom_line() +
      ggplot2::ylab("Prediction")
  } else {
    effect(object, data, !!feature_name) %>%
      ggplot2::ggplot(ggplot2::aes(x = !!feature_name, weight = .mean_pred)) +
      ggplot2::geom_bar() +
      ggplot2::ylab("Prediction")
  }
}
