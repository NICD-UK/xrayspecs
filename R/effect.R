
#' Feature sequence
#'
#' @param feature feature vector
#' @param length length of output vector
#'
#' @return sequence from min feature to max feature
feature_seq <- function(feature, length = 100) {

  seq(min(feature), max(feature), length.out = length)

}

#' Feature replace
#'
#' @param new_data date frame
#' @param feature_name name of feature
#' @param feature_value value of feature
#'
#' @return date frame with all feature values replace by feature_value
feature_replace <- function(new_data, feature_name, feature_value) {

  dplyr::mutate(new_data, !!ensym(feature_name) := feature_value)

}

#' Mean predict
#'
#' @param object parsnip model object
#' @param new_data data frame
#'
#' @return mean predictions
mean_predict <- function(object, new_data) {

  predict(object, new_data) %>%
    dplyr::summarise(.mean_pred = mean(.pred))

}

#' Effect
#'
#' @param object parsnip model object
#' @param new_data data frame
#' @param feature_name name of feature
#'
#' @return data frame with mean prediction and feature
#' @export
effect <- function(object, new_data, feature_name) {

  feature_name <- ensym(feature_name)

  fseq <- feature_seq(dplyr::pull(new_data, !!feature_name))

  new_data_expand <- purrr::map(fseq, feature_replace, new_data = new_data, feature_name = !!feature_name)

  mean_pred <- purrr::map_dfr(new_data_expand, mean_predict, object = object)

  dplyr::bind_cols(mean_pred, !!feature_name := fseq)

}

#' Effect plot
#'
#' @param object parsnip model object
#' @param new_data data frame
#' @param feature_name name of feature
#'
#' @return plot of effect
#' @export
effect_plot <- function(object, new_data, feature_name) {

  feature_name <- ensym(feature_name)

  effect(object, new_data, !!feature_name) %>%
    ggplot2::ggplot(ggplot2::aes(x = !!feature_name, y = .mean_pred)) +
    ggplot2::geom_line() +
    ggplot2::ylab("Prediction")

}
