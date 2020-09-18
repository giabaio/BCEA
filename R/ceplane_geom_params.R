
#' extract separate parameter sets
#'
ceplane_geom_params <- function(...) {
  
  extra_params <- list(...)
  
  icer_params <-
    extra_params[
      names(extra_params) %in% c("ICER_size", "ICER_colors")]
  names(icer_params) <- gsub("ICER_", "", names(icer_params))
  
  point_params <-
    extra_params[
      names(extra_params) %in% c("point_size", "point_colors")]
  names(point_params) <- gsub("point_", "", names(point_params))
  
  polygon_params <-
    extra_params[
      names(extra_params) %in% c("area_include", "area_color")]
  names(polygon_params) <- gsub("area_", "", names(polygon_params))
  
  wtp_params <-
    extra_params[
      names(extra_params) %in% "label.pos"]
  
  modifyList(
    list(
      area = list(include = TRUE)),
    list(
      icer = icer_params,
      point = point_params,
      area = polygon_params,
      wtp = wtp_params))
}