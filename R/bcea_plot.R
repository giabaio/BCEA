#' ##TODO:
#' 
#' # replace top-level functions with sub-classes:
#' 
#' ceac.plot <- function(he, graph = "base") {
#'   he <- structure(he, class = list("ceac", class(he)))
#'   bcea_plot(he, graph)
#' }
#' 
#' ceplane.plot <- function(he, graph = "base") {
#'   he <- structure(he, class = list("ceplane", class(he)))
#'   bcea_plot(he, graph)
#' }
#' 
#' # then don't have to repeat this structure every time:
#' 
#' bcea_plot <- function(he, graph, ...) {
#'   params <- make_plot_params(he)
#'   data <- make_plot_data(he)
#'   
#'   if (is_baseplot(graph))
#'     plot_base(he, data, params)
#'   if (is_ggplot(graph))
#'     plot_ggplot(he, data, params)
#' }
#' 
#' #'
#' make_plot_params <- function(he, ...) {
#'   UseMethod('make_plot_params', he)
#' }
#' 
#' make_plot_params.ceac <- function(he) {
#'   
#' }
#' 
#' make_plot_data.ceac <- function(he) {
#'   
#' }
#' 
#' #'
#' plot_base <- function(he, ...) {
#'   UseMethod('plot_base', he)
#' }
#' 
#' plot_base.ceac <- function(he) {
#'   
#' }
#' 
#' plot_base.ceplane <- function(he) {
#'   
#' }
#' 
#' #'
#' plot_ggplot <- function(he, ...) {
#'   UseMethod('plot_base', he)
#' }
#' 
#' plot_ggplot.ceac <- function(he) {
#'   
#' }
#' 
#' plot_ggplot.ceplane <- function(he) {
#'   
#' }
#' 
#' 
#' #'
#' make_plot_data <- function(he, ...) {
#'   UseMethod('make_plot_data', he)
#' }
#' 
#' 
#' 
