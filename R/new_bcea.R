#' Constructor for bcea
#'
#' @param df_ce Dataframe of all simulation eff and cost
#' @param k Vector of willingness to pay values
#'
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr pivot_wider
#' @importFrom stats setNames
#' 
#' @return List object of class bcea.
#' @seealso [bcea()]
#' 
#' @export
#'
new_bcea <- function(df_ce, k) {
  ref <- unique(df_ce$ref)
  comp <- setdiff(seq_len(max(df_ce$ints)), ref)
  df_ce_comp <- df_ce |> filter(.data$ints != ref) |> 
    # Arranges the dataset so that it can be operated upon using pivot_wider
    # NB: Needs to arrange by both sim and interv_names to ensure that
    #     the resulting data are in the same order when changing the 
    #     reference intervention using one of the setters. Needs to be
    #     explicit in defining the variables by which to sort!
    arrange(.data$sim, .data$ints)
  
  ICER <- compute_ICER(df_ce)
  ib <- compute_IB(df_ce, k)
  ceac <- compute_CEAC(ib)
  eib <- compute_EIB(ib)
  best <- best_interv_given_k(eib, ref, comp)
  kstar <- compute_kstar(k, best, ref)
  U <- compute_U(df_ce, k)
  Ustar <- compute_Ustar(U)
  vi <- compute_vi(Ustar, U)
  ol <- compute_ol(Ustar, U, best)
  evi <- compute_EVI(ol)
  
  interv_names <- levels(df_ce$interv_names)
  
  # Reshape eff1
  e_dat <- df_ce |>
    select("sim", interv_names, "eff1") |>
    pivot_wider(
      names_from = interv_names,
      values_from = "eff1"
    ) |>
    arrange(.data$sim) |> select(-"sim") |>
    as.data.frame()
  
  # Reshape cost1
  c_dat <- df_ce |>
    select("sim", interv_names, "cost1") |>
    pivot_wider(
      names_from = interv_names,
      values_from = "cost1"
    ) |>
    select(-"sim") |>
    as.data.frame()
  
  # Reshape delta_e
  delta_e <- df_ce_comp |>
    select("sim", interv_names, delta_e) |>
    pivot_wider(
      names_from = interv_names,
      values_from = delta_e
    ) |>
    select(-"sim") |>
    as.data.frame()
  
  # Reshape delta_c
  delta_c <- df_ce_comp |>
    select("sim", interv_names, delta_c) |>
    pivot_wider(
      names_from = interv_names,
      values_from = delta_c
    ) |>
    select(-"sim") |>
    as.data.frame()
  
  he <- list(
    n_sim = n_distinct(df_ce$sim),
    n_comparators = length(comp) + 1,
    n_comparisons = length(comp),
    delta_e = delta_e,
    delta_c = delta_c,
    ICER = ICER,
    Kmax = max(k),
    k = k,
    ceac = ceac,
    ib = ib,
    eib = eib,
    kstar = kstar,
    best = best,
    U = U,
    vi = vi,
    Ustar = Ustar,
    ol = ol,
    evi = evi,
    ref = ref,
    comp = comp,
    step = k[2] - k[1],
    interventions = interv_names,
    e = as.matrix(e_dat),
    c = as.matrix(c_dat)
  )
  
  structure(he, class = c("bcea", class(he)))
}
