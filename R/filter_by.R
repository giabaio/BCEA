
# helper functions so don't have to remember
# which dimension for which statistic

Ustar_filter_by <- function(he, wtp) {
  he$Ustar[, he$k == wtp]
}

U_filter_by <- function(he, wtp) {
  he$U[, he$k == wtp, ]
}

ib_filter_by <- function(he, wtp) {
  he$ib[he$k == wtp, , ]
}

ol_filter_by <- function(he, wtp) {
  he$ol[, he$k == wtp]
}

vi_filter_by <- function(he, wtp) {
  he$vi[, he$k == wtp]
}
