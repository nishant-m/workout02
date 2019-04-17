#' @title future value function
#' @description computes future value of an investment
#' @param amount initial invested amount
#' @param rate annual rate of return
#' @param years number of years
#' @return computed future value
future_value = function(amount = 0, rate = 0, years = 0) {
  fa = amount * (1 + rate) ^ (years)
  return(fa)
}

#' @title future value of annuity function
#' @description computes future value of annuity of an investment
#' @param contrib contributed amount
#' @param rate annual rate of return
#' @param years number of years
#' @return computed future value of annuity
annuity = function(contrib = 0, rate = 0, years = 0) {
  fv = contrib * (((1+rate)^years - 1) / rate)
  return(fv)
}

#' @title future value of growing annuity function
#' @description computes future value of growing annuity of an investment
#' @param contrib contributed amount
#' @param rate annual rate of return
#' @param growth annual growth rate
#' @param years number of years
#' @return computed future value of annuity
growing_annuity = function(contrib = 0, rate = 0, growth = 0, years = 0) {
  fvga = contrib * (((1+rate)^years - (1+growth)^years) / (rate - growth))
  return(fvga)
}

