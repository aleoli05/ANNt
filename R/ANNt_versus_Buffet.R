#' ANNt_versus_Buffet
#' Realize the 1st time series research, Validation: Long Term Testing, defined in "Value investing or quantitative financing: portfolio decision based on a new efficient frontier concept" paper.
#'
#' @param Parameters was defined in paper
#' @examples
#' ANNt_versus_Buffet()
#'
#' @export
ANNt_versus_Buffet <- function() {


# 1) Import the assets series, example:
Assets_series (Tickers='Current_SP500_Tickers','^GSPC', '2018-01-03', '','daily')

# 2) Specify Buffet´s Portfolio, example:
Name = c("My_Pf")
Portfolio=c("AAPL", "BAC", "CVX", "KO", "AXP", "KHC", "OXY", "MCO") # PORTFOLIO´s Buffet 2023
Weights=c( 0.441, 0.089, 0.079, 0.075, 0.074, 0.038, 0.038, 0.023) # PORTFOLIO weights 2023
Specify_Pf_RM(Name,Portfolio,Weights)

# 3) ANNt order generate, example:
ANNt_order ('2018-01-11', '2021-12-30','2022-08-04 ', '', 5000)

# 4) Generate portfolios, example:
Gen_portfolios(5,'2020-01-21','2022-08-04',0, 'T8')

# 5) Portfolios Backtesting, example:
Portfolio_backtesting('2020-01-21','2022-08-04')

# 6) Plot Cumulative Portfolio Returns, example:
Plot_Cumulative_Returns('')

# 7) Generate Efficient Frontier of Markowitz:
Gen_efficient_frontier('2020-01-21','2022-08-04')

# 8) Plot the Efficient Frontier graphic:
Plot_efficient_frontier()

# 9) Plot the New Efficient Frontier:
Plot_New_efficient_frontier()

# 10) Plot the Cumulative Sum of Returns (CUSUM Graphic):
Plot_CUSUM(1,5)

# 11) Save copy of alls processed data, example:
Backup_ANNt()

}
