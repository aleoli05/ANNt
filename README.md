# ANNt
Artificial Neural Network with 't' Distribution Portfolio 

# SINGLE COMMAND FOR THE ENTIRE PACKAGE AFTER INSTALLING THE REQUIRED PACKAGES#
        install.packages('remotes')
        library(remotes)
        install_github('aleoli05/ANNt')
        
        or if have problems:
        
        install.packages('devtools')
        options(download.file.method = "wininet") 
        devtools::install_github('aleoli05/ANNt')
        
        ANNt_Oliveira_Ceretta(c('AAPL','XOM','TSLA','KO', 'F'), '^GSPC', 0, '2018-01-03', '2022-12-29', '', 'daily', 5, 2500, 3)
        

Follow the steps for the test step by step:

# 1) Install and enable remotes package: 
        install.packages('remotes')
        library(remotes)
# 2) Install and enable the ANNt package: 
        install_github('aleoli05/ANNt') 
        library(ANNt)
# 3) Install all required package in ANNt: 
        install_required_pakage()
# 4) Import the assets series, example: 
Assets_series (Tickers=c('AAPL','GOOG','CCBG','XOM','TSLA'),'^GSPC', '2018-01-03', '','daily')
# 5) ANNt order generate, example: 
         ANNt_order ('2018-01-11', '2022-12-30',' ', 5, 2500)
# 6) Generate portfolios, example: 
         Gen_portfolios(5,'2023-01-03','',0)
# 7) Portfolios Backtesting, example: 
         Portfolios_backtesting()
# 8) Plot Cumulative Portfolio Returns, example: 
         Plot_Cumulative_Returns('')
# 9) Generate Efficient Frontier of Markowitz:
         Gen_efficient_frontier('','')
# 10) Plot the Efficient Frontier graphic:
         Plot_efficient_frontier()
# 11) Plot the New Efficient Frontier:
         Plot_New_Efficient_Frontier()
# 12) Plot the Cumulative Sum of Returns (CUSUM Graphic):
         Plot_CUSUM(1,5)
# 13) Save copy of alls processed data, example: 
         Backup_ANNt()

