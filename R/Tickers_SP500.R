#' Tickers_SP500
#' @description Return Current Tickers SP500
#' @param Tickers 'Current_SP500'
#' @param Exclude_ticket Exclude specific tickers
#' @examples
#' # example code
#'
#' Tickers_SP500(Tickers='Current_SP500_Tickers')
#' @export
Tickers_SP500<-function(Tickers='Current_SP500_Tickers', Exclude_ticket=''){

Tickers_1=Tickers
Condicao=Tickers
################################################################################

###############################
#install.packages("rvest")
library(dplyr)
library(rvest)
options(warn=-1)
# get the URL for the wikipedia page with all SP500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the SP500 table using rvest
tickers_ <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # one way to get table
  #html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  # easier way to get table
  html_nodes(xpath = '//*[@id="constituents"]') %>%
  html_table()
#create a vector of tickers
sp500tickers <- tickers_[[1]]
sp500tickers = sp500tickers %>% mutate(Symbol =
                                         case_when(Symbol == "BRK.B" ~ "BRK-B",
                                                   Symbol == "BF.B" ~ "BF-B",
                                                   TRUE ~ as.character(Symbol)))
Current_SP500<-sp500tickers$Symbol
#######

x=as.numeric(any(c('Current_SP500_Tickers') %in% Tickers_1))
if (x==1) {
  y=which(Tickers_1 %in% c('Current_SP500_Tickers'))
  Tickers_2=Tickers[-y]

  z=as.numeric(any(Tickers_2 %in% Current_SP500))
  if (z==1) {
    h=which(Tickers_2 %in% Current_SP500)
    Tickers_3=Tickers_2[-h]
    Tick=c(Tickers_3,Current_SP500)
  }

  Tick=c(Tickers_2,Current_SP500)
} else {
  Tick=Tickers_1

}


Tickers=Tick

###############Exclude_ticket
Exclude=NULL
for (i in 1:length(Exclude_ticket)) {
  Exclude[i]<-which(Tickers==Exclude_ticket[i])
  #Exclude<-Tickers[!Exclude_ticket]
}
if(length(Exclude)!=0){
  Tickers=Tickers[-Exclude]
}
###############
########################################################################################
if (x==1) {

  Tickers = Current_SP500
  #Calculate Returns: Daily
  ###############Exclude_ticket
  Exclude=NULL
  for (i in 1:length(Exclude_ticket)) {
    Exclude[i]<-which(Tickers==Exclude_ticket[i])
    #Exclude<-Tickers[!Exclude_ticket]
  }
  if(length(Exclude)!=0){
    Tickers=Tickers[-Exclude]
  }}
  ###############

  save(Tickers, file='~/Tickers_SP500.rda')
  print(Tickers)

}
