require('httr');require('highcharter');require('quantmod');require('scales');require('DT')

# set options
hcoptslang <- getOption('highcharter.lang')
hcoptslang$thousandsSep <-','
options(highcharter.lang = hcoptslang)

################################################################################
#  Get Financial : Income Statement, Cash Flow Statment, & Balance Sheet
################################################################################
# @param symbol = ticker symbol
# @param AQ = Annual (A) or Quarterly (Q)?
# @param FS = Financial Statement (FS): Income Statement (I), Balance Sheet (B), Cash Flow (C)
# @exemple
# getFins('TSLA', 'A', 'B')

getFins = function(symbol, AQ, FS){
  # get URL from Network
  URL = paste0('https://finviz.com/api/statement.ashx?t=',symbol,"&so=F&s=",FS,AQ)
  headers =c(
    'Host'='finviz.com',
    'User-Agent'='Mozilla/5.0 ( Mocintosh; Intel Mac 05 X 10.15; rv:132.0) Gecko/20100101 Firefox/132.0',
    'Accept'='*/*',
    'Accept-Language'= 'en-US,en;q=0.5',
    'Accept-Encoding'= 'gzip, deflote, br, zstd',
    'Referer'= 'http://finviz.com/quote.ashx?t=NVDA&p=d',
    'Connection'= 'keep-alive',
    'Cookie'= 'chartsTheme=dark; notice-newsletter=show; pv_date=Thu Dec 05 2024 01:52:09 GMT-0800 (Pacific Standard Time); pv_count=3; _ga_ZT9VQEWD4N=GS1.1.1.1733392370.0.0.0',
    'Sec-Fetch-Dest'= 'empty',
    'Sec-Fetch-Mode'= 'cors',
    'Sec-Fetch-Site'= 'same-origin',
    'Priority'='u=4'
    )
  # submit a get request
  pg = httr::GET(url= URL, add_headers(headers))
  # extract page contents
  res= httr::content(pg)
  # return as table
  tbl=unlist(res$data)
  # extract names for rows
  rName = gsub('1','',unique(names(tbl)[seq(1,length(tbl),8)]))
  # we can use matrix & assign the number of cols
  df=matrix(tbl, ncol=8, byrow=TRUE)
  # add row names
  df=data.frame(df,row.names=rName)
  # re-assign column names
  colnames(df)=df[1,1:ncol(df)]
  # remove first row
  df=df[-1,]
  # convert to numeric
  dfn= data.frame(apply(df, 2, function(x) as.numeric(gsub("\\,","", x))), row.names = rownames(df))
  colnames(dfn)= colnames(df)
  dfn = dfn[rowSums(is.na(dfn)) != ncol(dfn),]
  # return data
  dfn
  View(dfn)
}
################################################################################
# Plot Quarterly Values from the Financial
################################################################################
# ticker = 'TSLA'
# WHAT = 'Cash & Short Term Investments'

# @param ticker = ticker symbol
# @param WHAT = One of the row names from the financial table ex. 'Cash & Short Term Investments'
# @param FROM = data generate with getFins Command

table2plot = function(ticker, WHAT, FROM){
  # locate row
  dta <- subset(FROM, row.names(FROM)==WHAT)
  # reverse order (oldest to newest)
  dta <- rev(dta)
  # transpose
  dta = as.data.frame(t(dta))
  # as data.frame
  dta$date=row.names(dta)
  # data.frame
  dta = data.frame(date=format(as.Date(dta$date,"%m/%d/%Y"), "%b %d '%y"), value=dta[,1])
  # add Period to period growth
  dta$pct_change = ROC(dta$value, type = 'discrete') # pct = percentage
  dta$pct_change[is.na(dta$pct_change)]<-0
  # add YoY Growth
  dta$yoy_change = ROC(dta$value, n=4, type = 'discrete') # yoy = Year-on-year
  dta$yoy_change[is.na(dta$yoy_change)]<-0
  # plot using highcharter
  dta%>%
    hchart(
      'column', hcaes(x='date',y='value'), name='value',
      stacking='normal' #, tooltip = list(pointFormat = "<b>{series.name}: {point.y: , f}")
    ) %>%
    hc_colorAxis(minColor='aquamarine',maxColor='darkgreen') %>%
    hc_title(text=paste0('$',ticker), align='center') %>%
    hc_subtitle(text=paste0(WHAT)) %>%
    hc_yAxis_multiples(list(title=list(text='value'), opposite = FALSE),
                       list(showLastLabel = FALSE, opposite = TRUE, title=list(text='% Change'),
                            min=-500, max=500)) %>%
    hc_add_series(round(dta$yoy_change*100,2), yAxis=1, name="YoY % Change",
                  tooltip = list(pointFormat = "<b>{series.name}: {point.y:,.2f}%")) %>%
    hc_add_series(round(dta$pct_change*100,2), yAxis=1, name="QoQ % Change",
                  tooltip = list(pointFormat = "<b>{series.name}: {point.y:,.2f}%"))
  # QoQ = Quarter-on-quarter
}
##################################################################################
# Convert Financial Tables to Percentages
##################################################################################
# pecentage balance sheet
#@param BS = balance sheet from FinViz
pctBS = function(BS){
  #location of totals
  i = which(rownames(BS) == "Total Assets")   # for Assets
  j = which(rownames(BS) == "Total Liabilities") # for liabilities
  k = which(rownames(BS) == "Total Equity")  # for stockholder´s equity
  # for assets
  total_assets = do.call(rbind, lapply(as.list(1:i), function(ii){
   # convert row to percentage
    this_row <- t(scales::percent(as.numeric(BS[ii,]/BS[i,]), accuracy = 0.01))
   # convert to date frame
    this_row <- data.frame(this_row, row.names = rownames(BS)[ii])
   # add the column names
    colnames(this_row)= colnames(BS)
   # eliminate NA
    this_row[is.na(this_row)] <- scales::percent(0, accuracy = 0.01)
   # return formatted table
    this_row
  }))
  # for liabilities
  start_row = i+1
  total_liab = do.call(rbind, lapply(as.list(start_row:j), function(ii){
    # convert row to percentage
    this_row <- t(scales::percent(as.numeric(BS[ii,]/BS[j,]), accuracy = 0.01))
    # convert to data frame
    this_row <- data.frame(this_row, row.names = rownames(BS)[ii])
    # add the column names
    colnames(this_row)=colnames(BS)
    # eliminate NA
    this_row[is.na(this_row)] <- scales::percent(0, accuracy = 0.01)
    # return formatted table
    this_row
  }))
  # for stockholders equity
  start_row = j+1
  total_eqt = do.call(rbind, lapply(as.list(start_row:k), function(ii){
    # convert row to percentage
    this_row <- t(scales::percent(as.numeric(BS[ii,]/BS[k,]), accuracy = 0.01))
    # convert to data frame
    this_row <- data.frame(this_row, row.names = rownames(BS)[ii])
    # add the column names
    colnames(this_row) = colnames(BS)
    # eliminate NA
    this_row[is.na(this_row)] <- scales::percent(0, accuracy = 0.01)
    # return formatted table
    this_row
  }))
  # capture missing rows
  start_row = k+1
  misc_rows = BS[start_row:nrow(BS),]
  # combine
  rbind(total_assets, total_liab, total_eqt, misc_rows)
}

