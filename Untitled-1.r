USAll_withPIPs <- readRDS("~/Downloads/USAll_withPIPs.rds")

dataN <- "32818K"
dataN <- "88068M"
indices <- USAll_withPIPs[[dataN]]$PIPData
indices <- sort(indices)
PRICE <- USAll_withPIPs[[dataN]]$TSData$PRICE
DATE <- USAll_withPIPs[[dataN]]$TSData$DATE

QuerySeries_times <- DATE[indices]
QuerySeries_prices <- PRICE[indices]

Original_times <- DATE
Original_prices <- PRICE

dim(ChartPatterns::fastFind(indices-1, DATE, PRICE))

dim(ChartPatternAnalysis::fastFind(indices-1, DATE, PRICE))     # seems to be the old normal fastfind
dim(ChartPatterns::fastFind_chaosRegin(indices-1, DATE, PRICE)) # seems to be the old normal fastfind

dim(do.call(cbind, ChartPatternAnalysis::FastFindII(indices-1, DATE, PRICE)))     # seems to be the old normal fastfind but only SHS
dim(do.call(cbind, ChartPatterns::FastFindII_chaosRegin(indices-1, DATE, PRICE))) # seems to be the old normal fastfind but only SHS

dim(ChartPatterns::FastFindII(indices-1, DATE, PRICE))

dim(do.call(cbind, ChartPatterns::findPatterns(indices-1, DATE, PRICE)))



res2 <- ChartPatternAnalysis::FastFindII(indices-1, DATE, PRICE)
res5 <- do.call(cbind, ChartPatternAnalysis::FastFindII(indices-1, DATE, PRICE))

res1 <- ChartPatterns::fastFind(indices-1, DATE, PRICE)
res2 <- do.call(cbind, ChartPatterns::findPatterns(indices-1, DATE, PRICE))
res3 <- ChartPatterns::FastFindII(indices-1, DATE, PRICE)
res4 <- do.call(cbind, ChartPatterns::FastFindII_chaosRegin(indices-1, DATE, PRICE))
res5 <- ChartPatterns::fastFind_chaosRegin(indices-1, DATE, PRICE)

length(which(res1$PatternName == "SHS"))
length(which(res1$PatternName == "iSHS"))

length(which(res2$patternInfo.PatternName == "SHS"))
length(which(res2$patternInfo.PatternName == "iSHS"))

length(which(res3$patternInfo.PatternName == "SHS"))
length(which(res3$patternInfo.PatternName == "iSHS"))

length(which(res4$patternInfo.PatternName == "SHS"))
length(which(res4$patternInfo.PatternName == "iSHS"))

length(which(res5$patternInfo.PatternName == "SHS"))
length(which(res5$patternInfo.PatternName == "iSHS"))



length(which(res1$PatternName == "SHS"))
length(which(res1$PatternName == "iSHS"))

length(which(res2$patternInfo.PatternName == "SHS"  & res2$patternInfo.validPattern == TRUE))
length(which(res2$patternInfo.PatternName == "iSHS" & res2$patternInfo.validPattern == TRUE))

length(which(res3$patternInfo.PatternName == "SHS"  & res3$patternInfo.validPattern == TRUE))
length(which(res3$patternInfo.PatternName == "iSHS" & res3$patternInfo.validPattern == TRUE))

length(which(res4$patternInfo.PatternName == "SHS"  & res4$patternInfo.validPattern == TRUE))
length(which(res4$patternInfo.PatternName == "iSHS" & res4$patternInfo.validPattern == TRUE))

length(which(res5$patternInfo.PatternName == "SHS"  & res5$patternInfo.validPattern == TRUE))
length(which(res5$patternInfo.PatternName == "iSHS" & res5$patternInfo.validPattern == TRUE))


summary2 <- res2[c("patternInfo.PatternName","patternInfo.validPattern",
      "patternInfo.firstIndexinOriginal","patternInfo.breakoutIndexinOrig",
      "patternInfo.firstIndexinPrePro")]

summary3 <- res3[c("patternInfo.PatternName","patternInfo.validPattern",
      "patternInfo.firstIndexinOriginal","patternInfo.breakoutIndexinOrig",
      "patternInfo.firstIndexinPrePro")]

summary4 <- res4[c("patternInfo.PatternName","patternInfo.validPattern",
      "patternInfo.firstIndexinOriginal","patternInfo.breakoutIndexinOrig",
      "patternInfo.firstIndexinPrePro")]


cbind(tail(summary2[,c(1,2,3)]),
tail(summary3[,c(1,2,3)]),
tail(summary4[,c(1,2,3)]))

duplicated(rbind(summary2, summary3))
duplicated(rbind(summary3, summary2))


simpleDraw <- function(df, i, DATE, PRICE){
  start <- df$patternInfo.firstIndexinOriginal[i]
  end <- df$patternInfo.breakoutIndexinOrig[i] 
  
  istart <- df$patternInfo.firstIndexinPrePro[i]
  
  plot( DATE[start:(end + 10)], PRICE[start:(end + 10)], type="l" )
  lines( DATE[start:(end)], PRICE[start:(end)], col="red" )
  lines( DATE[indices[istart:(istart+10)]], PRICE[indices[istart:(istart+10)]], col="green" )
}

for(i in 140:150){
  simpleDraw(res2, i, DATE,PRICE)
}

simpleDraw(res3[which(res3$patternInfo.validPattern),], 40, DATE,PRICE)
simpleDraw(res2[which(res2$patternInfo.validPattern),], 40, DATE,PRICE)


i <- 417

ChartPatterns::safeLinearInterpolation( QuerySeries_times[i+2], 
                                        QuerySeries_times[i+4],
                                        QuerySeries_prices[i+2],
                                        QuerySeries_prices[i+4],
                                        QuerySeries_times[i+7])

ChartPatterns::linearInterpolation(     QuerySeries_times[i+2], 
                                        QuerySeries_times[i+4],
                                        QuerySeries_prices[i+2], 
                                        QuerySeries_prices[i+4],
                                        QuerySeries_times[i+7])


rbind(QuerySeries_times[i+2], 
      QuerySeries_prices[i+2],
      QuerySeries_times[i+4],
      QuerySeries_prices[i+4],
      QuerySeries_times[i+7]
      QuerySeries_prices[i+7])


res2[40, c(1:5)]
res3[40, c(1:7)]