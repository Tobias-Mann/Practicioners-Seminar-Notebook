#install.packages("iopsych")
#library(iopsych)

# read csv

path <- "/Users/kiliangerding/Downloads/relative_weights.csv"
hedge_partial <- read.csv(path, sep =",")

path2 <- "/Users/kiliangerding/Downloads/relative_weights_full.csv"
hedge_unhedged <- read.csv(path2, sep =",")

# avg weights
colMeans(hedge_partial[,10:16])


#cleaning
hedge_partial <- na.omit(hedge_partial)
hedge_unhedged <- na.omit(hedge_unhedged)

# weighting
hedge_partial['sum'] <- rowSums(hedge_partial[,10:16])
hedge_partial$excess <- hedge_partial$excess*hedge_partial$sum

hedge_unhedged['sum'] <- rowSums(hedge_unhedged[,10:16])
hedge_unhedged$excess <- hedge_unhedged$excess*hedge_unhedged$sum

hedge_partial$USDCHF <- hedge_partial$USDCHF*hedge_partial$USD
hedge_partial$EURCHF <- hedge_partial$EURCHF*hedge_partial$EUR
hedge_partial$GBPCHF <- hedge_partial$GBPCHF*hedge_partial$GBP
hedge_partial$AUDCHF <- hedge_partial$AUDCHF*hedge_partial$AUD
hedge_partial$CADCHF <- hedge_partial$CADCHF*hedge_partial$CAD
hedge_partial$JPYCHF <- hedge_partial$JPYCHF*hedge_partial$JPY

hedge_unhedged$USDCHF <- hedge_unhedged$USDCHF * hedge_unhedged$USD
hedge_unhedged$EURCHF <- hedge_unhedged$EURCHF * hedge_unhedged$EUR
hedge_unhedged$GBPCHF <- hedge_unhedged$GBPCHF * hedge_unhedged$GBP
hedge_unhedged$AUDCHF <- hedge_unhedged$AUDCHF * hedge_unhedged$AUD
hedge_unhedged$CADCHF <- hedge_unhedged$CADCHF * hedge_unhedged$CAD
hedge_unhedged$JPYCHF <- hedge_unhedged$JPYCHF * hedge_unhedged$JPY


hedge_partial <- hedge_partial[-c(1,3:5,7,8,10:17)]
hedge_unhedged <- hedge_unhedged[-c(1,8, 10:17)]

# relative strength analysis

# step1: cor matrix
m1 <- cor(hedge_partial)
m2 <- cor(hedge_unhedged)

# relative weights regression
relw1 <- relWt(m1, 3, c(1,2))
relw2 <- relWt(m2, 7, c(1:6))

# output
relw1
relw2

# simple regressions
 first <- lm(excess~USDCHF + JPYCHF +EURCHF +CADCHF+ AUDCHF+ GBPCHF,data= hedge_unhedged)
summary(first)




# single country hedge - example Japan

path3 <- "/Users/kiliangerding/Downloads/chf_hedged.csv"
hedge_single_index <- read.csv(path3, sep =",")

path4 <- "/Users/kiliangerding/Downloads/ok.csv"
ok <- read.csv(path4, sep =",")

eco_hedge <- data.frame(hedge_single_index$Date)
eco_hedge["excess"] <- data.frame(hedge_single_index$MSDLJN.Index - ok$MSDUJN.Index)
eco_hedge <- na.omit(eco_hedge)
colnames(eco_hedge) <- c("Exchange.Date", "Excess")

eco_hedge <- left_join(eco_hedge, hedge_unhedged[1:7])
eco_hedge <- na.omit(eco_hedge)
eco_hedge <- eco_hedge[-1]


# step1: cor matrix
mh <- cor(eco_hedge)

# relative weights regression
relwh <- relWt(mh, 1, c(2:7))

# output
relwh
