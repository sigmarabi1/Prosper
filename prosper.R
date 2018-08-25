##############################################################################
##############################################################################
###                                                                        ###
###         IST-687 Project: Analysis of Prosper dataset                   ###
###             By: Miguel A.                                              ###
###                                                                        ###
##############################################################################
##############################################################################

##############################
# Loading Libraries, and Data
##############################
library(dplyr)
library(ggplot2)
library(reshape)
library(RColorBrewer)
library(sqldf)
library(caTools)
library(tictoc)
library(e1071)
library(randomForest)
library(neuralnet)
library(class)
library(plotly)
library(maps)
library(lime)
library(ROSE)
library(caret)
library(ggcorr)
#library(plyr)

setwd("C:/Data Science/Syracuse/IST687/Project/")
loans <- read.csv(file <- "./Dataset/prosperLoanData.csv", header = T )
str(loans)
summary(loans)

# eliminating columns with all values = NaN
loans <- loans[,colSums(is.na(loans))<nrow(loans)]

# Cast income as numeric
loans$StatedMonthlyIncome <- as.numeric(loans$StatedMonthlyIncome)

# Function to determine fico range
fico_range = function(ficolow, ficohigh) {
  paste(ficolow, ficohigh, sep = "-")
}
# calling function fico_range
loans$fico_range_name <- fico_range(loans$CreditScoreRangeLower, 
                               loans$CreditScoreRangeUpper)

# Using sql to select certain columns and default values to 0
# and also filtering rows with income < 50k, fico > 460, and loan status not NULL
loans.sm <- sqldf("select 	
                      loans.Term, 
                      loans.BorrowerRate,
                      loans.BorrowerState, 
                      case when loans.EmploymentStatus is NULL then 'Unknown'
                           else loans.EmploymentStatus end as EmploymentStatus,
                      case when loans.EmploymentStatusDuration is NULL then 0
                           else loans.EmploymentStatusDuration end as EmploymentStatusDuration, 
                      case when loans.ProsperScore is NULL then 0
                           else  loans.ProsperScore end as ProsperScore,
                      loans.IsBorrowerHomeowner,
                      loans.CreditScoreRangeLower, 
                      loans.CreditScoreRangeUpper, 
                      loans.fico_range_name,
                      case when TotalCreditLinespast7years IS NULL then 0
                           else TotalCreditLinespast7years end as TotalCreditLinespast7year, 
                      loans.OpenRevolvingAccounts, 
                      loans.OpenRevolvingMonthlyPayment, 
                      case when loans.InquiriesLast6Months IS NULL then 0
                           else loans.InquiriesLast6Months end as InquiriesLast6Months,
                      case when loans.CurrentDelinquencies IS NULL then 0
                           else loans.CurrentDelinquencies end as CurrentDelinquencies,
                      case when loans.DelinquenciesLast7Years IS NULL then 0
                           else loans.DelinquenciesLast7Years end as DelinquenciesLast7Years,
                      case when loans.AmountDelinquent IS NULL then 0
                           else loans.AmountDelinquent end as AmountDelinquent,
                      case when loans.PublicRecordsLast10Years IS NULL then 0
                           else loans.PublicRecordsLast10Years end as PublicRecordsLast10Years,
                      case when loans.PublicRecordsLast12Months IS NULL then 0
                           else loans.PublicRecordsLast12Months end as PublicRecordsLast12Months,
                      case when loans.RevolvingCreditBalance IS NULL then 0
                           else loans.RevolvingCreditBalance end as RevolvingCreditBalance,
                      case when loans.BankcardUtilization IS NULL then 0
                           else loans.BankcardUtilization end as BankcardUtilization,
                      case when loans.AvailableBankcardCredit IS NULL then 0
                           else loans.AvailableBankcardCredit end as AvailableBankcardCredit,
                      case when loans.DebtToIncomeRatio IS NULL then 0
                           else loans.DebtToIncomeRatio end as DebtToIncomeRatio,
                      case when loans.Occupation is NULL then 'Unknown'
                           when loans.Occupation = 'Other' then 'Unknown'
                           when loans.Occupation like 'Student%'   then  'Student' 
                           when loans.Occupation like 'Nurse%'    then  'Nurse'
                           when loans.Occupation like 'Engineer%'  then  'Engineer' 
                           when loans.Occupation like 'Tradesman%' then  'Tradesman' 
                           when loans.Occupation like 'Military%'  then  'Military' 
                           when loans.Occupation like 'Teacher%'   then  'Teacher'
                           when loans.Occupation like 'Sales%'     then  'Sales' 
                           else loans.Occupation end as Occupation,
                      loans.IncomeRange,
                      loans.IncomeVerifiable,
                      loans.StatedMonthlyIncome,
                      loans.LoanOriginationQuarter,
                      loans.LoanOriginalAmount,
                      loans.LoanOriginalAmount * loans.PercentFunded as LoanAmountFunded,
                      loans.MonthlyLoanPayment,
                      loans.LP_CustomerPrincipalPayments as PrincipalPayments,
                      loans.PercentFunded,
                      loans.LoanStatus,
                      loans.ListingCategoryNum,
                      case when loans.LoanStatus like 'Past Due%' Then 1
                           when loans.LoanStatus In('Defaulted','Chargedoff') Then 1
                           else 0 end as not_fully_paid			
                  from loans
                  where loans.StatedMonthlyIncome < 50000 and
                        loans.loanStatus is NOT NULL and 
                        loans.CreditScoreRangeLower > 460 and 
                        loans.LoanStatus <> 'Cancelled' ")

# These columns become categorical data
loans.sm$Term                        <- factor(loans.sm$Term)
loans.sm$IncomeVerifiable            <- factor(loans.sm$IncomeVerifiable)
loans.sm$not_fully_paid              <- factor(loans.sm$not_fully_paid)
loans.sm$IsBorrowerHomeowner         <- factor(loans.sm$IsBorrowerHomeowner)
loans.sm$Occupation                  <- factor(loans.sm$Occupation)
loans.sm$LoanStatus                  <- factor(loans.sm$LoanStatus)
loans.sm$EmploymentStatus            <- factor(loans.sm$EmploymentStatus)
loans.sm$fico_range_name             <- factor(loans.sm$fico_range_name)
loans.sm$ListingCategoryNum          <- factor(loans.sm$ListingCategoryNum)
# Calculating average of Low and High Fico scores
loans.sm$CreditScoreRangeAvg         <- round((loans.sm$CreditScoreRangeLower +
                                             loans.sm$CreditScoreRangeUpper)/2)


detach("package:plyr", unload=TRUE) # due to conflict with dplyr package summarise

#Summary of loan amount grouped by variable 
sumAmnts = function(x, ...) {
  x %>% group_by(., ...) %>%
    summarise(., total_issued = prettyNum(round(sum(LoanOriginalAmount/1)),big.mark = ","),
              n = prettyNum(round(n()),big.mark = ","))
}

# Stats summary group by variable
sumStats = function(x, ...) {
  x %>% group_by(., ...) %>%
    summarise(., median = prettyNum(round(median(LoanOriginalAmount/1)),big.mark = ","),
                average = prettyNum(round(mean(LoanOriginalAmount/1)),big.mark = ","),
                 stdev  = prettyNum(round(sd(LoanOriginalAmount/1)),big.mark = ","))
}

sumStat = function(x, ...) {
  x %>% group_by(., ...) %>%
    summarise(., median = round(median(LoanOriginalAmount/1)),
              average = round(mean(LoanOriginalAmount/1)),
              stdev = round(sd(LoanOriginalAmount/1)))
}

# calculating return on investment
roi = function(x, ...){
  x %>% group_by(...) %>%
    summarize(., roi = round(sum((LP_CustomerPayments / LoanAmountFunded) * 100) / n(), 2))
}

sumAmnt = function(x, ...) {
  x %>% group_by(., ...) %>%
    summarise(., total_issued = round(sum(LoanOriginalAmount/1e6),1),
              n = round(n()))
}

sumPerSatus = function(x, ...){
  x %>% group_by(...) %>%
    summarize(., charged   = round(sum(not_fully_paid == 1) / n() * 100, 2),
              net_EL    = round(sum((1 - PrincipalPayments / LoanAmountFunded) * 100) / n(), 2),
              avg_fico  = round(mean(CreditScoreRangeAvg)))
}

# Looking at loan amounts types and loan amounts in each category
min(loans.sm$LoanOriginalAmount) # $1000
max(loans.sm$LoanOriginalAmount) # $35000
prettyNum(sum(loans.sm$LoanOriginalAmount/1), big.mark = ",") # $946,220,281
prettyNum(sum(loans.sm$LoanOriginalAmount/1, na.rm = T), big.mark = ",")

# amounts and number of loans for loan status:
sumAmnts(loans.sm, LoanStatus)
# amounts and number of loans for not_fully_paid:
sumAmnts(loans.sm, not_fully_paid)

# Summary by fico score range
SumByFico <- sumAmnts(loans.sm, fico_range_name)


# Report showing some statistics for each category, here the latest status of the loans
sumStats(loans.sm, not_fully_paid)

byQtr <- sumAmnts(loans.sm, LoanOriginationQuarter)

  sumStates = group_by(loans.sm, BorrowerState) %>%
    summarise(., total_issued = round(sum(LoanOriginalAmount/1e6),1), n = n()) %>%
    merge(sumPerSatus(loans.sm, BorrowerState)[,c(1,2)]) %>% 
  merge(data.frame(BorrowerState, state.name))

##############################################################################
# Data Visualization
##############################################################################

cbPalette <- c( "#33CC33" ,"#FF0000") #custom colors

# Bar plot by EmploymentStatus
pl <- ggplot(loans.sm,aes(x=factor(EmploymentStatus))) 
pl <- pl + geom_bar(aes(fill=not_fully_paid))
pl <- pl + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
pl +  scale_fill_manual(values=cbPalette)

# Bar plot by Occupation
pl <- ggplot(loans.sm,aes(x=factor(Occupation))) 
pl <- pl + geom_bar(aes(fill=not_fully_paid))
pl <- pl + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
pl +  scale_fill_manual(values=cbPalette)

# Bar plot by Income Range
pl <- ggplot(loans.sm,aes(x=factor(IncomeRange))) 
pl <- pl + geom_bar(aes(fill=not_fully_paid))
pl <- pl + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
pl +  scale_fill_manual(values=cbPalette)

# Scatter plot of interest rate by fico score and not_fully_paid
pl <- ggplot(loans.sm,aes(CreditScoreRangeAvg, BorrowerRate)) 
pl <- pl + geom_point(aes(color=not_fully_paid),alpha=0.5, size=3) + theme_bw()
pl +  geom_smooth(aes(group=1), method ='lm', formula = y~log(x), se=FALSE, color='red')

# Histogram by fico score
pl <- ggplot(loans.sm,aes(x=CreditScoreRangeUpper)) 
pl <- pl + geom_histogram(aes(fill=not_fully_paid),color='black',bins=20,alpha=0.5)
pl + scale_fill_manual(values=cbPalette)

# Box plot of interest_rate by loan status 
pl <- ggplot(loans.sm, aes(x=not_fully_paid, y=BorrowerRate))
pl <- pl +  geom_boxplot(aes(fill = not_fully_paid)) 
pl + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Box plot of interest_rate by income range
pl <- ggplot(loans.sm, aes(x=IncomeRange, y=BorrowerRate, fill= not_fully_paid))
pl <- pl +  geom_boxplot() 
#pl <- pl +  geom_boxplot(aes(fill = IncomeRange)) 
pl + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Box of interest_rate by EmploymentStatus
pl <- ggplot(loans.sm, aes(x=EmploymentStatus, y=BorrowerRate, fill= not_fully_paid))
pl <- pl +  geom_boxplot() 
#pl <- pl +  geom_boxplot(aes(fill = IncomeRange)) 
pl + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Bar chart of Prosper's quaterly loan issuance
byQtr <- sumAmnt(loans.sm, LoanOriginationQuarter)
  plot_ly(data=byQtr,
          type = "bar", 
          x = ~LoanOriginationQuarter,
          y = ~total_issued,
          marker = ~list(color = total_issued,
                        colorscale = list(c(0, "rgb(201, 218, 248)"), list(1, "rgb(61, 133, 198)")),
                        line = list(width = 1, color = "rgb(255, 255, 255)"))
  ) %>%
  layout(title = "QUATERLY LOAN ISSUANCE", bargap = 0,
         yaxis = list(title = "TOTAL LOAN ISSUED IN MLN USD"),
         xaxis = list(title = "QUARTER OF ISSUE", range = c(7,32.5), dtick = 2))


# Pie chart of the distribution of Loans by Employment status:
mycolors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 
            'rgb(171,104,87)', 'rgb(114,147,203)')
sumAmnt(loans.sm, EmploymentStatus) %>% 
  merge(sumPerSatus(loans.sm, EmploymentStatus)) %>%
    plot_ly(type = "pie", 
          labels = ~EmploymentStatus, 
          values = ~total_issued, 
          hole = 0.3,
          marker = list(colors = mycolors,
                        line = list(color = '#FFFFFF', width = 1)),
          sort = F,
          direction = "counterclockwise",
          rotation = 90,
          textinfo = "label+percent",
          insidetextfont = list(color = '#FFFFFF'),
          textfont = list(size = 14),
          text = ~paste("Default rates: ", charged),
          textposition = "outside") %>%
             layout(title = 'LOANS BY EMPLOYMENT STATUS - (Hover for breakdown)',
             height = 500, width = 1274, autosize = T, 
             legend = list(font = list(size = 16), x = 1, y = 1, traceorder = "normal"))

# Pearson correlation coefficients, using pairwise observations (default method)
# Non-numeric columns automatically removed/ignored
library(GGally)
ggcorr(loans.sm, label = TRUE, label_size = 3,
       hjust = 0.8, size = 2.5, color = "black", layout.exp = 2)


##############################################################################
# Modeling
##############################################################################

performScaling <- TRUE  # Turn it on/off for experimentation.
if (performScaling) {
  # Loop over each column.
  for (colName in names(loans.sm)) {
    # Check if the column contains numeric data.
    if (class(loans.sm[,colName]) == 'integer' | 
        class(loans.sm[,colName]) == 'numeric') {
      # Scale this column (scale() function applies z-scaling).
      loans.sm[,colName] <- scale(loans.sm[,colName])
    }
  }
}

######################
# Train and Test split
######################
set.seed(50) # repeatable results
df.splt = sample.split(loans.sm$not_fully_paid, 0.7)
train = subset(loans.sm, df.splt == TRUE)
test = subset(loans.sm, df.splt == FALSE)

train_new <- train[,c('Term','BorrowerRate','BorrowerState','EmploymentStatus', 'Occupation',
                    'CreditScoreRangeAvg','OpenRevolvingMonthlyPayment','CurrentDelinquencies',
                    'PublicRecordsLast12Months', 'IncomeRange','not_fully_paid')]

test_new <- test[,c('Term','BorrowerRate','BorrowerState','EmploymentStatus', 'Occupation',
                    'CreditScoreRangeAvg','OpenRevolvingMonthlyPayment','CurrentDelinquencies',
                    'PublicRecordsLast12Months', 'IncomeRange','not_fully_paid')]

ggcorr(train_new, label = TRUE, label_size = 3,
       hjust = 0.8, size = 2.5, color = "black", layout.exp = 2)

barplot(prop.table(table(train_new$not_fully_paid)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution of Training Dataset")

######################
# Random Forest
######################
tic("training rf started at:")
print(format(Sys.time(), "%b %d %X"))
rf <- randomForest(not_fully_paid ~ Term + BorrowerRate + BorrowerState + 
                                    EmploymentStatus + Occupation + CreditScoreRangeAvg + 
                                    OpenRevolvingMonthlyPayment + CurrentDelinquencies + 
                                    PublicRecordsLast12Months + IncomeRange, 
                   data=train_new, 
                   ntree= 1000,
                   mtry = 8,
                   keep.forest=TRUE, 
                   importance = TRUE)

print("rf training completed at:")
print(format(Sys.time(), "%b %d %X"))
toc()
print("printing svm model summary")
print(rf) # view results

importance(rf)
print(rf$importance)
varImpPlot(rf)

tic("predicting rf...")
pred <- predict(rf,test_new[1:10])
toc()
print("Table: predicted values vs actual values")
table(pred, test_new$not_fully_paid)

rf_classification_error <- 1- sum(pred == test_new[,11])/length(pred)
print("Random Forest Classification Error:")
print(rf_classification_error)

#Dealing with class imbalance using ROSE Package
under <- ovun.sample(not_fully_paid~.,data=train_new, method = "under", N =27000)$data
table(under$not_fully_paid)

barplot(prop.table(table(under$not_fully_paid)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution: New Training Dataset")

rf.under <- randomForest(not_fully_paid ~ Term + BorrowerRate + BorrowerState + 
                     EmploymentStatus + Occupation + CreditScoreRangeAvg + 
                     OpenRevolvingMonthlyPayment + CurrentDelinquencies + 
                     PublicRecordsLast12Months + IncomeRange, 
                   data=under, 
                   ntree= 1000,
                   mtry = 8,
                   keep.forest=TRUE, 
                   importance = TRUE)

pred.under <- predict(rf.under,test_new[1:10])
table(pred, test_new$not_fully_paid)
table(pred.under, test_new$not_fully_paid)

library(caret)
confusionMatrix(pred, test_new$not_fully_paid, positive = '1')
confusionMatrix(pred.under, test_new$not_fully_paid, positive = '1')
rf_classification_error <- 1- sum(pred.under == test_new[,11])/length(pred)
#########################
# Support Vector Machines
#########################
tic("training of svm classifier started at:")
print(format(Sys.time(), "%b %d %X"))
svcl <- svm(not_fully_paid ~ Term + BorrowerRate + BorrowerState + EmploymentStatus + Occupation + 
            CreditScoreRangeAvg + OpenRevolvingMonthlyPayment + CurrentDelinquencies + 
            PublicRecordsLast12Months + IncomeRange,
          data=train_new, type="C-classification")
print("svm training completed at:")
print(format(Sys.time(), "%b %d %X"))
toc()
print("printing svm model summary")
print(summary(svcl))
# training svm classifier: 1450.04 sec elapsed

pred <- predict(svcl,test_new[1:10])
print("Table: predicted values vs actual values")
table(pred, test_new$not_fully_paid)

svm_classification_error <- 1- sum(pred == test_new[,11])/length(pred)
print("SVM classification Error: ")
print(svm_classification_error)


######################
# Neural Network
######################
# Creating a new df with numeric values only
pr.sm <- sqldf("select 	
               pr.BorrowerApr,
               pr.BorrowerRate,
               pr.Term, 
               case when pr.ProsperScore is NULL then 0
               else pr.ProsperScore end as ProsperScore,
               pr.ListingCategoryNum,
               case when pr.EmploymentStatus in('Employed','Full-time','Self-employed') then 1
                    when pr.EmploymentStatus in('Part-time','Retired') Then 2
                   else 0 end as EmploymentStatus,
               case when pr.EmploymentStatusDuration is NULL then 0
               else pr.EmploymentStatusDuration end as EmploymentStatusDuration, 
               pr.CreditScoreRangeLower, 
               pr.CreditScoreRangeUpper, 
               case when TotalCreditLinespast7years IS NULL then 0
               else TotalCreditLinespast7years end as TotalCreditLinespast7year, 
               pr.OpenRevolvingAccounts, 
               pr.OpenRevolvingMonthlyPayment, 
               case when pr.InquiriesLast6Months IS NULL then 0
               else pr.InquiriesLast6Months end as InquiriesLast6Months,
               case when pr.TotalInquiries IS NULL then 0
               else pr.TotalInquiries end as TotalInquiries,
               case when pr.CurrentDelinquencies IS NULL then 0
               else pr.CurrentDelinquencies end as CurrentDelinquencies,
               case when pr.DelinquenciesLast7Years IS NULL then 0
               else pr.DelinquenciesLast7Years end as DelinquenciesLast7Years,
               case when pr.AmountDelinquent IS NULL then 0
               else pr.AmountDelinquent end as AmountDelinquent,
               case when pr.PublicRecordsLast10Years IS NULL then 0
               else pr.PublicRecordsLast10Years end as PublicRecordsLast10Years,
               case when pr.PublicRecordsLast12Months IS NULL then 0
               else pr.PublicRecordsLast12Months end as PublicRecordsLast12Months,
               case when pr.RevolvingCreditBalance IS NULL then 0
               else pr.RevolvingCreditBalance end as RevolvingCreditBalance,
               case when pr.BankcardUtilization IS NULL then 0
               else pr.BankcardUtilization end as BankcardUtilization,
               case when pr.AvailableBankcardCredit IS NULL then 0
               else pr.AvailableBankcardCredit end as AvailableBankcardCredit,
               case when pr.TotalTrades IS NULL then 0
               else pr.TotalTrades end as TotalTrades,
               case when pr.TradesNeverDelinquent IS NULL then 0
               else pr.TradesNeverDelinquent end as TradesNeverDelinquent,
               case when pr.DebtToIncomeRatio IS NULL then 0
               else pr.DebtToIncomeRatio end as DebtToIncomeRatio,
               pr.StatedMonthlyIncome,
               pr.LoanOriginalAmount,
               pr.PercentFunded,
               pr.LoanMonthsSinceOrigination,
               pr.Investors,
               case when pr.LoanStatus like 'Past Due%' Then 1
               when pr.LoanStatus In('Defaulted','Chargedoff') Then 1
               else 0 end as not_fully_paid		
               from loans pr
               where pr.StatedMonthlyIncome < 50000 and
               pr.LoanStatus is NOT NULL and 
               pr.CreditScoreRangeLower > 460 and 
               pr.LoanStatus <> 'Cancelled' ")

# Train/Test Split
set.seed(50) # repeatable results
pr.splt = sample.split(pr.sm$not_fully_paid, 0.7)
train = subset(pr.sm, pr.splt == TRUE)
test = subset(pr.sm, pr.splt == FALSE)

tic("training NN classifier started at:")
print(format(Sys.time(), "%b %d %X"))

nn <- neuralnet(formula = not_fully_paid ~ BorrowerAPR + BorrowerRate+ Term + 
                  ProsperScore + ListingCategoryNum + 
                  EmploymentStatusDuration + CreditScoreRangeLower + 
                  CreditScoreRangeUpper + TotalCreditLinespast7year +  
                  OpenRevolvingAccounts + OpenRevolvingMonthlyPayment  +                                                                          InquiriesLast6Months + TotalInquiries + 
                  CurrentDelinquencies + DelinquenciesLast7Years +  
                  AmountDelinquent + PublicRecordsLast10Years + 
                  PublicRecordsLast12Months  + RevolvingCreditBalance + 
                  BankcardUtilization + AvailableBankcardCredit + 
                  TotalTrades + TradesNeverDelinquent + DebtToIncomeRatio + 
                  StatedMonthlyIncome + LoanOriginalAmount + 
                  PercentFunded + LoanMonthsSinceOrigination + Investors  
                , data=train, hidden=10, linear.output=FALSE)

print("nn training completed at")
print(format(Sys.time(), "%b %d %X"))
toc()
#Summary of NN Model:
summary(nn)

# Generating predictions
predicted.nn.values <- compute(nn,test[,1:29])
predictions <- sapply(predicted.nn.values$net.result, round)
print("Nural Network table of predictions:")
table(predictions,test$not_fully_paid)
#Plot of NN
plot(nn)

nn_classification_error <- 1 - sum(predictions == test$not_fully_paid)/length(predictions)
print("Neural Network classification error:")
print(nn_classification_error)

