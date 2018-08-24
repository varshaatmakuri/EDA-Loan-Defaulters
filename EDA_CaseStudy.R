
#********************************* EDA CASE STUDY - FACTORS INFLUENCING LOAN DEFAULT **************#
#  Goal of this analysis to determine strong indicators that are more representative of loan       #
#  repayment being defaulted which eventually cause financial loss to lending company.             #
#  We also tend to analyse correlation or dependency between two or more relevant variables        #
#  to identify the areas that are more likely to cause customers getting into "Charged off"        #
#**************************************************************************************************#
# Installing required packages.

if(!require(ggplot2)) {
  install.packages("ggplot2");
}

if(!require(stringi)) {
  install.packages("stringi");
}

if(!require(stringr)) {
  install.packages("stringr");
}

if(!require(tidyr)) {
  install.packages("tidyr");
}

if(!require(dplyr)) {
  install.packages("dplyr");
}

if(!require(formattable)) {
  install.packages("formattable");
}

if(!require(ggrepel)) {
  install.packages("ggrepel");
}


if(!require(corrplot)) {
  install.packages("corrplot");
}


library(ggplot2)
library(stringi)
library(stringr)
library(tidyr)
library(dplyr)
library(formattable)
library(ggrepel)
library(corrplot)


Loan_master <- read.csv("loan.csv",stringsAsFactors = FALSE)

#---------------- The below step is to identify the most relevant fields (Based on Business understanding) from master file  *********#
# Extract only relevant fields for analysis. We care about only "Charged off" records #

Loan_EDA    <- Loan_master %>% subset(loan_status == "Charged Off",select = c( "id"
                                                                             , "installment"
                                                                             , "int_rate"
                                                                             , "inq_last_6mths"
                                                                             , "home_ownership"
                                                                             , "grade"
                                                                             , "funded_amnt"
                                                                             , "emp_length"
                                                                             , "dti"
                                                                             , "application_type"
                                                                             , "annual_inc"
                                                                             , "loan_amnt"
                                                                             , "loan_status"
                                                                             , "open_acc"
                                                                             , "purpose"
                                                                             , "term"
                                                                             , "title"
                                                                             , "revol_util"
                                                                             , "verification_status"
                                                                             , "addr_state"
                                                                             , "earliest_cr_line"
                                                                             , "issue_d"
                                                                             ))

str(Loan_EDA)

#*************** Check if duplicate records in the Loan ID field which is supposed to be unique **************#

nrow(Loan_EDA)
length(Loan_EDA$id)
length(unique(Loan_EDA$id))

# ***********************************************************************************************************#
#                                      DATA CLEANING!!!!!
#      Check if any missing values in any of the records against the fields or variables we care about     *#
#***********************************************************************************************************#

sum(is.na(Loan_EDA))

colSums(Loan_EDA == "n/a")

Emp_length_na <- (228/nrow(Loan_EDA)) * 100
Emp_length_na

# 4% missing values in Emp_Length field. Therefore, we can impute them with 0 for further analysis.

Loan_EDA$emp_length[which(Loan_EDA$emp_length == "n/a")] <- 0
length(Loan_EDA$emp_length[which(Loan_EDA$emp_length == "0")])

#************ Computing the credit length ***************************************************************#

#Standardizing of date formats

#-  Earliest Credit Line

Loan_EDA$earliest_cr_line1 <- as.POSIXct(paste('01-',Loan_EDA$earliest_cr_line), format="%d- %b-%y")
Loan_EDA$earliest_cr_line  <- Loan_EDA$earliest_cr_line1;

# Removing the temporary columns
Loan_EDA$earliest_cr_line1 <- NULL;

#Extracting the year for computing the credit length
year                    <- as.numeric(strftime(Loan_EDA$earliest_cr_line, "%Y"))
year[year > 2018]       <- year[year > 2018] -100
Loan_EDA$Credit_Length  <- 2018 - year


#-  Loan Issue date

Loan_EDA$issue_d1       <- as.POSIXct(paste('01-',Loan_EDA$issue_d), format="%d- %b-%y")
Loan_EDA$issue_d        <- Loan_EDA$issue_d1
Loan_EDA$issue_d1       <- NULL

#Extracting the year from issue date
Loan_EDA$Issue_Year              <- as.numeric(strftime(Loan_EDA$issue_d, "%Y"))
Loan_EDA$Issue_Year              <- as.factor(Loan_EDA$Issue_Year)

#----- Year wise Loan Default count

Loan_Def_Cnt_Year <- ggplot(Loan_EDA, aes(Loan_EDA$Issue_Year)) + geom_bar(stat = "count", fill='coral')
Loan_Def_Cnt_Year <- Loan_Def_Cnt_Year + geom_text(aes(label = ..count..),stat = "count", vjust = -0.5)
Loan_Def_Cnt_Year <- Loan_Def_Cnt_Year + labs (x = "Year", y = "count")
Loan_Def_Cnt_Year

#-- Histogram for Credit Length frequency distribution

ggplot(Loan_EDA,aes( x = Loan_EDA$Credit_Length)) + geom_bar(stat = "count", fill='coral') + labs ( x = "Credit_Length");


#From the distribution of credit length, it can be inferred that there are three distinct patterns
#Starters    - Started the first credit line recently
#Experienced - Started the first credit line between 10 to 25 years ago
#Veterans    - Started the first credit line 25 years ago

Loan_EDA$Credit_Period   <- as.factor(ifelse(Loan_EDA$Credit_Length < 10, "Starter",ifelse(Loan_EDA$Credit_Length < 25,"Experienced", "Veteran")))
ggplot(Loan_EDA,aes( x = Loan_EDA$Credit_Period)) + geom_bar(stat = "count",fill='coral') + labs ( x = "Credit Period") + geom_text(aes(label = ..count..),stat = "count", vjust = -0.5)

#************ Manipulate the data to get rid of unneeded characters to ease the analysis ******************#

Loan_EDA$int_rate        <- as.numeric(str_replace_all(Loan_EDA$int_rate,"%",""))


#******************************** Convert character fields with finite values as factors to make them  categorical *****#
Loan_EDA$home_ownership      <- as.factor(Loan_EDA$home_ownership)
Loan_EDA$grade               <- as.factor(Loan_EDA$grade)
Loan_EDA$emp_length          <- as.factor(Loan_EDA$emp_length)
Loan_EDA$application_type    <- as.factor(Loan_EDA$application_type)
Loan_EDA$term                <- as.factor(Loan_EDA$term)

Loan_EDA$purpose             <- as.factor(Loan_EDA$purpose)
Loan_EDA$verification_status <- as.factor(Loan_EDA$verification_status)
Loan_EDA$title               <- as.factor(Loan_EDA$title)

#****************** Let us bucket the interest rate under Low/Medium/High buckets *************************#

Loan_EDA$int_rate_type   <- as.factor(ifelse(Loan_EDA$int_rate < 10,"Low",ifelse(Loan_EDA$int_rate < 16,"Medium", "High")))

#*************************** Removal of "<", "Years" characters from Emp_Length field *********************#

Loan_EDA$emp_length      <- str_replace_all(str_replace_all(Loan_EDA$emp_length,"[<]","-"),"[+yearyears]","")
Loan_EDA$emp_length      <- str_replace_all(Loan_EDA$emp_length,"- 1","0.5")

Loan_EDA$term            <- str_replace_all(Loan_EDA$term,"[months]","")
Loan_EDA$revol_util      <- str_replace_all(Loan_EDA$revol_util,"[%]","")  

#---------------- Total number of loan records by status ------------------------#

Total_by_Loan_Status <- ggplot(Loan_master,aes(x = Loan_master$loan_status,fill = Loan_master$loan_status))
Total_by_Loan_Status <- Total_by_Loan_Status + geom_bar(stat = "count")
Total_by_Loan_Status <- Total_by_Loan_Status + geom_text(aes(label = ..count..),vjust = -0.5,stat = "count")
Total_by_Loan_Status <- Total_by_Loan_Status + labs (x = "Loan Status", y = "Number of Loan Records", fill = "Loan Status") 
Total_by_Loan_Status

#***************************** Identify ratio of individual loan status to the total ************#

Loan_status_Num <- ifelse(Loan_master$loan_status == "Charged Off", 1,0)
round(mean(Loan_status_Num) * 100)   # 14% Loan Records are Default Records

Loan_status_Num <- ifelse(Loan_master$loan_status == "Fully Paid", 1,0)
round(mean(Loan_status_Num) * 100)   # 83% Loan Records are coming under Fully Paid category

#-- Interest rate frequency distribution

ggplot(Loan_EDA,aes( x = Loan_EDA$int_rate)) + geom_histogram(binwidth = 2,fill='coral') + labs ( x = "Interest Rate")

#***** It is clear from the plot that interest rate between 10 and 20% have contributed more to loan default #

#******************************* Density plots on quantitative variables ********************************#

ggplot(Loan_EDA,aes(Loan_EDA$installment)) + geom_density(fill='coral') + labs( x = "Installment")    # Left Skewed
ggplot(Loan_EDA,aes(Loan_EDA$int_rate)) + geom_density(fill='coral') + labs( x = "Interest Rate")     # Skewed towards middle
ggplot(Loan_EDA,aes(Loan_EDA$loan_amnt)) + geom_density(fill='coral') + labs( x = "Loan Amount")      # Left Skewed
ggplot(Loan_EDA,aes(Loan_EDA$funded_amnt)) + geom_density(fill='coral') + labs( x = "Funded Amount")  # Left Skewed
ggplot(Loan_EDA,aes(Loan_EDA$open_acc)) + geom_density(fill='coral') + labs( x = "Open Acc")          # Left Skewed
ggplot(Loan_EDA,aes(Loan_EDA$dti)) + geom_density(fill='coral') + labs( x = "DTI")                    # Almost normal distribution

# Let's do univariate analysis on categorical variables

#-- Count by Address state

ggplot(Loan_EDA,aes(x = reorder(Loan_EDA$addr_state,Loan_EDA$id,length))) + geom_bar(stat = "count",fill='coral') + labs (x = "Address State") + coord_flip()

# California state appears to have topped the list where number of defaulters are high.

#--  Count by Interest Rate group (Low, Medium, High) across years.

ggplot(Loan_EDA,aes(Loan_EDA$int_rate_type)) + geom_bar(stat = "count",fill='coral') + labs (x = "Interest_Rate_Type") + facet_wrap(~Loan_EDA$Issue_Year,nrow = 2, ncol = 3) + geom_text(aes(label = ..count..),stat = "count",vjust = -0.20)

#--  Count by Interest Rate group (Low, Medium, High) across years.

ggplot(Loan_EDA, aes(Loan_EDA$int_rate_type)) + geom_bar(stat = "count",fill='coral') + labs (x = "Interest_Rate_Type");

#  Medium interest rated loans have more number of loan defaults.


#-- Count by Employee Length

ggplot(Loan_EDA,aes(Loan_EDA$emp_length)) + geom_bar(stat = "count",fill = "coral") + labs (x = "Emp_Length") + geom_text(aes(label = ..count..),stat = "count", vjust = -0.5)

# Borrowers with having 10+ or less than one year of employment length are more prone to loan default.

#-- Count by Home ownership

ggplot(Loan_EDA,aes(Loan_EDA$home_ownership)) + geom_bar(stat = "count",fill = "coral") + labs (x = "Home Ownership") + geom_text(aes(label = ..count..),stat= "count",vjust = -0.5)

# Borrowers who have taken loan for mortgage or rented the house are highest number of loan defaults.


#-- Count by Inquiry in last 6 months variable

ggplot(Loan_EDA,aes(Loan_EDA$inq_last_6mths)) + geom_bar(stat = "count",fill='coral') + labs (x = "Inquiry_Last_6 Months") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.5)

# Those who have not inquired about loan are more representative for loan defaults.


#-- Count by Grade 

ggplot(Loan_EDA,aes(Loan_EDA$grade)) + geom_bar(stat = "count",fill='coral') + labs (x = "Grade") + geom_text(aes(label = ..count..),stat = "count",vjust = -0.5)

# Grade B,c,D and E are having high number of default in the order of magnitude.

#-- Count by Verification status

ggplot(Loan_EDA,aes(Loan_EDA$verification_status)) + geom_bar(stat = "count",fill='coral') + labs (x = "Verification Status") + geom_text(aes(label = ..count..),stat = "count",vjust = -0.5)

2142/nrow(Loan_EDA) * 100  # 38% of defaulted loan are from the ones whose source of income not verified
2052/nrow(Loan_EDA) * 100  # 36% of defaulted loan are from the ones whose income was verified
1434/nrow(Loan_EDA) * 100  # 25% of defaulted loan records have source of income verified.

#-- Count by Purpose

Loan_by_Purpose <- ggplot(Loan_EDA,aes(x = reorder(Loan_EDA$purpose,Loan_EDA$id,length))) + geom_bar(stat= "count",fill='coral') + labs ( x = "Loan Purpose", y = "Count")
Loan_by_Purpose <- Loan_by_Purpose + geom_text(aes(label = paste(round((..count..)/sum(..count..) * 100),"%")),stat = "count",hjust = -0.1) 
Loan_by_Purpose <- Loan_by_Purpose + coord_flip()
Loan_by_Purpose

#  Box Plot - Interest Rate Vs Annual Income

boxplot(log="y",Loan_EDA$annual_inc ~ Loan_EDA$int_rate_type, Loan_EDA, xlab="Interest rate", ylab="Annual Income", col="lightblue2") ;

# Box Plot - Interest Rate Vs DTI

boxplot(Loan_EDA$dti ~ Loan_EDA$int_rate_type, Loan_EDA,  xlab="Interest rate", ylab="DTI", col="lightblue2")

# Boxplot - Interest Rate Vs Loan Amount

boxplot(Loan_EDA$loan_amnt ~ Loan_EDA$int_rate_type, Loan_EDA, xlab="Interest rate", ylab="Loan Amount", col="lightblue2")

#**********  Get rid of outliers in terms of Annual Income  ********************#

P <- c(1:100)/100
Percent_Quant <-  quantile(Loan_EDA$annual_inc,probs = P)

Loan_EDA <- Loan_EDA [Loan_EDA$annual_inc < Percent_Quant[97],]

#-------- It is clear that debt_consolidation tops the list in terms of its ratio with total defaulted loans.
#--------- Let's create the subset with top 5 purposes which attributed to loan default for further analysis.

Loan_EDA_Top5         <- subset(Loan_EDA, Loan_EDA$purpose == "debt_consolidation" |
                                          Loan_EDA$purpose == "credit_card"        |
                                          Loan_EDA$purpose == "small_business"     |
                                          Loan_EDA$purpose == "home_improvement"   |
                                          Loan_EDA$purpose == "major_purchase")

                                      
tapply(Loan_EDA_Top5$int_rate,Loan_EDA_Top5$purpose,mean)
tapply(Loan_EDA_Top5$int_rate,Loan_EDA_Top5$grade,mean)

#-- Percentage of records across different loan terms.

ggplot(Loan_EDA_Top5,aes(Loan_EDA_Top5$term, y = (..count..)/sum(..count..)*100)) + geom_bar(stat = "count",fill='coral') + labs ( x = "Loan Term", y = "Percentage") + geom_text(aes(label = paste(round((..count..)/sum(..count..)*100),"%")),stat = "count",vjust = -0.5)

#**********  55% loan defaulted under the loan term 36 months
#**********  45% loan defaulted under the loan term 60 months for which loan purpose lies in top 5 loan purposes.


# - Bi-Variate Analysis

# Employee Length + Term of Loan

EmplVsTerm <- ggplot(Loan_EDA_Top5, aes(x = Loan_EDA_Top5$emp_length, y = (..count..)/sum(..count..) * 100,fill = Loan_EDA_Top5$term)) + geom_bar(stat = "count",position = "dodge")
EmplVsTerm <- EmplVsTerm + labs ( x = "Employee Length", y = "Percentage", fill = "Loan Term")
EmplVsTerm <- EmplVsTerm + facet_wrap(~purpose,nrow = 5, ncol = 1)
EmplVsTerm

# Interest Rate Vs Term

TermVsIntrate <- ggplot(Loan_EDA_Top5, aes(x = Loan_EDA_Top5$int_rate_type, y = (..count..)/sum(..count..) * 100, fill = Loan_EDA_Top5$term)) + geom_bar(stat = "count",position = "dodge")
TermVsIntrate <- TermVsIntrate + labs ( x = "Interest Rate", y = "Percentage", fill = "Term")  + facet_wrap(~purpose, nrow = 2, ncol = 3)
TermVsIntrate

# Purpose Vs Interest Rate

PurposeVsIntrate <- ggplot(Loan_EDA_Top5, aes(x = Loan_EDA_Top5$int_rate_type,fill = Loan_EDA_Top5$int_rate_type)) + geom_bar(stat = "count") + facet_wrap(~Loan_EDA_Top5$purpose)
PurposeVsIntrate <- PurposeVsIntrate + geom_text(aes(label = paste(round((..count..)/sum(..count..) * 100),"%")), stat = "count", vjust = -0.25)
PurposeVsIntrate <- PurposeVsIntrate + labs ( x = "Interest Rate", y = "Count", fill = "Interest Rate")
PurposeVsIntrate

# Grade Vs Purpose

GradeVsPurpose   <- ggplot(Loan_EDA_Top5, aes(x = Loan_EDA_Top5$grade,y = (..count..)/sum(..count..) *100, fill = Loan_EDA_Top5$int_rate_type)) + geom_bar(stat = "count",position = "dodge")
GradeVsPurpose   <- GradeVsPurpose + facet_wrap(~Loan_EDA_Top5$purpose, nrow = 5,ncol = 1)
GradeVsPurpose   <- GradeVsPurpose + labs ( x = "Grade", y = "Percentage", fill = "Interest Rate")
GradeVsPurpose


home_ownershipVsPurpose <- ggplot(Loan_EDA_Top5, aes(x = Loan_EDA_Top5$home_ownership, y = (..count..)/sum(..count..) * 100,fill = Loan_EDA_Top5$int_rate_type)) + geom_bar(stat = "count",position = "dodge")
home_ownershipVsPurpose <- home_ownershipVsPurpose + facet_wrap(~Loan_EDA_Top5$purpose,nrow = 5, ncol = 1)
home_ownershipVsPurpose <- home_ownershipVsPurpose + labs( x = "Home Ownership", y = "Percentage", fill = "Interest Rate")
home_ownershipVsPurpose

Credit_LengthVsPurpose  <- ggplot(Loan_EDA_Top5, aes(x = Loan_EDA_Top5$Credit_Period, y= (..count..)/sum(..count..) * 100, fill = Loan_EDA_Top5$int_rate_type)) + geom_bar(stat = "count", position = "dodge")
Credit_LengthVsPurpose  <- Credit_LengthVsPurpose + labs (x = "Credit Length", y = "Percentage", fill = "Interest Rate") + facet_wrap(~Loan_EDA_Top5$purpose)
Credit_LengthVsPurpose


VerifyStVsPurpose       <- ggplot(Loan_EDA_Top5, aes(x = Loan_EDA_Top5$verification_status, y = (..count..)/sum(..count..) * 100, fill = Loan_EDA_Top5$int_rate_type)) + geom_bar(stat = "count",position = "dodge")
VerifyStVsPurpose       <- VerifyStVsPurpose + labs (x = "Verification Status", y = "Percentage", fill = "Interest Rate") + facet_wrap(~Loan_EDA_Top5$purpose,nrow = 5, ncol = 1)
VerifyStVsPurpose

OpenaccVsPurpose        <- ggplot(Loan_EDA_Top5,aes(x = Loan_EDA_Top5$open_acc, fill = Loan_EDA_Top5$int_rate_type)) + geom_bar(stat = "count")
OpenaccVsPurpose        <- OpenaccVsPurpose + labs ( x = "Open_Acc", y = "Count", fill = "Interest Rate") + facet_wrap(~Loan_EDA_Top5$purpose)
OpenaccVsPurpose

#********** Let's analyse the correlation between quantitative/continuous variables *********#

Loan_EDA_Cont_Fields <- Loan_EDA_Top5[sapply(Loan_EDA_Top5,class) != "factor"] 
Loan_EDA_Cont_Fields$loan_status      <- NULL
Loan_EDA_Cont_Fields$addr_state       <- NULL
Loan_EDA_Cont_Fields$earliest_cr_line <- NULL
Loan_EDA_Cont_Fields$issue_d          <- NULL
Loan_EDA_Cont_Fields$revol_util       <- NULL
Loan_EDA_Cont_Fields$emp_length       <- NULL
Loan_EDA_Cont_Fields$term             <- NULL
Loan_EDA_Cont_Fields$id               <- NULL

corrplot(cor(Loan_EDA_Cont_Fields), method = "number", col = c("black","blue"))

Loan_EDA_Top5$term <- as.factor(Loan_EDA_Top5$term)

#--------------------- Below code creates buckets for the fields that when bucketed can be analysed easily #######
Loan_EDA_Top5$Loan_Amt_grp <- as.factor(ifelse(Loan_EDA_Top5$loan_amnt < 5000, "Low", ifelse(Loan_EDA_Top5$loan_amnt < 10000, "Medium", "High")))
Loan_EDA_Top5$Emp_Length_grp <- as.factor(ifelse(Loan_EDA_Top5$emp_length < 1, "Entry Level",ifelse(Loan_EDA_Top5$emp_length < 4, "Junior",ifelse(Loan_EDA_Top5$emp_length < 10, "Senior", "Experienced"))))
Loan_EDA_Top5$Annual_Inc_grp <- as.factor(ifelse(Loan_EDA_Top5$annual_inc < 5000, "Low", ifelse(Loan_EDA_Top5$annual_inc < 10000, "Medium", "High")))
Loan_EDA_Top5$dti_grp <- as.factor(ifelse(Loan_EDA_Top5$dti < 1, "Low", ifelse(Loan_EDA_Top5$dti < 10, "Medium", "High")))
Loan_EDA_Top5$funded_amnt <- as.factor(ifelse(Loan_EDA_Top5$funded_amnt < 5000, "Low", ifelse(Loan_EDA_Top5$funded_amnt < 10000, "Medium", "High")))


#********* Dataframe for all categorical variables *******************************#

Loan_EDA_Cat_Fields <- Loan_EDA_Top5[sapply(Loan_EDA_Top5,class) == "factor"]
Loan_EDA_Cat_Fields$title <- NULL
Loan_EDA_Cat_Fields$funded_amnt <- NULL

Loan_EDA_Cat_Fields$term  <- as.factor(Loan_EDA_Top5$term)


# Routine to calculate mean difference across derived metrics within each categorical variable

Mean_Aggregate <- function (DF,Field) {
  
  A <- data.frame(prop.table((table(Field)) * 100))
  colnames(A)[1] <- Field
  colnames(A)[2] <- "Frequency"
  A <- arrange(A,desc(Frequency))
  return (A [1,2] - A[nrow(A),2])
}


Cat_Order <- function(DF){
  DF$purpose <- NULL
  A  <- data.frame ( sapply(DF, function(x) Mean_Aggregate(DF,as.factor(x))[[1]]))
  A  <- cbind(Name = row.names(A),A)
  rownames(A)   <- NULL
  colnames(A)   < c("Name", "Frequency")
  A             <- arrange(A, desc(A[,2]))
  
  ggplot(data= A, aes( x = reorder(Name,A[,2]),y=A[,2])) +
    geom_bar(position="dodge",stat="identity",fill='purple') + coord_flip() +
    ggtitle("Strong Drivers of Loan Default in Descending Order") + labs(x = "Fields", y = "Frequency")
}

#************ Overall Strong Indicators of Loan Default across all Loan Purposes

Cat_Order(Loan_EDA_Cat_Fields)

#************ Strong Indicators of Loan Default for "Debt Consolidation" *********#

Cat_Order(Loan_EDA_Cat_Fields[which(Loan_EDA_Cat_Fields$purpose == "debt_consolidation"),])

#************ Strong Indicators of Loan Default for "Credit Card" *********#
Cat_Order(Loan_EDA_Cat_Fields[which(Loan_EDA_Cat_Fields$purpose == "credit_card"),])

#************ Strong Indicators of Loan Default for "Small Business" *********#
Cat_Order(Loan_EDA_Cat_Fields[which(Loan_EDA_Cat_Fields$purpose == "small_business"),])

#************ Strong Indicators of Loan Default for "Major Purchase" *********#
Cat_Order(Loan_EDA_Cat_Fields[which(Loan_EDA_Cat_Fields$purpose == "major_purchase"),])

#************ Strong Indicators of Loan Default for "Home Improvement" *********#
Cat_Order(Loan_EDA_Cat_Fields[which(Loan_EDA_Cat_Fields$purpose == "home_improvement"),])

