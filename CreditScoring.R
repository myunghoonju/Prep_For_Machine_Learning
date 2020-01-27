#Credit Scoring practice

library(easypackages)
packages('DT','lattice','knitr','gplots','ggplot2','ClustOfVar',
        'ape','Information','ROCR','caret','rpart',
        'rpart.utils','rpart.plot','randomForest','party',
        'bnlearn','DAAG','vcd','kernlab','neuralnet','lars','glmnet')

-------------
# Function 1: Create function to calculate percent distribution for factors
pct <- function(x){
  tbl <- table(x)
  tbl_pct <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(tbl_pct) <- c('Count','Percentage')
  kable(tbl_pct)
}

# Function 2: Own function to calculate IV, WOE and Eefficiency 
gbpct <- function(x, y=cdata$good_bad_21){
  mt <- as.matrix(table(as.factor(x), as.factor(y))) # x -> independent variable(vector), y->dependent variable(vector)
  Total <- mt[,1] + mt[,2]                          # Total observations
  Total_Pct <- round(Total/sum(mt)*100, 2)          # Total PCT
  Bad_pct <- round((mt[,1]/sum(mt[,1]))*100, 2)     # PCT of BAd or event or response
  Good_pct <- round((mt[,2]/sum(mt[,2]))*100, 2)   # PCT of Good or non-event
  Bad_Rate <- round((mt[,1]/(mt[,1]+mt[,2]))*100, 2) # Bad rate or response rate
  grp_score <- round((Good_pct/(Good_pct + Bad_pct))*10, 2) # score for each group
  WOE <- round(log(Good_pct/Bad_pct)*10, 2)      # Weight of Evidence for each group
  g_b_comp <- ifelse(mt[,1] == mt[,2], 0, 1)
  IV <- ifelse(g_b_comp == 0, 0, (Good_pct - Bad_pct)*(WOE/10)) # Information value for each group
  Efficiency <- abs(Good_pct - Bad_pct)/2                       # Efficiency for each group
  otb<-as.data.frame(cbind(mt, Good_pct,  Bad_pct,  Total, 
                           Total_Pct,  Bad_Rate, grp_score, 
                           WOE, IV, Efficiency ))
  otb$Names <- rownames(otb)
  rownames(otb) <- NULL
  otb[,c(12,2,1,3:11)] # return IV table
}

# Function 3: Normalize using Range
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

-------------

cdata<-read.table("german.data",fileEncoding="UTF-8",dec=",")
cdatanum<-read.table("german.data",fileEncoding="UTF-8",dec=",")
cdatanum<-as.data.frame(sapply(cdatanum,as.numeric))

colnames(cdata) <- c("chk_ac_status_1",
        "duration_month_2", "credit_history_3", "purpose_4",
        "credit_amount_5","savings_ac_bond_6","p_employment_since_7", 
        "instalment_pct_8", "personal_status_9","other_debtors_or_grantors_10", 
        "present_residence_since_11","property_type_12","age_in_yrs_13",
        "other_instalment_type_14", "housing_type_15", 
        "number_cards_this_bank_16","job_17","no_people_liable_for_mntnance_18",
        "telephone_19", "foreign_worker_20", 
        "good_bad_21")

kable(as.data.frame(colnames(cdata))) #check var names
DT::datatable(cdata[1:50,]) #print on web page

cdata$duration_month_2  <- as.numeric(cdata$duration_month_2)
cdata$credit_amount_5   <-  as.numeric(cdata$credit_amount_5 )
cdata$instalment_pct_8 <-  as.numeric(cdata$instalment_pct_8)
cdata$present_residence_since_11 <-  as.numeric(cdata$present_residence_since_11)
cdata$age_in_yrs_13        <-  as.numeric(cdata$age_in_yrs_13)
cdata$number_cards_this_bank_16    <-  as.numeric(cdata$number_cards_this_bank_16)
cdata$no_people_liable_for_mntnance_18 <-  as.numeric(cdata$no_people_liable_for_mntnance_18)
cdata$good_bad_21<-as.factor(ifelse(cdata$good_bad_21 == 1, "Good", "Bad"))

pct(cdata$good_bad_21)

#Status check: Good, Bad
#Status of existing checking account
#A11 :      ... <    0 DM
#A12 : 0 <= ... <  200 DM
#A13 :... >= 200 DM /salary assignments for at least 1 year
#A14 : no checking account

A1<-gbpct(cdata$chk_ac_status_1)

##plot
op1<-par(mfrow=c(1,2), new=TRUE)
plot(cdata$chk_ac_status_1,cdata$good_bad_21,
    ylab="Good_Bad", xlab="category",
    main="Checking Account Status ~ Good_Bad")

barplot(A1$WOE,col="brown",names.arg=c(A1$Levels),
    main="Score:Checking Account Status",
    xlab="Category",
    ylab="WOE")
##

kable(A1,caption="check Acc Status ~ Good_Bad")

summary(cdata$duration_month_2)

#Load Duration
op2<-par(mfrow=c(1,2))
boxplot(cdata$duration_month_2, ylab="Loan Duration(Month)", main="Boxplot:Loan Duration")

plot(cdata$duration_month_2, cdata$good_bad_21, 
     ylab="Good-Bad", xlab="Loan Duration(Month)",
     main="Loan Duration ~ Good-Bad ")

#Groups from continuous vars
plot(as.factor(cdata$duration_month_2),cdata$good_bad_21,
    ylab="Good_Bad", xlab="Loan Duration(Month)",
    main="Load Duration(Before Group)")

cdata$duration_month_2<-as.factor(
    (ifelse(cdata$duration_month_2<=6,'00-06',
        ifelse(cdata$duration_month_2<=12,'06-12',
            ifelse(cdata$duration_month_2<=24,'12-24',
                ifelse(cdata$duration_month_2<=30,'24-30',
                    ifelse(cdata$duration_month_2<=36,'30-36',
                        ifelse(cdata$duration_month_2<=42,'36-42','42+'
                        )
                       )
                      )
                     )
                    )
                   )
				)
			   )
plot(cdata$duration_month_2,cdata$good_bad_21,
    main="Loan Duration(after group)",
    xlab="Loan Duration(Month)",
    ylab="Good_Bad"
)

A2<-gbpct(cdata$duration_month_2)

barplot(A2$WOE, col="red",names.arg=c(A2$Levels),
    main="Loan Duration",
    xlab="Duration(Month)",
    ylab="WOE"
)

kable(A2, caption="LoanDuration~Good/Bad")

#Credit Hist

#A30: no credits taken / all credits paid back duly
#A31: all credits at this bank paid back duly
#A32: existing credits paid back duly till now
#A33: delay in paying off in the past
#A34: critical acc / other credits existing(not at this bank)

#group based on WOE, bad rates
cdata$credit_history_3<-as.factor(ifelse(cdata$credit_history_3 == "A30", "01.A30",
                                         ifelse(cdata$credit_history_3 == "A31","02.A31",
                                                ifelse(cdata$credit_history_3 == "A32","03.A32.A33",
                                                       ifelse(cdata$credit_history_3 == "A33","03.A32.A33",
                                                              "04.A34")))))

op3<-par(mfrow=c(1,2))
plot(cdata$credit_history_3, cdata$good_bad_21, 
      main = "Credit History ~ Good-Bad",
     xlab = "Credit History",
     ylab = "Good-Bad")

plot(cdata$credit_history_3, cdata$good_bad_21, 
     main = "Credit History(After Groupping) ~ Good-Bad ",
      xlab = "Credit History",
     ylab = "Good-Bad")

par(op3)

A3<-gbpct(cdata$credit_history_3)

barplot(A3$WOE, col="brown", names.arg=c(A3$Levels),
        main="Credit History",
        xlab="Credit History",
        ylab="WOE"
)

kable(A3, caption = "Credit History~ Good-Bad")

# Purpose of the loan

#A40 : car (new)
#A41 : car (used)
#A42 : furniture/equipment
#A43 : radio/television
#A44 : domestic appliances
#A45 : repairs
#A46 : education
#A47 : (vacation - does not exist?)
#A48 : retraining
#A49 : business
#A410: others


A4<-gbpct(cdata$purpose_4)


op4<-par(mfrow=c(1,2))
plot(cdata$purpose_4, cdata$good_bad_21, 
     main="Purpose of Loan~ Good-Bad ",
     xlab="Purpose",
     ylab="Good-Bad")

barplot(A4$WOE, col="brown", names.arg=c(A4$Levels),
        main="Purpose of Loan",
        xlab="Category",
        ylab="WOE")

par(op4)
kable(A4, caption = "Purpose of Loan~ Good-Bad")

# Credit (Loan) Amount
cdata$credit_amount_5 <- as.double(cdata$credit_amount_5)
summary(cdata$credit_amount_5)

# Create groups based on their distribution
cdata$credit_amount_5<-as.factor(ifelse(cdata$credit_amount_5<=1400,'0-1400',
                                        ifelse(cdata$credit_amount_5<=2500,'1400-2500',
                                               ifelse(cdata$credit_amount_5<=3500,'2500-3500', 
                                                      ifelse(cdata$credit_amount_5<=4500,'3500-4500',
                                                             ifelse(cdata$credit_amount_5<=5500,'4500-5500','5500+'))))))


A5<-gbpct(cdata$credit_amount_5)



plot(cdata$credit_amount_5, cdata$good_bad_21, 
      main="Credit Ammount (After Grouping) ~ Good-Bad",
      xlab="Amount",
     ylab="Good-Bad")

barplot(A5$WOE, col="brown", names.arg=c(A5$Levels),
        main="Credit Ammount",
        xlab="Amount",
        ylab="WOE")

kable(A5, caption = "Credit Ammount ~ Good-Bad")


# Savings account/bonds

#A61 :          ... <  100 DM
#A62 :   100 <= ... <  500 DM
#A63 :   500 <= ... < 1000 DM
#A64 :          .. >= 1000 DM
#A65 :   unknown/ no savings account

A6<-gbpct(cdata$savings_ac_bond_6)


plot(cdata$savings_ac_bond_6, cdata$good_bad_21, 
     main="Savings account/bonds ~ Good-Bad",
     xlab="Savings/Bonds",
     ylab="Good-Bad")

barplot(A6$WOE, col="brown", names.arg=c(A6$Levels),
        main="Savings account/bonds",
        xlab="Category",
        ylab="WOE")

kable(A6, caption = "Savings account/bonds ~ Good-Bad" )

# Present employment since

# A71 : unemployed
# A72 :       ... < 1 year
# A73 : 1  <= ... < 4 years
# A74 : 4  <= ... < 7 years
# A75 :       .. >= 7 years

A7<-gbpct(cdata$p_employment_since_7)

op7<-par(mfrow=c(1,2))
plot(cdata$p_employment_since_7, cdata$good_bad_21,
     main="Present employment since ~ Good-Bad",
      xlab="Employment in Years",
     ylab="Good-Bad")

barplot(A7$WOE, col="brown", names.arg=c(A7$Levels),
        main="Present employment",
        xlab="Category",
        ylab="WOE")

par(op7)

kable(A7, caption ="Present employment since ~ Good-Bad")

# instalment rate in percentage of disposable income

summary(cdata$instalment_pct_8)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   2.973   4.000   4.000
op8<-par(mfrow=c(1,2))
boxplot(cdata$instalment_pct_8)
histogram(cdata$instalment_pct_8,
          main = "instalment rate in percentage of disposable income",
          xlab = "instalment percent",
          ylab = "Percent Population")
par(op8)

A8<-gbpct(cdata$instalment_pct_8)

op8_1<-par(mfrow=c(1,2))
plot(as.factor(cdata$instalment_pct_8), cdata$good_bad_21,
     main="instalment rate in percentage of disposable income ~ Good-Bad",
     xlab="Percent",
     ylab="Good-Bad")

barplot(A8$WOE, col="brown", names.arg=c(A8$Levels),
        main="instalment rate",
        xlab="Percent",
        ylab="WOE")


par(op8_1)

kable(A8, caption = "instalment rate in percentage of disposable income ~ Good-Bad")

# Personal status and sex - you may not use for some country due to regulations

#A91 : male   : divorced/separated
#A92 : female : divorced/separated/married
#A93 : male   : single
#A94 : male   : married/widowed
#A95 : female : single

A9<-gbpct(cdata$personal_status_9)

op9<-par(mfrow=c(1,2))
plot(cdata$personal_status_9, cdata$good_bad_21, 
          main=" Personal status",
     xlab=" Personal status",
     ylab="Good-Bad")


barplot(A9$WOE, col="brown", names.arg=c(A9$Levels),
        main="Personal status",
        xlab="Category",
        ylab="WOE")


par(op9)

kable(A9, caption =  "Personal status ~ Good-Bad")

# Other debtors / guarantors
#A101 : none
#A102 : co-applicant
#A103 : guarantor

A10<-gbpct(cdata$other_debtors_or_grantors_10)

op10<-par(mfrow=c(1,2))

plot(cdata$other_debtors_or_grantors_10, cdata$good_bad_21, 
      main="Other debtors / guarantors",
     xlab="Category",
     ylab="Good-Bad")

barplot(A10$WOE, col="brown", names.arg=c(A10$Levels),
        main="Other debtors / guarantors",
        xlab="Category",
        ylab="WOE")

par(op10)

kable(A10, caption = "Other debtors / guarantors ~ Good-Bad")

# Present residence since
summary(cdata$present_residence_since_11)

A11<-gbpct(cdata$present_residence_since_11)

op11<-par(mfrow=c(1,2))
histogram(cdata$present_residence_since_11,
          main="Present Residence~ Good-Bad",
          xlab="Present residence Since", 
          ylab="Percent Population")

barplot(A11$WOE, col="brown", names.arg=c(A11$Levels),
        main="Present Residence",
        xlab="Category",
        ylab="WOE")
par(op11)

kable(A11, caption = "Present Residence~ Good-Bad")

#property
#A121 : real estate
#A122 : if not A121 : building society savings agreement / life insurance
#A123 : if not A121/A122 : car or other, not in attribute 6
#A124 : unknown / no property

A12 <- gbpct(cdata$property_type_12)

op12 <- par(mfrow = c(1,2))
plot(cdata$property_type_12, cdata$good_bad_21, 
     main = "Property Type",
      xlab="Type",
     ylab="Good-Bad")         

barplot(A12$WOE, col="brown", names.arg=c(A12$Levels),
        main="Property Type",
        xlab="Category",
        ylab="WOE")

par(op12)
kable(A12,  caption = "Property Type")

# Age in Years
summary(cdata$age_in_yrs_13)

op13 <- par(mfrow = c(1,2))
boxplot(cdata$age_in_yrs_13)

plot(as.factor(cdata$age_in_yrs_13),  cdata$good_bad_21,
     main = "Age",
     xlab = "Age in Years",
     ylab = "Good-Bad")

par(op13)

# Group AGE - Coarse Classing (after some iterations in fine classing stage)
cdata$age_in_yrs_13 <- as.factor(ifelse(cdata$age_in_yrs_13<=25, '0-25',
                                      ifelse(cdata$age_in_yrs_13<=30, '25-30',
                                             ifelse(cdata$age_in_yrs_13<=35, '30-35', 
                                                    ifelse(cdata$age_in_yrs_13<=40, '35-40', 
                                                           ifelse(cdata$age_in_yrs_13<=45, '40-45', 
                                                                  ifelse(cdata$age_in_yrs_13<=50, '45-50',
                                                                         ifelse(cdata$age_in_yrs_13<=60, '50-60',
                                                                                '60+'))))))))


A13<-gbpct(cdata$age_in_yrs_13)

op13_1<-par(mfrow=c(1,2))
plot(as.factor(cdata$age_in_yrs_13),  cdata$good_bad_21, 
      main="Age (After Grouping)",
     xlab="Other instalment plans",
     ylab="Good-Bad")


barplot(A13$WOE, col="brown", names.arg=c(A13$Levels),
        main="Age",
        xlab="Category",
        ylab="WOE")

par(op13_1)

kable(A13,  caption = "Age (After Grouping) ~ Good-Bad")

# Attribute 14: (qualitative)
#Other instalment plans 
#A141 : bank
#A142 : stores
#A143 : none

A14<-gbpct(cdata$other_instalment_type_14)

op14<-par(mfrow=c(1,2))

plot(cdata$other_instalment_type_14, cdata$good_bad_21, 
     main="Other instalment plans ~ Good-Bad",
     xlab="Other instalment plans",
     ylab="Good-Bad")

barplot(A14$WOE, col="brown", names.arg=c(A14$Levels),
        main="Other instalment plans",
        xlab="Category",
        ylab="WOE")

par(op14)

kable(A14, caption = "Other instalment plans ~ Good-Bad")

# Attribute 15: (qualitative)
#-----------------------------------------------------------
#         Housing
#         A151 : rent
#         A152 : own
#         A153 : for free

A15<-gbpct(cdata$housing_type_15)

op15<-par(mfrow=c(1,2))
plot(cdata$housing_type_15, cdata$good_bad_21, 
      main="Home Ownership Type",
      xlab="Type",
      ylab="Good-Bad")

barplot(A15$WOE, col="brown", names.arg=c(A15$Levels),
        main="Home Ownership Type",
        xlab="Type",
        ylab="WOE")

par(op15)

kable(A15, caption = "Home Ownership Type ~ Good-Bad")

# Attribute 16: (numerical)
#-----------------------------------------------------------
#               Number of existing credits at this bank
summary(cdata$number_cards_this_bank_16)

A16<-gbpct(cdata$number_cards_this_bank_16)

op16<-par(mfrow=c(1,2))
plot(as.factor(cdata$number_cards_this_bank_16), cdata$good_bad_21,
      main="Number of credits at this bank",
      xlab="Number of Cards",
      ylab="Good-Bad")

barplot(A16$WOE, col="brown", names.arg=c(A16$Levels),
        main="Number of credits at this bank",
        xlab="Number of Cards",
        ylab="WOE")

par(op16)

kable(A16, caption = "Number of credits at this bank ~ Good-Bad")

#Attribute 17: (qualitative)
#Job
#A171 : unemployed/ unskilled  - non-resident
#A172 : unskilled - resident
#A173 : skilled employee / official
#A174 : management/ self-employed/highly qualified employee/ officer

A17<-gbpct(cdata$job_17)

op17<-par(mfrow=c(1,2))

plot(cdata$job_17, cdata$good_bad_21, 
     main="Employment Type",
     xlab="Job",
     ylab="Good-Bad")

barplot(A17$WOE, col="brown", names.arg=c(A17$Levels),
        main="Employment Type",
        xlab="Job",
        ylab="WOE")

par(op17)

kable(A17, caption = "Employment Type ~ Good-Bad")

# Attribute 18: (numerical)
#-----------------------------------------------------------
#         Number of people being liable to provide maintenance for

summary(cdata$no_people_liable_for_mntnance_18)

A18<-gbpct(cdata$no_people_liable_for_mntnance_18)

op18<-par(mfrow = c(1,2))

plot(as.factor(cdata$no_people_liable_for_mntnance_18), cdata$good_bad_21, 
        main = "Number of people being liable",
        xlab = "Number of People",
        ylab = "Good-Bad")

barplot(A18$WOE, col = "brown", names.arg=c(A18$Levels),
        main = " Number of people being liable",
        xlab = "Number of People",
        ylab = "WOE")

par(op18)
kable(A18, caption = "Number of people being liable ~ Good-Bad")

#Telephone
#A191 : none
#A192 : yes, registered under the customers name

A19<-gbpct(cdata$telephone_19)

op19<-par(mfrow=c(1,2))

plot(cdata$telephone_19, cdata$good_bad_21, 
     main="Telephone",
     xlab="Telephone(Yes/No)",
     ylab="Good-Bad")

barplot(A19$WOE, col="brown", names.arg=c(A19$Levels),
        main="Telephone",
        xlab="Telephone(Yeas/No)",
        ylab="WOE")

par(op19)
kable(A19, caption = "Telephone ~ Good-Bad")

#foreign worker
#A201 : yes
#A202 : no
A20<-gbpct(cdata$foreign_worker_20)

op20<-par(mfrow=c(1,2))

plot(cdata$foreign_worker_20, cdata$good_bad_21, 
     main="Foreign Worker",
     xlab="Foreign Worker(Yes/No)",
     ylab="Good-Bad")

barplot(A20$WOE, col="brown", names.arg=c(A20$Levels),
        main="Foreign Worker",
        xlab="Foreign Worker(Yes/No)",
        ylab="WOE")

par(op20)

kable(A20,  caption = "Foreign Worker ~ Good-Bad")

# require library(Information) 
cdata$good_bad_21<-as.numeric(ifelse(cdata$good_bad_21 == "Good", 0, 1))
IV <- Information::create_infotables(data=cdata, NULL, y="good_bad_21", 10)
IV$Summary$IV <- round(IV$Summary$IV*100,2)

IV$Tables
kable(IV$Summary)

cdata$good_bad_21<-as.factor(ifelse(cdata$good_bad_21 == 0, "Good", "Bad"))

var_list_1 <- IV$Summary[IV$Summary$IV > 2,] # 15 variables
cdata_reduced_1 <- cdata[, c(var_list_1$Variable,"good_bad_21")] #16 variables

#Multivariate Analysis
# Step 1: Subset quantitative and qualitative variables X.quanti and X.quali
factors <- sapply(cdata_reduced_1, is.factor)

#subset Qualitative variables 
vars_quali <- cdata_reduced_1[,factors]

#vars_quali$good_bad_21<-vars_quali$good_bad_21[drop=TRUE] # remove empty factors
str(vars_quali)

vars_quanti <- cdata_reduced_1[,!factors]
str(vars_quanti)

#Cluster
tree <- hclustvar(X.quanti=vars_quanti,X.quali=vars_quali[,-c(12)])
plot(tree, main="variable clustering")
rect.hclust(tree, k=10,  border = 1:10)

summary(tree)

plot(as.phylo(tree), type = "fan",
     tip.color = hsv(runif(15, 0.65,  0.95), 1, 1, 0.7),
     edge.color = hsv(runif(10, 0.65, 0.75), 1, 1, 0.7), 
     edge.width = runif(20,  0.5, 3), use.edge.length = TRUE, col = "gray80")
summary.phylo(as.phylo(tree))

keep<- c(1:8,12,13,21)
cdata_reduced_2 <- cdata[,keep]
str(cdata_reduced_2)

div_part <- sort(sample(nrow(cdata_reduced_2), nrow(cdata_reduced_2)*.6))

#select training sample 
train<-cdata_reduced_2[div_part,] # 70% here
pct(train$good_bad_21)

# put remaining into test sample
test<-cdata_reduced_2[-div_part,] # rest of the 30% data goes here
pct(test$good_bad_21)

div_part_1 <- createDataPartition(y = cdata_reduced_2$good_bad_21, p = 0.7, list = F)

# Training Sample
train_1 <- cdata_reduced_2[div_part_1,] # 70% here
pct(train_1$good_bad_21)

# Test Sample
test_1 <- cdata_reduced_2[-div_part_1,] # rest of the 30% data goes here
pct(test_1$good_bad_21)

save(train_1, file="train_1.RData")
save(test_1, file="test_1.RData")

# For neural network we would need contious data
# Sampling for Neural Network - It can be used for other modeling as well
div_part_2 <- createDataPartition(y = cdatanum[,21], p = 0.7, list = F)

# Training Sample for Neural Network
train_num <- cdatanum[div_part_2,] # 70% here

# Test Sample for Neural Network
test_num <- cdatanum[-div_part_2,] # rest of the 30% data goes here

# Save for the future
save(train_num, file="train_num.RData")
save(test_num, file="test_num.RData")

# Model: Stepwise Logistic Regression Model
m1 <- glm(good_bad_21~.,data=train_1,family=binomial())
m1 <- step(m1)
summary(m1)

# List of significant variables and features with p-value <0.01
significant.variables <- summary(m1)$coeff[-1,4] < 0.01
names(significant.variables)[significant.variables == TRUE]

prob <- predict(m1, type = "response")
res <- residuals(m1, type = "deviance")

#Plot Residuals
plot(predict(m1), res,
     xlab="Fitted values", ylab = "Residuals",
     ylim = max(abs(res)) * c(-1,1))

## CIs using profiled log-likelihood
confint(m1)

## CIs using standard errors
confint.default(m1)

## odds ratios and 95% CI
exp(cbind(OR = coef(m1), confint(m1)))

#Logistic
m1_1 <- glm(good_bad_21~chk_ac_status_1+duration_month_2
          +savings_ac_bond_6+instalment_pct_8,
          data=train_1,family=binomial())
step(m1_1)
summary(m1_1)

#Tree

# Requires library(rpart)
m2 <- rpart(good_bad_21~.,data=train_1)
# Print tree detail
printcp(m2)

plot(m2, main="Tree:Recursive Partitioning");text(m2);
prp(m2,type=2,extra=1,  main="Tree:Recursive Partitioning")

# Bayes tree

# m2_1 <- rpart(good_bad_21~.,data=train_1,parms=list(prior=c(.9,.1)),cp=.0002) #- build model using 90%-10% priors
# m2_1 <- rpart(good_bad_21~.,data=train_1,parms=list(prior=c(.8,.2)),cp=.0002) #- build model using 80%-20% priors
 m2_1 <- rpart(good_bad_21~.,data=train_1,parms=list(prior=c(.7,.3)),cp=.0002)  #- build model using 70%-30% priors
# m2_1 <- rpart(good_bad_21~.,data=train_1,parms=list(prior=c(.75,.25)),cp=.0002) #- build model using 75%-25% priors
# m2_1 <- rpart(good_bad_21~.,data=train_1,parms=list(prior=c(.6,.4)),cp=.0002) #- build model using 60%-40% priors

# Print tree detail
printcp(m2_1)
# plot trees
plot(m2_1, main="m2_1-Recursive Partitioning - Using Bayesian N 70%-30%");text(m2_1)
prp(m2_1,type=2,extra=1, main="m2_1-Recursive Partitioning - Using Bayesian N 70%-30%")

#Random forest
m3 <- randomForest(good_bad_21 ~ ., data = train_1)

#plot variable importance
varImpPlot(m3, main="Random Forest: Variable Importance")

#Conditional inference tree

