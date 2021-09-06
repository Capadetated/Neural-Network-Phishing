# This program contains all the R code used to explore the XXX dataset for Assignment 4
# Data 630 Assignment 4
# Ted Fitch
# Last updated 20JUL21
 
# Section 1 - Introduction /////
# Install packages
install.packages("neuralnet")
library("neuralnet")

# Set working directory and read the data
# Read the CSV file.  
setwd("C:/Users/soari/Documents/Assignments/Data Analytics/UMGC/Summer 2021 Data 630/Assignment 4")
# Turn file into an object
PH <- read.csv("phishing.csv", head =TRUE, sep=",", as.is=FALSE)
str(PH)
summary(PH)

# 2. Data Pre-processing
# Remove key variable // no key variable to remove
# Change class from integer into variable in order to cleanly display variables and make graphs (repull data to convert all back to integer when done)
PH$having_IP_Address<-factor(PH$having_IP_Address)
PH$URL_Length<-factor(PH$URL_Length)
PH$Shortining_Service<-factor(PH$Shortining_Service)
PH$having_At_Symbol<-factor(PH$having_At_Symbol)
PH$double_slash_redirecting<-factor(PH$double_slash_redirecting)
PH$Prefix_Suffix<-factor(PH$Prefix_Suffix)
PH$having_Sub_Domain<-factor(PH$having_Sub_Domain)
PH$SSLfinal_State<-factor(PH$SSLfinal_State)
PH$Domain_registeration_length<-factor(PH$Domain_registeration_length)
PH$Favicon<-factor(PH$Favicon)
PH$port<-factor(PH$port)
PH$HTTPS_token<-factor(PH$HTTPS_token)
PH$Request_URL<-factor(PH$Request_URL)
PH$URL_of_Anchor<-factor(PH$URL_of_Anchor)
PH$Links_in_tags<-factor(PH$Links_in_tags)
PH$SFH<-factor(PH$SFH)
PH$Submitting_to_email<-factor(PH$Submitting_to_email)
PH$Abnormal_URL<-factor(PH$Abnormal_URL)
PH$Redirect<-factor(PH$Redirect)
PH$on_mouseover<-factor(PH$on_mouseover)
PH$RightClick<-factor(PH$RightClick)
PH$popUpWidnow<-factor(PH$popUpWidnow)
PH$Iframe<-factor(PH$Iframe)
PH$age_of_domain<-factor(PH$age_of_domain)
PH$DNSRecord<-factor(PH$DNSRecord)
PH$web_traffic<-factor(PH$web_traffic)
PH$Page_Rank<-factor(PH$Page_Rank)
PH$Google_Index<-factor(PH$Google_Index)
PH$Links_pointing_to_page<-factor(PH$Links_pointing_to_page)
PH$Statistical_report<-factor(PH$Statistical_report)
PH$Result<-factor(PH$Result)
str(PH)
summary(PH)

# barplot of result distribution
x<- PH$Result
cnt <- table(x)
cnt
barplot (cnt,main="Distribution of Result",
         xlab="Result (0 = phishing, 1 = legitimate)",
         ylab="Count",
         border="brown",
         col="brown", space =1.0,beside=TRUE,ylim=range(pretty(c(0, cnt))))


# Pull data again to transform all back to integer so NN will work
PH <- read.csv("phishing.csv", head =TRUE, sep=",", as.is=FALSE)

# Replace -1 in result with 0
PH$Result <- as.character(PH$Result)
PH$Result[PH$Result == "-1"] <- "0"
# Check replacement worked
View(PH)
# Return to integer
PH$Result <- as.integer(PH$Result)
str(PH)

# Compute correlation matrix with p-values: see which have highest r and lowest/highest p-values
# install necessary package
install.packages("Hmisc")
library("Hmisc")
# run command
res2 <- Hmisc::rcorr(as.matrix(PH))
# view matrix
res2

# 3. Split the data into training and test set
set.seed(1234)
ind <- sample(2, nrow(PH), replace = TRUE, prob = c(0.7, 0.3))
train.PH <- PH[ind == 1, ]
test.PH <- PH[ind == 2, ]
str(train.PH)
str(test.PH)
View(train.PH)
length(test.PH$Redirect) # 3287 rows
length(train.PH$Redirect)# 7768 rows

# 4. Run the method on the training data
#Build the model. If you receive a warning, rerun the command.
nn<-neuralnet(formula = Result~., data = train.PH, hidden=2, err.fct="sse", linear.output = FALSE)
#names command displays the available neural network properties
names(nn)

# 5. Run the commands to display the network properties
nn$call                  # the command we ran to generate the model
nn$response[1:10]        # actual values of the dependent variable for first 10 records
nn$covariate [1:12,]     # input variables that were used to build the model for first 12 records
nn$model.list            # list dependent and independent variables in the model
nn$net.result[[1]][1:10] # display the first 10 predicted probabilities
nn$weights               # network weights after the last method iteration
nn$startweights          # weights on the first method iteration
nn$result.matrix         # number of trainings steps, the error, and the weights 
plot(nn)                 # plot the network

# 6. Model evaluation; Round the predicted probabilities
mypredict<-compute(nn, nn$covariate)$net.result
mypredict<-apply(mypredict, c(1), round)

# 7. confusion matrix for the training set
table(mypredict, train.PH$Result[0:7768], dnn =c("Predicted", "Actual"))
# Gives model accuracy for this dataset
mean(mypredict==train.PH$Result)

# 8. confusion matrix for the test set
testPred <- compute(nn, test.PH[, 0:30])$net.result
testPred<-apply(testPred, c(1), round)
table(testPred, test.PH$Result, dnn =c("Predicted", "Actual"))
# Gives model accuracy for this dataset
mean(testPred==test.PH$Result)

# 9. New Parameters Model: copying steps 4-8 below remaking the nn model with new parameters
# will only used variables with high r value (> 0.3)
# 9.4. Run the method on a training data
#Build the model. If you receive a warning, rerun the command.
nn2<-neuralnet(formula = Result~ Prefix_Suffix + having_Sub_Domain + SSLfinal_State + URL_of_Anchor + web_traffic, data = train.PH, hidden=2, err.fct="sse", linear.output = FALSE)
#names command displays the available neural network properties
names(nn2)

# 9.5. Run the commands to display the network properties
plot(nn2)                 # plot the network

# 9.6. Model evaluation; Round the predicted probabilities
mypredict<-compute(nn2, nn2$covariate)$net.result
mypredict<-apply(mypredict, c(1), round)

# 9.7. confusion matrix for the training set
table(mypredict, train.PH$Result, dnn =c("Predicted", "Actual"))
# Gives model accuracy for this dataset
mean(mypredict==train.PH$Result)

# 9.8. confusion matrix for the test set
testPred <- compute(nn2, test.PH[, 0:30])$net.result
testPred<-apply(testPred, c(1), round)
table(testPred, test.PH$Result, dnn =c("Predicted", "Actual"))
# Gives model accuracy for this dataset
mean(testPred==test.PH$Result)


# 10. New Parameters Model: copying steps 4-8 below remaking the nn model with new parameters
# will only used variables with low p value (< 0.05)
# 10.4. Run the method on a training data
#Build the model. If you receive a warning, rerun the command.
nn3<-neuralnet(formula = Result~ having_IP_Address + URL_Length + Shortining_Service + having_At_Symbol + double_slash_redirecting + Prefix_Suffix + having_Sub_Domain + SSLfinal_State + Domain_registeration_length + port + HTTPS_token + Request_URL + URL_of_Anchor + Links_in_tags + SFH + Abnormal_URL + Redirect + on_mouseover+ RightClick + age_of_domain + DNSRecord + web_traffic + Page_Rank+ Google_Index + Links_pointing_to_page + Statistical_report,data = train.PH, hidden=2, err.fct="sse", linear.output = FALSE)
#names command displays the available neural network properties
names(nn3)

# 10.5. Run the commands to display the network properties
plot(nn3)                 # plot the network

# 10.6. Model evaluation; Round the predicted probabilities
mypredict<-compute(nn3, nn3$covariate)$net.result
mypredict<-apply(mypredict, c(1), round)

# 10.7. confusion matrix for the training set
table(mypredict, train.PH$Result, dnn =c("Predicted", "Actual"))
# Gives model accuracy for this dataset
mean(mypredict==train.PH$Result)

# 10.8. confusion matrix for the test set
testPred <- compute(nn3, test.PH[, 0:30])$net.result
testPred<-apply(testPred, c(1), round)
table(testPred, test.PH$Result, dnn =c("Predicted", "Actual"))
# Gives model accuracy for this dataset
mean(testPred==test.PH$Result)


# End of script