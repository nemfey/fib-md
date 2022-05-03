#Clean workSpace
if(!is.null(dev.list())) dev.off()  # Clear plots
rm(list=ls())                       # Clean workSpace

#Read data (set your own path)
filepath<-"F:/Documents/MD/Trabajo"
df<-read.csv(paste0(filepath,"/sample_5000.csv"),header=T, sep=",")[-c(1)]

###Some cleaning###

#Delete not interesting columns
df$Name <- NULL
df$Bank <- NULL
df$BalanceGross <- NULL

#Variables from [Char] to Numerical
#DisbursementGross
df$DisbursementGross <- trimws(gsub(",", "", df$DisbursementGross, fixed = T))
df$DisbursementGross <- as.numeric(trimws(gsub("$", "", df$DisbursementGross, fixed = T)))
summary(df$DisbursementGross)

#GrAppv
df$GrAppv <- trimws(gsub(",", "", df$GrAppv, fixed = T))
df$GrAppv <- as.numeric(trimws(gsub("$", "", df$GrAppv, fixed = T)))
summary(df$GrAppv)

#SBA_Appv
df$SBA_Appv <- trimws(gsub(",", "", df$SBA_Appv, fixed = T))
df$SBA_Appv <- as.numeric(trimws(gsub("$", "", df$SBA_Appv, fixed = T)))
summary(df$SBA_Appv)

#Variables from Numerical to Categorical
df$UrbanRural <- factor(df$UrbanRural,levels = c("0", "1", "2"), 
                        labels=c("Undefined","Urban","Rural"))

#Take only first 2 digits (allows us to know type of company)
df$WhichCompany <- substr(sprintf("%0.6d", df$NAICS), 1, 2)


df$NewExist <- factor(df$NewExist,levels = c("0", "1", "2"), 
                      labels=c("Undefined","NotNew","New"))

#RevLineCr
df$RevLineCr <- factor(df$RevLineCr)
df$RevLineCr[which(df$RevLineCr != 'Y' & df$RevLineCr!='N' & df$RevLineCr!=0)] <- 0
table(droplevels(df$RevLineCr))
summary(df$RevLineCr)

#MIS_Status
df$MIS_Status[which(df$MIS_Status == "")] <- NA
del <- which(is.na(df$MIS_Status))
df <- df[-del,]
df$MIS_Status <- factor(df$MIS_Status)
summary(df$MIS_Status)

regions <-list()
regions[[1]] <- c("WA", "OR", "ID", "CA", "NV", "UT", "AZ")
regions[[2]] <- c("MT", "WY", "ND", "SD", "NE", "KS", "CO", "NM", "TX", "OK")
regions[[3]] <- c("MN", "IA", "MO", "WI", "IL", "MI", "IN", "KY", "OH")
regions[[4]] <- c("AR", "LA", "MS", "TN", "AL", "GA", "FL", "SC", "NC")
regions[[5]] <- c("ME", "NH", "VT", "MA", "NY", "RI", "CT", "NJ", "PA", "DE", "WV", "VA", "MD", "DC")
areas <- c("Pacific West", "Plains", "Midwest", "Southeast", "Northeast")

for(i in 1:length(regions)){
  for(ii in 1:length(regions[[i]])){
    df$State[which(df$State == regions[[i]][ii] )] <- areas[i]
  }
}

for(i in 1:length(regions)){
  for(ii in 1:length(regions[[i]])){
    df$BankState[which(df$BankState == regions[[i]][ii] )] <- areas[i]
  }
}

summary(df$UrbanRural)

summary(df$State)
df$State <- factor(df$State)
summary(df$State)

summary(df$BankState)
df$BankState <- factor(df$BankState)
summary(df$BankState)

df$NAICS <- NULL

summary(df)

#####Counting NAs#####
countNA <- function(x) { # Function to count the NA values
  mis_x <- NULL
  for (j in 1:ncol(x)) {mis_x[j] <- sum(is.na(x[,j])) }
  mis_x <- as.data.frame(mis_x)
  rownames(mis_x) <- names(x)
  mis_i <- rep(0,nrow(x))
  for (j in 1:ncol(x)) {mis_i <- mis_i + as.numeric(is.na(x[,j])) }
  list(mis_col=mis_x,mis_ind=mis_i) 
}

mis1<-countNA(df)
mis1$mis_ind # Number of missings for the current set of cars (observations)
mis1$mis_col # Number of missings for the current set of variables

####################

#####Looking for extreme outliers#####
#Quartils
iouts<-rep(0,nrow(df))  # rows - cars
jouts<-rep(0,ncol(df))  # columns - variables
calcQ <- function(x) { # Function to calculate the different quartiles
  s.x <- summary(x)
  iqr<-s.x[5]-s.x[2]
  list(souti=s.x[2]-3*iqr, mouti=s.x[2]-1.5*iqr, min=s.x[1], q1=s.x[2], q2=s.x[3], 
       q3=s.x[5], max=s.x[6], mouts=s.x[5]+1.5*iqr, souts=s.x[5]+3*iqr ) 
}

####################

#ApprovalFY
summary(df$ApprovalFY)
hist(df$ApprovalFY)

#New variable yearsAfterAprv
df$yearsAfterAprv <-  2022 - df$ApprovalFY
summary(df$yearsAfterAprv)
hist(df$yearsAfterAprv)

#Checking if extreme outliers
boxplot(df$yearsAfterAprv, main="Boxplot of years after approval", col="darkslateblue")
summary(df$yearsAfterAprv)
var_out<-calcQ(df$yearsAfterAprv)
abline(h=var_out$souts,col="red")
abline(h=var_out$souti,col="red")

llout<-which((df$yearsAfterAprv >= var_out$souts))
iouts[llout] <- iouts[llout]+1
jouts[which(colnames(df)=="yearsAfterAprv")]<-length(llout)

df$yearsAfterAprv[which((df$yearsAfterAprv >= var_out$souts))] <- NA

summary(df$yearsAfterAprv)
hist(df$yearsAfterAprv)

####################

#Term
summary(df$Term)
hist(df$Term)

#Checking if extreme outliers
boxplot(df$Term, main="Boxplot of Term", col="darkslateblue")
summary(df$Term)
var_out<-calcQ(df$Term)
abline(h=var_out$souts,col="red")
abline(h=var_out$souti,col="red")

llout<-which((df$Term >= var_out$souts))
iouts[llout] <- iouts[llout]+1
jouts[which(colnames(df)=="Term")]<-length(llout)

#Treat extreme outliers
#MEDIA GENERAL O MEDIA PARECIDOS? (de momento inputamos media)
df$Term[which((df$Term >= var_out$souts))] <- NA

summary(df$Term)
hist(df$Term)

####################

#NoEmp
summary(df$NoEmp)
hist(df$NoEmp)

#Checking if extreme outliers
boxplot(df$NoEmp, main="Boxplot of NoEmp", col="darkslateblue")
summary(df$NoEmp)
var_out<-calcQ(df$NoEmp)
abline(h=var_out$souts,col="red")
abline(h=var_out$souti,col="red")

llout<-which((df$NoEmp >= var_out$souts))
iouts[llout] <- iouts[llout]+1
jouts[which(colnames(df)=="NoEmp")]<-length(llout)

#Treat extreme outliers
#MEDIA GENERAL O MEDIA PARECIDOS? (de momento inputamos media)
df$NoEmp[which((df$NoEmp >= var_out$souts))] <- NA

summary(df$NoEmp)
hist(df$NoEmp)

####################

#CreateJob
summary(df$CreateJob)
hist(df$CreateJob)

#Check if extreme outliers
boxplot(df$CreateJob, main="Boxplot of CreateJob", col="darkslateblue")
summary(df$CreateJob)
var_out<-calcQ(df$CreateJob)
abline(h=var_out$souts,col="red")
abline(h=var_out$souti,col="red")

llout<-which((df$CreateJob >= var_out$souts))
iouts[llout] <- iouts[llout]+1
jouts[which(colnames(df)=="CreateJob")]<-length(llout)

#Treat extreme outliers
#MEDIA GENERAL O MEDIA PARECIDOS? (de momento inputamos media)
df$CreateJob[which((df$CreateJob >= var_out$souts))] <- NA

summary(df$CreateJob)
hist(df$CreateJob)

####################

#RetainedJob
summary(df$RetainedJob)
hist(df$RetainedJob)

#Check if extreme outliers
boxplot(df$RetainedJob, main="Boxplot of RetainedJob", col="darkslateblue")
summary(df$RetainedJob)
var_out<-calcQ(df$RetainedJob)
abline(h=var_out$souts,col="red")
abline(h=var_out$souti,col="red")

llout<-which((df$RetainedJob >= var_out$souts))
iouts[llout] <- iouts[llout]+1
jouts[which(colnames(df)=="RetainedJob")]<-length(llout)

#Treat extreme outliers
#MEDIA GENERAL O MEDIA PARECIDOS? (de momento inputamos media)
df$RetainedJob[which((df$RetainedJob >= var_out$souts))] <- NA

summary(df$RetainedJob)
hist(df$RetainedJob)

####################

#DisbursementGross
summary(df$DisbursementGross)
hist(df$DisbursementGross)

#Check if extreme outliers
boxplot(df$DisbursementGross, main="Boxplot of DisbursementGross", col="darkslateblue")
summary(df$DisbursementGross)
var_out<-calcQ(df$DisbursementGross)
abline(h=var_out$souts,col="red")
abline(h=var_out$souti,col="red")

llout<-which((df$DisbursementGross >= var_out$souts))
iouts[llout] <- iouts[llout]+1
jouts[which(colnames(df)=="DisbursementGross")]<-length(llout)

#Treat extreme outliers
#MEDIA GENERAL O MEDIA PARECIDOS? (de momento inputamos media)
df$DisbursementGross[which((df$DisbursementGross >= var_out$souts))] <- NA

summary(df$DisbursementGross)
hist(df$DisbursementGross)

####################

#GrAppv
summary(df$GrAppv)
hist(df$GrAppv)

#Check if extreme outliers
boxplot(df$GrAppv, main="Boxplot of GrAppv", col="darkslateblue")
summary(df$GrAppv)
var_out<-calcQ(df$GrAppv)
abline(h=var_out$souts,col="red")
abline(h=var_out$souti,col="red")

llout<-which((df$GrAppv >= var_out$souts))
iouts[llout] <- iouts[llout]+1
jouts[which(colnames(df)=="GrAppv")]<-length(llout)

#Treat extreme outliers
#MEDIA GENERAL O MEDIA PARECIDOS? (de momento inputamos media)
df$GrAppv[which((df$GrAppv >= var_out$souts))] <- NA

summary(df$GrAppv)
hist(df$GrAppv)

####################

#SBA_Appv
summary(df$SBA_Appv)
hist(df$SBA_Appv)

#Check if extreme outliers
boxplot(df$SBA_Appv, main="Boxplot of SBA_Appv", col="darkslateblue")
summary(df$SBA_Appv)
var_out<-calcQ(df$SBA_Appv)
abline(h=var_out$souts,col="red")
abline(h=var_out$souti,col="red")

llout<-which((df$SBA_Appv >= var_out$souts))
iouts[llout] <- iouts[llout]+1
jouts[which(colnames(df)=="SBA_Appv")]<-length(llout)

#Treat extreme outliers
#MEDIA GENERAL O MEDIA PARECIDOS? (de momento inputamos media)
df$SBA_Appv[which((df$SBA_Appv >= var_out$souts))] <- NA

summary(df$SBA_Appv)
hist(df$SBA_Appv)

#################

summary(df)

# IMPUTATION By THE 1NN

library(class)

# FOR EVERY INDIVIDUAL WITH MISSING Incomes LOOK FOR THE MOST SIMILAR INDIVIDUAL 
# wrt REMAINING VARIABLES AND COPY THE VALUE OF INGRESSOS ON THE FIRST 
#For more robustness average the values of k-NN in general (with small k)

#For several Variables: 

#built indexes of numerical variables that require inputation
uncompleteVars <- c(4,5,7,8,11,13,14)

summary(df)

fullVariables <- c(3,16)

aux<-df[,fullVariables]
dim(aux)
names(aux)

for (k in uncompleteVars){
  aux1 <- aux[!is.na(df[,k]),]
  dim(aux1) 
  aux2 <- aux[is.na(df[,k]),]
  dim(aux2)
  
  RefValues<- df[!is.na(df[,k]),k]
  #Find nns for aux2
  knn.values = knn(aux1,aux2,RefValues)   
  
  #CARE: neither aux1 nor aux2 can contain NAs
  
  
  #CARE: knn.ing is generated as a factor 
  #Be sure to retrieve the correct values
  
  df[is.na(df[,k]),k] = as.numeric(as.character(knn.values))
  fullVariables<-c(fullVariables, k)
  aux<-df[,fullVariables]
}

mean(df$SBA_Appv)
#144820.1 (Antes de Knn y NAs)

#aprox 115000 (Despues de Knn)

dim(df)
summary(df)
