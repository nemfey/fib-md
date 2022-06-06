



#We start the Univariate analysis

#Load function to descrive numeric, categoric and date variables
n<-dim(df)[1]
descriptiva<-function(X, nom){
  if (!(is.numeric(X) || class(X)=="Date")){ 
    frecs<-table(as.factor(X), useNA="ifany")
    proportions<-frecs/n
    #ojo, decidir si calcular porcentages con o sin missing values
    pie(frecs, cex=0.6, main=paste("Pie of", nom))
    barplot(frecs, las=3, cex.names=0.7, main=paste("Barplot of", nom), col=listOfColors)
    print(paste("Number of modalities: ", length(frecs)))
    print("Frequency table")
    print(frecs)
    print("Relative frequency table (proportions)")
    print(proportions)
    print("Frequency table sorted")
    print(sort(frecs, decreasing=TRUE))
    print("Relative frequency table (proportions) sorted")
    print(sort(proportions, decreasing=TRUE))
  }else{
    if(class(X)=="Date"){
      print(summary(X))
      print(sd(X))
      #decide breaks: weeks, months, quarters...
      hist(X,breaks="weeks")
    }else{
      hist(X, main=paste("Histogram of", nom))
      boxplot(X, horizontal=TRUE, main=paste("Boxplot of",nom))
      print("Extended Summary Statistics")
      print(summary(X))
      print(paste("sd: ", sd(X, na.rm=TRUE)))
      print(paste("vc: ", sd(X, na.rm=TRUE)/mean(X, na.rm=TRUE)))
    }
  }
}

#(decide the maximum number of colors you can need in a graph based on your metadata file)
listOfColors<-"gray"
par(ask=FALSE)
actives <- c(1:16) #variables to analyse
for(k in actives){
  print(paste("variable ", k, ":", names(df)[k] ))
  descriptiva(df[,k], names(df)[k])
}



#Bivariate description 
##Numeric variables
library(mvoutlier)
library(FactoMineR)
vars_quantitatives <- c(3,4,5,7,8,11,13,14)
res <- cor(df[,vars_quantitatives])
round(res, 2)
library(corrplot)
corrplot(res)
scatterplot(df$Term~df$ApprovalFY)
scatterplot(df$DisbursementGross~df$Term)
scatterplot(df$GrAppv~df$Term)
scatterplot(df$SBA_Appv~df$DisbursementGross)
scatterplot(df$DisbursementGross~df$GrAppv)
scatterplot(df$GrAppv~df$SBA_Appv)
scatterplot(df$RetainedJob ~ df$ApprovalFY)
scatterplot(df$SBA_Appv~df$Term )
scatterplot(df$CreateJob ~df$RetainedJob)

##Categorical vs numeric variables
res.aov <- aov(Term ~ MIS_Status, data = df)
summary(res.aov)
boxplot(df$Term~df$MIS_Status)
#t.test(x=df$MIS_Status, y=df$Term, alternative = "two.sided", var.equal = FALSE)
catdes(df, 12, proba=0.05)
condes(df, 6, proba=0.05)
boxplot(df$ApprovalFY ~ df$MIS_Status)
boxplot(df$Term ~ df$MIS_Status)
boxplot(df$RetainedJob~df$MIS_Status)
boxplot(df$NoEmp ~ df$MIS_Status)
boxplot(df$CreateJob ~ df$MIS_Status)
boxplot(df$RetainedJob ~ df$MIS_Status)
boxplot(df$DisbursementGross~ df$MIS_Status)
boxplot(df$GrAppv ~ df$MIS_Status)
boxplot(df$SBA_Appv ~ df$MIS_Status)


#https://r-charts.com/part-whole/stacked-bar-chart-ggplot2/

# library
library(ggplot2)

##Categorical vs categorical variables

ggplot(data=df, aes(fill = State, x = MIS_Status)) + 
  geom_bar(position = position_dodge()) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x=NULL, y="Numer of businesses") + theme_classic()
freq_table <- table(df$State,df$MIS_Status)
prop.table(freq_table)
prop.table(freq_table, 1) #row percentages
prop.table(freq_table, 2) #column percentages  
#chisq.test(table(df$State,df$MIS_Status))
#res.cat <- catdes(df, 13, proba=0.05)
#res.cat$test.chi2
table(df$BankState)
ggplot(data=df, aes(fill = BankState, x = MIS_Status)) + 
  geom_bar(position = position_dodge()) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x=NULL, y="Numer of businesses") + theme_classic()
freq_table <- table(df$BankState,df$MIS_Status)
prop.table(freq_table, 1) #row percentages
prop.table(freq_table, 2) #column percentages

ggplot(data=df, aes(fill = WhichCompany, x = MIS_Status)) + 
  geom_bar(position = position_dodge()) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x=NULL, y="Numer of businesses") + theme_classic()
freq_table <- table(df$WhichCompany,df$MIS_Status)
prop.table(freq_table, 1) #row percentages
prop.table(freq_table, 2) #column percentages

ggplot(data=df, aes(fill = NewExist, x = MIS_Status)) + 
  geom_bar(position = position_dodge()) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x=NULL, y="Numer of businesses") + theme_classic()
freq_table <- table(df$NewExist,df$MIS_Status)
prop.table(freq_table, 1) #row percentages
prop.table(freq_table, 2) #column percentages

ggplot(data=df, aes(fill = UrbanRural, x = MIS_Status)) + 
  geom_bar(position = position_dodge()) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x=NULL, y="Numer of businesses") + theme_classic()
freq_table <- table(df$UrbanRural,df$MIS_Status)
prop.table(freq_table, 1) #row percentages
prop.table(freq_table, 2) #column percentages

ggplot(data=df, aes(fill = RevLineCr, x = MIS_Status)) + 
  geom_bar(position = position_dodge()) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x=NULL, y="Numer of businesses") + theme_classic()
freq_table <- table(df$RevLineCr,df$MIS_Status)
prop.table(freq_table, 1) #row percentages
prop.table(freq_table, 2) #column percentages





