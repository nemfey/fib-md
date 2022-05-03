#alternatively 
#df<- <your-data-frame>

objects()
attributes(df)

#
# VISUALISATION OF DATA
#
# PRINCIPAL COMPONENT ANALYSIS OF CONTINcUOUS VARIABLES, WITH Dictamen PROJECTED AS ILLUSTRATIVE
#

# CREATION OF THE DATA FRAME OF CONTINUOUS VARIABLES

names(df)

#is R understanding well my factor variables?
sapply(df,class)

#set a list of numerical variables (with no missing values)


numeriques<-which(sapply(df,is.numeric))
numeriques

dcon<-df[,numeriques]
sapply(dcon,class)

#be sure you don't have missing data in your numerical variables.

#in case of having missing data, select complete rows JUST TO FOLLOW THE CLASS
#df<-df[!is.na(df[,indecCon[1]])& !is.na(df[,indecCon[2]]) & !is.na(df[,indecCon[3]])& !is.na(df[,indecCon[4]]),]
#then preprocess your complete data set to IMPUTE all missing data, and reproduce
#the whole analysis again
# PRINCIPAL COMPONENT ANALYSIS OF dcon

results <- prcomp(dcon, scale=TRUE)
class(results)
attributes(results) #sdev rotation center scale x

print(results)

# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?

results$sdev
inerProj<- results$sdev^2 
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix <- inerProj/totalIner
pinerEix
barplot(pinerEix)

#SCREE PLOT
library(ggplot2)

qplot(numeriques, pinerEix) + geom_line() +
  xlab("Principal Component") + ylab("Variance Explained") + 
  ggtitle("Scree Plot") + ylim(0,1)

print(pinerEix)

#Cummulated Inertia in subspaces, from first principal component to the 11th dimension subspace
barplot(100*cumsum(results$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2])
percInerAccum<-100*cumsum(results$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2]
percInerAccum


# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)

nd = 4 #>80%

print(results)
attributes(results)
results$rotation

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS
View(results$x) #x = score
dim(results$x)
dim(dcon)

Psi = results$x[,1:nd] #cogemos de la primera columna hasta la dimension deseada
dim(Psi)

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES

iden = row.names(dcon)
etiq = names(dcon)
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS

# PLOT OF INDIVIDUALS

#select your axis
eje1<-1   #more representative
eje2<-2

#Projection of variables
Phi = cor(dcon,Psi) #correlation

View(Phi)

#select your axis

X<-Phi[,eje1]
Y<-Phi[,eje2]

plot(Psi[,eje1],Psi[,eje2],type="n")
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=0.7)


#zooms
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(min(X,0),max(X,0)), ylim=c(-1,1))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="grey")
text(X,Y,labels=etiq,col="black", cex=0.7)

#flecha izq = inverse
#flecha der = direct

###############################
#TODOS JUNTOS

dcat<-c(1,2,6,9,10,12,15)
#divide categoricals in several graphs if joint representation saturates

#build a palette with as much colors as qualitative variables 
colors<-rainbow(length(dcat))


#determine zoom level
fm=20



#represent numerical variables in background
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1.5,1), ylim=c(-1,1))
#plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#adf projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="gray")
text(X,Y,labels=etiq,col="black", cex=0.7)

#adf centroids
c<-1
for(k in dcat){
  seguentColor<-colors[c]
  
  fdic1 = tapply(Psi[,eje1],df[,k],mean)
  fdic2 = tapply(Psi[,eje2],df[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(df[,k]))
  text(fdic1,fdic2,labels=levels(df[,k]),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(df)[dcat],pch=1,col=colors, cex=0.3)




###########################
#BANKSTATE STATE

#represent numerical variables in background
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-2,1), ylim=c(-1,1))
#plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#adf projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="gray")
text(X,Y,labels=etiq,col="black", cex=0.7)

#adf centroids
dcat<-c(1,2)

c<-1
for(k in dcat){
  seguentColor<-colors[c]
  
  fdic1 = tapply(Psi[,eje1],df[,k],mean)
  fdic2 = tapply(Psi[,eje2],df[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(df[,k]))
  text(fdic1,fdic2,labels=levels(df[,k]),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(df)[dcat],pch=1,col=colors, cex=0.5)




##############################
#NEWEXIST

plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1.5,0.5), ylim=c(-1,1))
#plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#adf projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="gray")
text(X,Y,labels=etiq,col="black", cex=0.7)

#adf centroids
dcat<-c(6)

c<-1
seguentColor<-colors[c]
  
fdic1 = tapply(Psi[,eje1],df[,dcat],mean)
fdic2 = tapply(Psi[,eje2],df[,dcat],mean) 
text(fdic1,fdic2,labels=levels(df[,dcat]),col=seguentColor, cex=0.6)
c<-c+1
legend("bottomleft",names(df)[dcat],pch=1,col=colors, cex=0.5)


##########################
#URBANRURAL
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1.5,1), ylim=c(-1,1.5))
#plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#adf projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="gray")
text(X,Y,labels=etiq,col="black", cex=0.7)

#adf centroids
dcat<-c(9)

c<-1
seguentColor<-colors[c]

fdic1 = tapply(Psi[,eje1],df[,dcat],mean)
fdic2 = tapply(Psi[,eje2],df[,dcat],mean) 
text(fdic1,fdic2,labels=levels(df[,dcat]),col=seguentColor, cex=0.6)
c<-c+1
legend("bottomleft",names(df)[dcat],pch=1,col=colors, cex=0.5)



##########################
#REVLINECR
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-2,2), ylim=c(-1,1))
#plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#adf projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="gray")
text(X,Y,labels=etiq,col="black", cex=0.7)

#adf centroids
dcat<-c(10)

c<-1
seguentColor<-colors[c]

fdic1 = tapply(Psi[,eje1],df[,dcat],mean)
fdic2 = tapply(Psi[,eje2],df[,dcat],mean) 
text(fdic1,fdic2,labels=levels(df[,dcat]),col=seguentColor, cex=0.6)
c<-c+1
legend("bottomleft",names(df)[dcat],pch=1,col=colors, cex=0.5)





##########################
#MIS_STATUS
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1.5,1), ylim=c(-1,1.5))
#plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#adf projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="gray")
text(X,Y,labels=etiq,col="black", cex=0.7)

#adf centroids
dcat<-c(12)

c<-1
seguentColor<-colors[c]

fdic1 = tapply(Psi[,eje1],df[,dcat],mean)
fdic2 = tapply(Psi[,eje2],df[,dcat],mean) 
text(fdic1,fdic2,labels=levels(df[,dcat]),col=seguentColor, cex=0.6)
c<-c+1
legend("bottomleft",names(df)[dcat],pch=1,col=colors, cex=0.5)





##########################
#WHICH COMPANY



plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1.5,1.5), ylim=c(-2,1.5))
#plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#adf projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="gray")
text(X,Y,labels=etiq,col="black", cex=0.7)

#adf centroids
dcat<-c(15)

c<-1
seguentColor<-colors[c]

fdic1 = tapply(Psi[,eje1],df[,dcat],mean)
fdic2 = tapply(Psi[,eje2],df[,dcat],mean) 
text(fdic1,fdic2,labels=levels(df[,dcat]),col=seguentColor, cex=0.6)
c<-c+1
legend("bottomleft",names(df)[dcat],pch=1,col=colors, cex=0.5)




