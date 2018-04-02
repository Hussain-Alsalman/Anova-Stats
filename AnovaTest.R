
# Chapter 9 ANOVA test 


#problem 9-4 (five plant types, 21 ovservations  with alpha = 0.05
#-----#Finding Critical Value from F-Distribution
qf(0.95, 4,100)
#-----#Approximate of P-value
pf(3.4, 4,100) -0.95
# Ch.9 Working Example p.353 - 366
#--- Data 
ShapesDate <-as.data.frame(cbind(c(rep("T",4), rep("S", 4), rep("C",3)),  c(4,5,7,8,10:13,1:3)),stringsAsFactors = FALSE)
ShapesDate[,2]<- as.numeric(ShapesDate[,2])

# Point Xij p.359
x24 <- ShapesDate[8,2]
# Mean of group Squares
Xs<- mean(ShapesDate[which(ShapesDate$V1 =='S'),2])

# Mean of all points 
Xt <- mean(ShapesDate$V2)

#------ Calculating Deviation for point X_24
#Total deviation 
Tot <- x24 - Xt

#Treatment Deviation 
t <- Xs - Xt

#Error Deviation 
e <- x24 - Xs
# Deviation formula is  Tot = t + e 
# t = Treatement Deviation 
# e = Error Deviatoin (single point)

#Other definition of Total Deviation 
 t + e == Tot  # this is  
 t + e == (Xs - Xt) + (x24 - Xs) # as same as this 
 #TRUE 
 
 # if we sequare terms we don't get same results 
 t^2 + e^2 == (Xs - Xt)^2 + (x24 - Xs)^2 # this is not 
 t^2 + e^2 == Tot^2 # as same as this 

 # The sum of sequares is true 
 attach(ShapesDate)
 sapply(V1~V2, FUN = sum)
 #Using Statistics 9-1 

summary(aov(V2~V1 , data=ShapesDate))

useStat9.1 <- read.csv("CH9_CASE11.csv", header = TRUE)


#Example 9-1 

exm9.1 <- read.csv("CH9_CASE12.csv", header = TRUE)

#tell where the data come from
datafilename="http://personality-project.org/R/datasets/R.appendix1.data"
data.ex1=read.table(datafilename,header=T) 
