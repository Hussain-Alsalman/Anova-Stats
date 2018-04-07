
# Chapter 9 ANOVA test 

#problem 9-4 (five plant types, 21 ovservations  with alpha = 0.05
#-----#Finding Critical Value from F-Distribution
qf(0.95, 4,100)
#-----#Approximate of P-value
pf(3.4, 4,100) -0.95
# Ch.9 Working Example p.353 - 366
#--- Data 
ShapesData <-as.data.frame(cbind(c(rep("T",4), rep("S", 4), rep("C",3)),  c(4,5,7,8,10:13,1:3)),stringsAsFactors = FALSE)
ShapesData[,2]<- as.numeric(ShapesData[,2])

# Point Xij p.359
x24 <- ShapesData[8,2]
# Mean of group Squares
Xs<- mean(ShapesData[which(ShapesData$V1 =='S'),2])

# Mean of all points 
Xt <- mean(ShapesData$V2)

#------ Calculating Deviation for point X_24
#Total deviation 
Tot <- x24 - Xt

#Treatment Deviation 
t <- Xs - Xt
t
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
 attach(ShapesData)

tb_shapes <- tibble::tribble(
~Triangle, ~Square, ~Circle,
4, 10,1,
5,11,2,
7,12,3,
8,13,NA
 )
#Calculating the total Mean, Sample Size for each group and Mean for each group

x_dbar <- mean(t(tb_shapes), na.rm = TRUE) #total Mean
Ns <-apply(tb_shapes, 2, function(x) sum(!is.na(x))) #Sample Size for each group
means_i <-colMeans(tb_shapes,na.rm = TRUE) #Sample Size for each group

SSE <- sum((t(tb_shapes)-means_i)^2,na.rm = T)
SSTR <- sum(t(Ns*t((means_i- x_dbar))^2))

SST <- SSTR + SSE

# Degrees of freedom :
#Df(treatment) = r -1
#Df (error) = n - r
#Df(total = df(treatment) + df(error)

df.treatment <- dim(tb_shapes)[2] - 1
df.error <- sum(Ns) - dim(tb_shapes)[2]
df.total <- df.treatment + df.error

# Mean of Squares 
MSTR <- SSTR / df.treatment
MSE <- SSE / df.error

#F-Distribution 
#Test statistics for our data 
F.df_tr.df_er <- MSTR/MSE

#Critical value from F-Distribution with Alph = 0.01
Critic.val <-qf(0.99,df.treatment, df.error)

#Reject the null Hypothesis if Test Statistics is GREATER than Critical Value 
F.df_tr.df_er > Critic.val
# TRUE

###Then We reject the Null Hypothesis 

#Short-cut for all steps 
summary(aov(V2~V1 , data=ShapesData))
#-----------------Exercises-----------------------
#------Problem 9-18
#...Reading in the data
P18.dat <-read.csv(file = "Ch9_P18.csv", header = TRUE)

#...Changing Data from wide format to long format
P18.dat.w <-stack(P18.dat, select =c(Prototype.A,Prototype.B,Prototype.C))
#...Fixing the Column names
colnames(P18.dat.w) <- c("miles", "plan.type")

#...Conducting the ANOVA Test on the data
res.P18<- aov(miles~plan.type, data = P18.dat.w)
summary(res.P18)
#...Reject the null Hypothesis if Test Statistics is GREATER than Critical Value
Critic.val.P18 <-qf(.95,2,27)
#...Extracting F-test value
F.test.P18<-summary(res.P18)[[1]]["F value"][[1]][1]
F.test.P18 > Critic.val.P18
#...TRUE
#...Reject Null Hypothesis

#------Problem 9-19
#...Reading in the data
P19.dat <-read.csv(file = "Ch9_P19.csv", header = TRUE)
#...Changing Data from wide format to long format
P19.dat.w <-stack(P19.dat, select =c(U.K., Mexico, U.A.E., Oman))
#...Fixing the Column names
colnames(P19.dat.w) <- c("prices", "countries")
#...Conducting the ANOVA Test on the data
res.P19 <-aov(prices~countries, data = P19.dat.w)
summary(res.P19)

#...Reject the null Hypothesis if Test Statistics is GREATER than Critical Value
Critic.val.P19 <-qf(.95,3,28)
#...Extracting F-test value
F.test.P19<-summary(res.P19)[[1]]["F value"][[1]][1]
F.test.P19 > Critic.val.P19
#...TRUE
#...Reject Null Hypothesis

#------Problem 9-21
#...Reading in the data
P21.dat <-read.csv(file = "Ch9_P21.csv", header = TRUE)
#...Changing Data from wide format to long format
P21.dat.w <-stack(P21.dat, select =c(High, Medium, Low))
#...Fixing the Column names
colnames(P21.dat.w) <- c("months", "performance")
#...Conducting the ANOVA Test on the data
res.P21 <-aov(months~performance, data = P21.dat.w)
summary(res.P21)
lattice::intervals(res.P21)
#...Reject the null Hypothesis if Test Statistics is GREATER than Critical Value
Critic.val.P21 <-qf(.95,2,38)
#...Extracting F-test value
F.test.P21<-summary(res.P21)[[1]]["F value"][[1]][1]
F.test.P21 > Critic.val.P21
#...TRUE
#...Reject Null Hypothesis
