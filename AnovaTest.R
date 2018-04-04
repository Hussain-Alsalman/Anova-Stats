
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
#Reading in the data 
P18.dat <-read.csv(file = "Ch9_P18.csv", header = TRUE)

#Changing Data from wide format to long format 
P18.dat.w <-stack(P18.dat, select =c(Prototype.A,Prototype.B,Prototype.C))
#Fixing the Column names
colnames(P18.dat.w) <- c("miles", "plan.type")

# Conducting the ANOVA Test on the data 
summary(aov(miles~plan.type, data = P18.dat.w))

#The
useStat9.1 <- read.csv("CH9_CASE11.csv", header = TRUE)


#Example 9-1 

exm9.1 <- read.csv("CH9_CASE12.csv", header = TRUE)

#tell where the data come from
datafilename="http://personality-project.org/R/datasets/R.appendix1.data"
data.ex1=read.table(datafilename,header=T) 
