# Final Project
# Tony Wang/ Shengjue Yuan
# E-mail: zw003@bucknell.edu / sy010@bucknell.edu
# Class: UNIV 200 - Introduction to Data Science
# Prof: Abby Flynt and Brian King
# Semester: Spring 2016
library(ggplot2)
##install ggmap package
.libPaths()
install.packages(c('ggmap','sp'),lib="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library(ggmap)
citation("ggmap")

#######################
## Data preprocessing##
#######################

Tobacco.FULL<-read.csv("Behavioral_Risk_Factor_Data__Tobacco_Use__2010_And_Prior_.csv",head=TRUE)
#get rid of all NAs
Tobacco<-Tobacco.FULL[complete.cases(Tobacco.FULL),]
# get the levels of Tobacco$TopicDesc
levels(Tobacco$TopicDesc)
#[1] "Cessation (Adults)"             "Cigarette Consumption (Adults)"
#[3] "Cigarette Use (Adults)"  

#1.get rid of Cessation (Adults) and Cigarette Consumption (Adults)
Tobacco.CU<-Tobacco[Tobacco[,5]!="Cessation (Adults)" &  Tobacco[,5]!="Cigarette Consumption (Adults)",]

#2.get rid of useless columns
Tobacco.CU1<-Tobacco.CU[,-c(3,4,5,6,7,9,10,12,13,14,15,16,18,19,20,21,23,24)] 

#3.Explore the data set, we find out each state in each year need to answer 3 questions and
# the sum of the percentage among the responses for each question would be the 100 percent 
Tobacco.CU1[Tobacco.CU1[,1]==2000 & Tobacco.CU1[,9]=="8AGE" & Tobacco.CU1[,8]=="1GEN" & Tobacco.CU1[,10]=="6RAC" & Tobacco.CU1[,11]=="6EDU" &  Tobacco.CU1[,2]=="AL" , ]
# YEAR LocationAbbr  Response Data_Value Sample_Size                             GeoLocation MeasureId StratificationID1 StratificationID2 StratificationID3
# 501 2000           AL                 25.2        2234 (32.84057112200048, -86.63186076199969)    110CSA              1GEN              8AGE              6RAC
# 515 2000           AL Every Day       76.4         549 (32.84057112200048, -86.63186076199969)    166SSP              1GEN              8AGE              6RAC
# 519 2000           AL Some Days       23.6         549 (32.84057112200048, -86.63186076199969)    166SSP              1GEN              8AGE              6RAC
# 521 2000           AL   Current       25.2        2234 (32.84057112200048, -86.63186076199969)    165SSA              1GEN              8AGE              6RAC
# 524 2000           AL    Former       24.0        2234 (32.84057112200048, -86.63186076199969)    165SSA              1GEN              8AGE              6RAC
# 527 2000           AL     Never       50.8        2234 (32.84057112200048, -86.63186076199969)    165SSA              1GEN              8AGE              6RAC
# StratificationID4
# 501              6EDU
# 515              6EDU
# 519              6EDU
# 521              6EDU
# 524              6EDU
# 527              6EDU
#4. now we separate the data into 3 data frames according to the questions they are asked.
#1) current smoking
Tobacco.CU1c<-Tobacco.CU1[Tobacco.CU1[,7]=="110CSA",]
Tobacco.CU1c<-Tobacco.CU1c[ ,-7]
#2) smoking frequency
Tobacco.CU1f<-Tobacco.CU1[Tobacco.CU1[,7]=="166SSP",]
nrow(Tobacco.CU1f[Tobacco.CU1f[,9]!="8AGE" | Tobacco.CU1f[,10]!="6RAC"| Tobacco.CU1f[,11]!="6EDU", ])
#[1] 0
# We found that in smoking frequency data set only gender variable has different values
# whearas the other variables are all set to be "all ages","all education level" and "all races" 
# Therefore, we delete the variables that are unchanged all the time
Tobacco.CU1f<-Tobacco.CU1f[ ,-c(7,9,10,11)]
#3) smoking status
Tobacco.CU1s<-Tobacco.CU1[Tobacco.CU1[,7]=="165SSA",]
# Same as the smoking frequency, the smoking status data set also has changing values for gender but unchanged
# values for other variables. Therefore, we do the same thing to the smoking status data set.
Tobacco.CU1s<-Tobacco.CU1s[ ,-c(7,9,10,11)]




######################
##Data visualization##
######################

#1
# current smoking 
Tran.year<- Tobacco.CU1c[Tobacco.CU1c[,7]=="1GEN" & Tobacco.CU1c[,8]=="8AGE" & Tobacco.CU1c[,9]=="6RAC" & Tobacco.CU1c[,10]=="6EDU", ]
# get rid of useless variables.
Tran.year<- Tran.year[,-c(3,7,8,9,10)]
# We figure out that the sample size for different state in different time are different. Therefore, we could not simply get the exact
# number of people by multiplying the percentage by the sample size to get exact number of people for each observation and compare these numbers.
# However, the percentage is relatively more convincable since the sample size for each state in each year is large and once you change
# the sample size to a larger one, the percentage would not change much. We could visualize the change of percentage based on two variables: State and Year
# 1Q+1C
#1) percentage of people current smoking changes over state
ggplot(Tran.year) + geom_point(aes(x = LocationAbbr,y=Data_Value,color=YEAR))+xlab("Year")+ylab("percentage")+ggtitle("percentage of people smoking changes over states")
#2) percentage of people current smoking changes over year with boxplot
ggplot(Tran.year, aes( x= as.factor(YEAR), y = Data_Value))+ geom_boxplot() + guides(fill = FALSE)+xlab("Year")+ylab("percentage")+ggtitle("percentage of people smoking in different states changes over year")

# Now we need to figure out how different variables are measured: how to control other variables
# while measuring one variable.
# First, let's see how age level is measured.
#3) changes over ages
# explore levels of ages
levels(Tobacco.CU[,"Age"])
# [1] "18 to 24 Years"     "18 to 44 Years"     "25 to 44 Years"     "45 to 64 Years"     "65 Years and Older" "Age 20 and Older"   "Age 25 and Older"  
# [8] "All Ages"
# Therefore, we decide to keep the ones with age level[1] "18 to 24 Years" or "25 to 44 Years" or  "45 to 64 Years" or "65 Years and Older" which means that we only need to keep 
# "1AGE", "2AGE" , "3AGE" or "4AGE" 
# We figure out how they control other variables while measuring ages so we explore the observations
# they collected in one year one area:
Tobacco.CU1c[Tobacco.CU1c[,1]==2000 & Tobacco.CU1c[,2]=="AL" & c(Tobacco.CU1c[,8]=="1AGE" | Tobacco.CU1c[,8]=="2AGE" | Tobacco.CU1c[,8]=="3AGE" | Tobacco.CU1c[,8]=="4AGE") , ]
# YEAR LocationAbbr Response Data_Value Sample_Size                             GeoLocation
# 493 2000           AL                11.1         431 (32.84057112200048, -86.63186076199969)
# 498 2000           AL                24.0         750 (32.84057112200048, -86.63186076199969)
# 505 2000           AL                30.5         839 (32.84057112200048, -86.63186076199969)
# 506 2000           AL                32.3         200 (32.84057112200048, -86.63186076199969)
# StratificationID1 StratificationID2 StratificationID3 StratificationID4
# 493              1GEN              4AGE              6RAC              6EDU
# 498              1GEN              3AGE              6RAC              6EDU
# 505              1GEN              2AGE              6RAC              6EDU
# 506              1GEN              1AGE              6RAC              6EDU
# We could notice that only the value of Age changes while the values for Gender, Race and Education does not change.
# We can check this out by the following code:
Tran1.year<- Tobacco.CU1c[Tobacco.CU1c[,8]=="1AGE" | Tobacco.CU1c[,8]=="2AGE" | Tobacco.CU1c[,8]=="3AGE" | Tobacco.CU1c[,8]=="4AGE" , ]
nrow(Tran1.year[Tran1.year[,7]!="1GEN"|Tran1.year[,9]!="6RAC"|Tran1.year[,10]!="6EDU",])
#[1] 0
# This means that when age is measured, the other variables stay the same.
levels(Tran1.year[,8])<-c("18~24","25~44","44~64",">65","0","0","0","0")
# get rid of useless variables
Tran1.year<-Tran1.year[,-c(3,6,7,9,10)]
Tran1.year[,3]<-Tran1.year[,3]/100
ggplot(Tran1.year, aes( x= as.factor(YEAR), y = Data_Value,color=Tran1.year[,5])) + geom_violin(scale = "width", alpha = .5  , draw_quantiles = c(0.25, 0.5, 0.75))+labs(color = "Age Level")+xlab("Year")+ylab("percentage") +ggtitle("percentage of people smoking over years with different ages") 

#4) changes over education
# explore levels of education
levels(Tobacco.CU[,"Education"])
# [1] "< 12th Grade" "> 12th Grade" "12th Grade"   "All Grades"
# Therefore, we decide to keep the ones with education levels "< 12th Grade" or "> 12th Grade"or  "12th Grade", which means we will keep the ones with "3EDU" or "4EDU" or "5EDU"
# We also figure out how they control other variables while measuring education so we explore the observations
# they collected in one year one area:
Tobacco.CU1c[Tobacco.CU1c[,1]==2000 & Tobacco.CU1c[,2]=="AL" & c(Tobacco.CU1c[,10]=="3EDU" | Tobacco.CU1c[,10]=="4EDU" | Tobacco.CU1c[,10]=="5EDU"), ]
#    YEAR LocationAbbr Response Data_Value Sample_Size                             GeoLocation
# 495 2000           AL                20.2         974 (32.84057112200048, -86.63186076199969)
# 497 2000           AL                22.0        1060 (32.84057112200048, -86.63186076199969)
# 499 2000           AL                24.5         731 (32.84057112200048, -86.63186076199969)
# 500 2000           AL                24.8         680 (32.84057112200048, -86.63186076199969)
# 507 2000           AL                33.4         363 (32.84057112200048, -86.63186076199969)
# 508 2000           AL                33.9         378 (32.84057112200048, -86.63186076199969)
# StratificationID1 StratificationID2 StratificationID3 StratificationID4
# 495              1GEN              7AGE              6RAC              5EDU
# 497              1GEN              6AGE              6RAC              5EDU
# 499              1GEN              6AGE              6RAC              4EDU
# 500              1GEN              7AGE              6RAC              4EDU
# 507              1GEN              7AGE              6RAC              3EDU
# 508              1GEN              6AGE              6RAC              3EDU
# We figure out that only the value of education changes while the values for Gender, Race and gender does not change.
# 6AGE is "age 20 or older" and 7AGE is age 25 or older"
# We can check this out by the following code:
Tran2.year<- Tobacco.CU1c[Tobacco.CU1c[,10]=="3EDU" | Tobacco.CU1c[,10]=="4EDU" | Tobacco.CU1c[,10]=="5EDU", ]
nrow(Tran2.year[Tran2.year[,7]!="1GEN" | Tran2.year[,9]!="6RAC", ])
nrow(Tran2.year[Tran2.year[,8]!="7AGE" & Tran2.year[,8]!="6AGE",])
#[1] 0
#[1] 0
# This means that when education level is measured, the other variables stay the same.
levels(Tran2.year[,10])<-c("< 12th Grade", "> 12th Grade", "12th Grade","0")
levels(Tran2.year[,8])<-c("18 to 24 Years","18 to 44 Years","25 to 44 Years","45 to 64 Years","65 Years and Older","Age 20 and Older","Age 25 and Older","All Ages")
# get rid of useless variables
Tran2.year<-Tran2.year[,-c(3,7,9)]
Tran2.year[,3]<-Tran2.year[,3]/100
ggplot(Tran2.year, aes( x= as.factor(YEAR), y = Data_Value,color=Tran2.year[,7])) + geom_violin(scale = "width", alpha = .5  , draw_quantiles = c(0.25, 0.5, 0.75))+labs(color = "Education Level")+xlab("Year")+ylab("percentage") +ggtitle("percentage of people smoking over years with different education background") 
ggplot(Tran2.year, aes( x= as.factor(StratificationID4), y = Data_Value,color=StratificationID2)) + geom_violin(scale = "width", alpha = .5  , draw_quantiles = c(0.25, 0.5, 0.75))+labs(color = "Age Level")+xlab("Year")+ylab("percentage") +ggtitle("percentage of people smoking over education background with different ages") 


#5) changes over race
# explore levels of education
levels(Tobacco.CU[,"Race"])
# [1] "African American"              "All Races"                     "American Indian/Alaska Native"
# [4] "Asian/Pacific Islander"        "Hispanic"                      "White"
# Therefore, we decide to keep the ones with race levels
# "African American"  "American Indian/Alaska Native" "Asian/Pacific Islander" "Hispanic" "White" which means we will keep the ones with 
Tobacco.CU1c[Tobacco.CU1c[,1]==2010 & Tobacco.CU1c[,2]=="AL" & Tobacco.CU1c[,9]!="6RAC", ]
# YEAR LocationAbbr Response Data_Value Sample_Size                             GeoLocation StratificationID1
# 494 2000           AL                19.3         498 (32.84057112200048, -86.63186076199969)              1GEN
# 502 2000           AL                25.5        1634 (32.84057112200048, -86.63186076199969)              1GEN
# StratificationID2 StratificationID3 StratificationID4
# 494              8AGE              1RAC              6EDU
# 502              8AGE              5RAC              6EDU
# We also figure out how they control other variables while measuring education so we explore the observations
# they collected in one year one area:
Tran3.year<- Tobacco.CU1c[Tobacco.CU1c[,7]=="1GEN" & Tobacco.CU1c[,8]=="8AGE" & Tobacco.CU1c[,10]=="6EDU" & Tobacco.CU1c[,9]!= "6RAC", ]
#check other variables:
nrow(Tran3.year[Tran3.year[,7]!="1GEN"|Tran3.year[,8]!="8AGE"|Tran3.year[,10]!="6EDU",])
#[1] 0
levels(Tran3.year[,9])
levels(Tran3.year[,9])<-c("African American","American Indian/Alaska Native","Asian/Pacific Islander","Hispanic","White","All Races")
Tran3.year[,3]<-Tran3.year[,3]/100
ggplot(Tran3.year, aes( x= as.factor(YEAR), y = Data_Value,color=Tran3.year[,9],fill = Tran3.year[,9])) + geom_boxplot()+labs(color = "Race Level",fill = "Race Level")+xlab("Year")+ylab("percentage") +ggtitle("percentage of people smoking over years with different races")

#6) changes over gender
# explore levels of gender
levels(Tobacco.CU[,"Gender"])
# [1] "Female"  "Male"    "Overall"
# Therefore, we decide to keep the ones with education levels "Female"  "Male" , which means we will keep the ones with "2GEN" or "3GEN"
# We also figure out how they control other variables while measuring education so we explore the observations
# they collected in one year one area:
Tobacco.CU1c[Tobacco.CU1c[,1]==2000 & Tobacco.CU1c[,2]=="AL" & c(Tobacco.CU1c[,7]=="2GEN" | Tobacco.CU1c[,7]=="3GEN"), ]
# YEAR LocationAbbr Response Data_Value Sample_Size                             GeoLocation StratificationID1 StratificationID2 StratificationID3 StratificationID4
# 496 2000           AL                22.0        1398 (32.84057112200048, -86.63186076199969)              3GEN              8AGE              6RAC              6EDU
# 504 2000           AL                28.9         836 (32.84057112200048, -86.63186076199969)              2GEN              8AGE              6RAC              6EDU
Tran4.year <- Tobacco.CU1c[Tobacco.CU1c[,7]=="2GEN" | Tobacco.CU1c[,7]=="3GEN", ]
levels(Tran4.year[,7])<-c("overall", "male", "female")
ggplot(Tran4.year, aes( x= as.factor(YEAR), y = Data_Value,color=Tran4.year[,7])) + geom_violin(scale = "width", alpha = .5  , draw_quantiles = c(0.25, 0.5, 0.75))+labs(color = "Gender")+xlab("Year")+ylab("percentage") +ggtitle("percentage of people smoking over years with different gender") 

#Frequency
# People smoking everyday change over year
Tobacco.CU1f.year = Tobacco.CU1f[Tobacco.CU1f[,7] == "1GEN",]
ggplot(Tobacco.CU1f.year,aes(x = YEAR,y = Data_Value))+geom_point()+geom_smooth(method = "loess")+ggtitle("Scatterplot of percentage of people smoking every day")

Tobbaco.CU1f.gender = Tobacco.CU1f[Tobacco.CU1f[,7] != "1GEN",]
Tobbaco.CU1f.gender.eve = Tobbaco.CU1f.gender[Tobbaco.CU1f.gender$Response == "Every Day",]
levels(Tobbaco.CU1f.gender.eve$StratificationID1)<-c("overall", "male", "female")
ggplot(Tobbaco.CU1f.gender.eve, aes(x= as.factor(YEAR), y = Data_Value, color=Tobbaco.CU1f.gender.eve[,7])) + geom_violin(scale = "width", alpha = .5  , draw_quantiles = c(0.25, 0.5, 0.75))+labs(color = "Gender")+xlab("Year")+ylab("percentage") +ggtitle("percentage of people smoking every day over years with different gender")

#Smoking status
Tobacco.CU1s.year = Tobacco.CU1s[Tobacco.CU1s[,7]=="1GEN"& Tobacco.CU1s[,3]!="Current",]
# distribution of people who have former smoking and who never smoke change over year.
ggplot(Tobacco.CU1s.year,aes(x = YEAR,y = Data_Value, color = Response))+geom_point()+geom_smooth(method = "loess")+ggtitle("Scatterplot of percentage of former and never smoker")+ylab("percentage") 


Tobacco.CU1s.year = Tobacco.CU1s[Tobacco.CU1s[,7]!="1GEN"& Tobacco.CU1s[,3]=="Former",]
# distribution of people who have former smoking and who never smoke change over year.
levels(Tobacco.CU1s.year[,7])<-c("overall","male","female")
ggplot(Tobacco.CU1s.year,aes(x = YEAR,y = Data_Value, color = StratificationID1))+geom_point()+geom_smooth(method = "loess")+ggtitle("Scatterplot of percentage of former smoker over gender")+ylab("percentage")+scale_color_discrete(name="gender")

Tobacco.CU1s.year = Tobacco.CU1s[Tobacco.CU1s[,7]!="1GEN"& Tobacco.CU1s[,3]=="Never",]
# distribution of people who have former smoking and who never smoke change over year.
levels(Tobacco.CU1s.year[,7])<-c("overall","male","female")
ggplot(Tobacco.CU1s.year,aes(x = YEAR,y = Data_Value, color = StratificationID1))+geom_point()+geom_smooth(method = "loess")+ggtitle("Scatterplot of percentage of people never smoke change over year among male and female")+ylab("percentage")+scale_color_discrete(name="gender")


###############
#####ggmap#####
###############


## current smoking 
##create a new data frame that only contains current smoking data and 
gg.current.s<-Tobacco.CU[,-c(2,4,5,6,7,9,10,12,13,14,15,16,18,19,20,21,23,24)] 
gg.current.s.all<-gg.current.s[gg.current.s[,7]=="110CSA",]
gg.current.s.all<-gg.current.s.all[,-7]
gg.current.s.all<-gg.current.s.all[gg.current.s.all[,7]=="1GEN" & gg.current.s.all[,8]=="8AGE" & gg.current.s.all[,9]=="6RAC" & gg.current.s.all[,10]=="6EDU", ]

#create a vector of state to get the geo location of each state
state.vecotr<-as.character(levels(gg.current.s.all[,2]))
state<-geocode(state.vecotr)

#match the geo location of each state to the data frame
gg.current.s.all$long<-rep(0,nrow(gg.current.s.all))
gg.current.s.all$lat<-rep(0,nrow(gg.current.s.all))

for(i in 1:length(state.vecotr)){
  l<-nrow(gg.current.s.all[gg.current.s.all[,2]==state.vecotr[i],])
  gg.current.s.all[gg.current.s.all[,2]==state.vecotr[i],]$long<-rep(state[i,1],l)
  gg.current.s.all[gg.current.s.all[,2]==state.vecotr[i],]$lat<-rep(state[i,2],l)
}

#set the center of the map and create a map
co <- geocode("CO")
map <- get_map(co,zoom = 3,scale=2,color=("bw"))

#create a plot showing the smoking percentage changing over longtitude and latitude 
gg.current.s.all<-gg.current.s.all[,-3]
colnames(gg.current.s.all)[3]<-"percentage"
qplot(long, lat, data = gg.current.s.all, colour =percentage)+scale_color_gradient2(low="blue",mid="green",high="red",midpoint = 20)
## this is not clear enough so we decide to put that on a map
## in order to show the change of percentage through time, we set the longtitude of the every year to be 0.2 more than the previous year, start from the second year which is 1997
tt<-as.numeric(levels(factor(gg.current.s.all[,1])))
gg.current.s.all$long1<-gg.current.s.all$long

gg.current.s.all[gg.current.s.all[,1]==tt[2],]$long1<-gg.current.s.all[gg.current.s.all[,1]==tt[2],]$long
gg.current.s.all[gg.current.s.all[,1]==tt[2],]$long1<-gg.current.s.all[gg.current.s.all[,1]==tt[2],]$long+0.2
gg.current.s.all[gg.current.s.all[,1]==tt[3],]$long1<-gg.current.s.all[gg.current.s.all[,1]==tt[3],]$long+0.4
gg.current.s.all[gg.current.s.all[,1]==tt[4],]$long1<-gg.current.s.all[gg.current.s.all[,1]==tt[4],]$long+0.6
gg.current.s.all[gg.current.s.all[,1]==tt[5],]$long1<-gg.current.s.all[gg.current.s.all[,1]==tt[5],]$long+0.8
gg.current.s.all[gg.current.s.all[,1]==tt[6],]$long1<-gg.current.s.all[gg.current.s.all[,1]==tt[6],]$long+1
gg.current.s.all[gg.current.s.all[,1]==tt[7],]$long1<-gg.current.s.all[gg.current.s.all[,1]==tt[7],]$long+1.2
gg.current.s.all[gg.current.s.all[,1]==tt[8],]$long1<-gg.current.s.all[gg.current.s.all[,1]==tt[8],]$long+1.4
gg.current.s.all[gg.current.s.all[,1]==tt[9],]$long1<-gg.current.s.all[gg.current.s.all[,1]==tt[9],]$long+1.6
gg.current.s.all[gg.current.s.all[,1]==tt[10],]$long1<-gg.current.s.all[gg.current.s.all[,1]==tt[10],]$long+1.8
gg.current.s.all[gg.current.s.all[,1]==tt[11],]$long1<-gg.current.s.all[gg.current.s.all[,1]==tt[11],]$long+2
gg.current.s.all[gg.current.s.all[,1]==tt[12],]$long1<-gg.current.s.all[gg.current.s.all[,1]==tt[12],]$long+2.2
gg.current.s.all[gg.current.s.all[,1]==tt[13],]$long1<-gg.current.s.all[gg.current.s.all[,1]==tt[13],]$long+2.4
gg.current.s.all[gg.current.s.all[,1]==tt[14],]$long1<-gg.current.s.all[gg.current.s.all[,1]==tt[14],]$long+2.6
gg.current.s.all[gg.current.s.all[,1]==tt[15],]$long1<-gg.current.s.all[gg.current.s.all[,1]==tt[15],]$long+2.8
ggmap(map) +
  geom_point(aes(x = long1, y = lat, colour = percentage), data = gg.current.s.all, alpha = .7, size=0.5)+ggtitle("percentage of people currently smoking")+scale_color_gradient2(low="blue",mid="green",high="red",midpoint=20,name="percentage for \n all gender \n smoking every \n day")

# This map shows percentage of people smoking in different states and in different years. For 
# Every bar we see in each state, the left side represents the smoking percentage of the first
# year and the right shows the smoking percentage of the last year. 

# Well, we later figure out that there is another way to show people current smoking percentage
# We tried using loop but it seems that if we use the loop, then we could not create the maps
# 1996
states <- map_data("state")
arrests1 <- gg.current.s.all[gg.current.s.all[,1]==1996,]
names(arrests1) <- tolower(names(arrests1))
colnames(arrests1)[2]<-"region"
arrests1$region<-tolower(arrests1$region)
arrests1<-arrests1[-c(4:12)]
  
choro <- merge(states, arrests1, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
colnames(choro)[8]<-"percentage"
ggplot(choro,aes(x=long, y=lat)) +
    geom_polygon(aes(group = group, fill =percentage),color="black",alpha=0.4) +
    coord_map("albers",  at0 = 45.5, lat1 = 29.5)+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20)+ggtitle("percentage of people smoking in 1996")

# 1997

arrests1 <- gg.current.s.all[gg.current.s.all[,1]==1997,]
names(arrests1) <- tolower(names(arrests1))
colnames(arrests1)[2]<-"region"
arrests1$region<-tolower(arrests1$region)
arrests1<-arrests1[-c(4:12)]
  
choro <- merge(states, arrests1, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
colnames(choro)[8]<-"percentage"
ggplot(choro,aes(x=long, y=lat)) +
    geom_polygon(aes(group = group, fill =percentage),color="black",alpha=0.4) +
    coord_map("albers",  at0 = 45.5, lat1 = 29.5)+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20)+ggtitle("percentage of people smoking in 1997")

# 1998

arrests1 <- gg.current.s.all[gg.current.s.all[,1]==1998,]
names(arrests1) <- tolower(names(arrests1))
colnames(arrests1)[2]<-"region"
arrests1$region<-tolower(arrests1$region)
arrests1<-arrests1[-c(4:12)]
  
choro <- merge(states, arrests1, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
colnames(choro)[8]<-"percentage"
ggplot(choro,aes(x=long, y=lat)) +
    geom_polygon(aes(group = group, fill =percentage),color="black",alpha=0.4) +
    coord_map("albers",  at0 = 45.5, lat1 = 29.5)+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20)+ggtitle("percentage of people smoking in 1998")

# 1999

arrests1 <- gg.current.s.all[gg.current.s.all[,1]==1999,]
names(arrests1) <- tolower(names(arrests1))
colnames(arrests1)[2]<-"region"
arrests1$region<-tolower(arrests1$region)
arrests1<-arrests1[-c(4:12)]
  
choro <- merge(states, arrests1, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
colnames(choro)[8]<-"percentage"
ggplot(choro,aes(x=long, y=lat)) +
    geom_polygon(aes(group = group, fill =percentage),color="black",alpha=0.4) +
    coord_map("albers",  at0 = 45.5, lat1 = 29.5)+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20)+ggtitle("percentage of people smoking in 1999")

# 2000

arrests1 <- gg.current.s.all[gg.current.s.all[,1]==2000,]
names(arrests1) <- tolower(names(arrests1))
colnames(arrests1)[2]<-"region"
arrests1$region<-tolower(arrests1$region)
arrests1<-arrests1[-c(4:12)]
  
choro <- merge(states, arrests1, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
colnames(choro)[8]<-"percentage"
ggplot(choro,aes(x=long, y=lat)) +
    geom_polygon(aes(group = group, fill =percentage),color="black",alpha=0.4) +
    coord_map("albers",  at0 = 45.5, lat1 = 29.5)+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20)+ggtitle("percentage of people smoking in 2000")

# 2001

arrests1 <- gg.current.s.all[gg.current.s.all[,1]==2001,]
names(arrests1) <- tolower(names(arrests1))
colnames(arrests1)[2]<-"region"
arrests1$region<-tolower(arrests1$region)
arrests1<-arrests1[-c(4:12)]
  
choro <- merge(states, arrests1, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
colnames(choro)[8]<-"percentage"
ggplot(choro,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill =percentage),color="black",alpha=0.4) +
    coord_map("albers",  at0 = 45.5, lat1 = 29.5)+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20)+ggtitle("percentage of people smoking in 2001")

# 2002

arrests1 <- gg.current.s.all[gg.current.s.all[,1]==2002,]
names(arrests1) <- tolower(names(arrests1))
colnames(arrests1)[2]<-"region"
arrests1$region<-tolower(arrests1$region)
arrests1<-arrests1[-c(4:12)]
  
choro <- merge(states, arrests1, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
colnames(choro)[8]<-"percentage"
ggplot(choro,aes(x=long, y=lat)) +
    geom_polygon(aes(group = group, fill =percentage),color="black",alpha=0.4) +
    coord_map("albers",  at0 = 45.5, lat1 = 29.5)+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20)+ggtitle("percentage of people smoking in 2002")

# 2003

arrests1 <- gg.current.s.all[gg.current.s.all[,1]==2003,]
names(arrests1) <- tolower(names(arrests1))
colnames(arrests1)[2]<-"region"
arrests1$region<-tolower(arrests1$region)
arrests1<-arrests1[-c(4:12)]
  
choro <- merge(states, arrests1, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
colnames(choro)[8]<-"percentage"
ggplot(choro,aes(x=long, y=lat)) +
    geom_polygon(aes(group = group, fill =percentage),color="black",alpha=0.4) +
    coord_map("albers",  at0 = 45.5, lat1 = 29.5)+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20)+ggtitle("percentage of people smoking in 2003")


# 2004

arrests1 <- gg.current.s.all[gg.current.s.all[,1]==2004,]
names(arrests1) <- tolower(names(arrests1))
colnames(arrests1)[2]<-"region"
arrests1$region<-tolower(arrests1$region)
arrests1<-arrests1[-c(4:12)]
  
choro <- merge(states, arrests1, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
colnames(choro)[8]<-"percentage"
ggplot(choro,aes(x=long, y=lat)) +
    geom_polygon(aes(group = group, fill =percentage),color="black",alpha=0.4) +
    coord_map("albers",  at0 = 45.5, lat1 = 29.5)+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20)+ggtitle("percentage of people smoking in 2004")

# 2005

arrests1 <- gg.current.s.all[gg.current.s.all[,1]==2005,]
names(arrests1) <- tolower(names(arrests1))
colnames(arrests1)[2]<-"region"
arrests1$region<-tolower(arrests1$region)
arrests1<-arrests1[-c(4:12)]
  
choro <- merge(states, arrests1, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
colnames(choro)[8]<-"percentage"
ggplot(choro,aes(x=long, y=lat)) +
    geom_polygon(aes(group = group, fill =percentage),color="black",alpha=0.4) +
    coord_map("albers",  at0 = 45.5, lat1 = 29.5)+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20)+ggtitle("percentage of people smoking in 2005")

# 2006
arrests1 <- gg.current.s.all[gg.current.s.all[,1]==2006,]
names(arrests1) <- tolower(names(arrests1))
colnames(arrests1)[2]<-"region"
arrests1$region<-tolower(arrests1$region)
arrests1<-arrests1[-c(4:12)]
  
choro <- merge(states, arrests1, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
colnames(choro)[8]<-"percentage"
ggplot(choro,aes(x=long, y=lat)) +
    geom_polygon(aes(group = group, fill =percentage),color="black",alpha=0.4) +
    coord_map("albers",  at0 = 45.5, lat1 = 29.5)+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20)+ggtitle("percentage of people smoking in 2006")

# 2007

arrests1 <- gg.current.s.all[gg.current.s.all[,1]==2007,]
names(arrests1) <- tolower(names(arrests1))
colnames(arrests1)[2]<-"region"
arrests1$region<-tolower(arrests1$region)
arrests1<-arrests1[-c(4:12)]
  
choro <- merge(states, arrests1, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
colnames(choro)[8]<-"percentage"
ggplot(choro,aes(x=long, y=lat)) +
    geom_polygon(aes(group = group, fill =percentage),color="black",alpha=0.4) +
    coord_map("albers",  at0 = 45.5, lat1 = 29.5)+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20)+ggtitle("percentage of people smoking in 2007")

# 2008
arrests1 <- gg.current.s.all[gg.current.s.all[,1]==2008,]
names(arrests1) <- tolower(names(arrests1))
colnames(arrests1)[2]<-"region"
arrests1$region<-tolower(arrests1$region)
arrests1<-arrests1[-c(4:12)]
  
choro <- merge(states, arrests1, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
colnames(choro)[8]<-"percentage"
ggplot(choro,aes(x=long, y=lat)) +
    geom_polygon(aes(group = group, fill =percentage),color="black",alpha=0.4) +
    coord_map("albers",  at0 = 45.5, lat1 = 29.5)+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20)+ggtitle("percentage of people smoking in 2008")

# 2009
arrests1 <- gg.current.s.all[gg.current.s.all[,1]==2009,]
names(arrests1) <- tolower(names(arrests1))
colnames(arrests1)[2]<-"region"
arrests1$region<-tolower(arrests1$region)
arrests1<-arrests1[-c(4:12)]
  
choro <- merge(states, arrests1, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
colnames(choro)[8]<-"percentage"
ggplot(choro,aes(x=long, y=lat)) +
    geom_polygon(aes(group = group, fill =percentage),color="black",alpha=0.4) +
    coord_map("albers",  at0 = 45.5, lat1 = 29.5)+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20)+ggtitle("percentage of people smoking in 2009")

# 2010
arrests1 <- gg.current.s.all[gg.current.s.all[,1]==2010,]
names(arrests1) <- tolower(names(arrests1))
colnames(arrests1)[2]<-"region"
arrests1$region<-tolower(arrests1$region)
arrests1<-arrests1[-c(4:12)]
  
choro <- merge(states, arrests1, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
colnames(choro)[8]<-"percentage"
ggplot(choro,aes(x=long, y=lat)) +
    geom_polygon(aes(group = group, fill =percentage),color="black",alpha=0.4) +
    coord_map("albers",  at0 = 45.5, lat1 = 29.5)+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20)+ggtitle("percentage of people smoking in 2010")

# We are gonna make an GIF animation to show the change of percentage of people smoking in different states and in different years.


###############
##new dataset##
###############


# our goal here is to create a new data set that for each row we have one state in one year and all the values in centain area in certain year. 
# The code below aims to create this new data frame. However, since it really takes a long time to 
# make this data frame, we output this data frame in csv file and save it in our directory. 


# We can simply read this new.d using read.csv. We save the code for making this csv below 
new.d<-read.csv("new.d.csv",head=T)

# code for generating this file, do not run it, it takes a long time.
########################################################################################################
########################################################################################################
# new.d<-data.frame(rep(1996:2010,55))
# colnames(new.d)<-"Year"
# new.place<-rep(as.character(levels(gg.current.s.all[,2])),each=15)
# new.d<-cbind(new.d,new.place)
# 
# # add current smoking to new data
# new.d$cu<- rep(0,nrow(new.d))
# for(i in 1:nrow(gg.current.s.all)){
#   for(j in 1:nrow(new.d)){
#     if(gg.current.s.all[i,1]==new.d[j,1] & gg.current.s.all[i,2]==new.d[j,2]){
#       new.d[j,3] = gg.current.s.all[i,3]
#       break
#   }
#   }
# }
# #add smoking frequency to new data
gg.frequecy.s.all<-gg.current.s[gg.current.s[,7]=="166SSP",]
# new.d$FEM<- rep(0,nrow(new.d))
# new.d$FEF<-rep(0,nrow(new.d))
# new.d$FSM<-rep(0,nrow(new.d))
# new.d$FSF<-rep(0,nrow(new.d))
gg.frequecy.e.male<-gg.frequecy.s.all[gg.frequecy.s.all[,3]=="Every Day" & gg.frequecy.s.all[,8]=="2GEN" ,]
# for(i in 1:nrow(gg.frequecy.e.male)){
#   for(j in 1:nrow(new.d)){
#     if(gg.frequecy.e.male[i,1]==new.d[j,1] & gg.frequecy.e.male[i,2]==new.d[j,2]){
#       new.d[j,4] = gg.frequecy.e.male[i,4]
#       break
#     }
#   }
# }
gg.frequecy.e.female<-gg.frequecy.s.all[gg.frequecy.s.all[,3]=="Every Day" & gg.frequecy.s.all[,8]=="3GEN" ,]
# for(i in 1:nrow(gg.frequecy.e.female)){
#   for(j in 1:nrow(new.d)){
#     if(gg.frequecy.e.female[i,1]==new.d[j,1] & gg.frequecy.e.female[i,2]==new.d[j,2]){
#       new.d[j,5] = gg.frequecy.e.female[i,4]
#       break
#     }
#   }
# }
gg.frequecy.s.male<-gg.frequecy.s.all[gg.frequecy.s.all[,3]=="Some Days" & gg.frequecy.s.all[,8]=="2GEN" ,]
# for(i in 1:nrow(gg.frequecy.s.male)){
#   for(j in 1:nrow(new.d)){
#     if(gg.frequecy.s.male[i,1]==new.d[j,1] & gg.frequecy.s.male[i,2]==new.d[j,2]){
#       new.d[j,6] = gg.frequecy.s.male[i,4]
#       break
#     }
#   }
# }
gg.frequecy.s.female<-gg.frequecy.s.all[gg.frequecy.s.all[,3]=="Some Days" & gg.frequecy.s.all[,8]=="3GEN" ,]
# for(i in 1:nrow(gg.frequecy.s.female)){
#   for(j in 1:nrow(new.d)){
#     if(gg.frequecy.s.female[i,1]==new.d[j,1] & gg.frequecy.s.female[i,2]==new.d[j,2]){
#       new.d[j,7] = gg.frequecy.s.female[i,4]
#       break
#     }
#   }
# }
# # add smoking status to new data
# new.d$SCM<-rep(0,nrow(new.d))
# new.d$SCF<-rep(0,nrow(new.d))
# new.d$SFM<-rep(0,nrow(new.d))
# new.d$SFF<-rep(0,nrow(new.d))
# new.d$SNM<-rep(0,nrow(new.d))
# new.d$SNF<-rep(0,nrow(new.d))
# new.d$SCA<-rep(0,nrow(new.d))
# new.d$SFA<-rep(0,nrow(new.d))
# new.d$SNA<-rep(0,nrow(new.d))
# 
# 
gg.status<-gg.current.s[gg.current.s[,7]=="165SSA",]
gg.status.c.male<-gg.status[gg.status[,3]=="Current" & gg.status[,8]=="2GEN", ]
# for(i in 1:nrow(gg.status.c.male)){
#   for(j in 1:nrow(new.d)){
#     if(gg.status.c.male[i,1]==new.d[j,1] & gg.status.c.male[i,2]==new.d[j,2]){
#       new.d[j,8] = gg.status.c.male[i,4]
#       break
#     }
#   }
# }
gg.status.c.female<-gg.status[gg.status[,3]=="Current" & gg.status[,8]=="3GEN", ]
# for(i in 1:nrow(gg.status.c.female)){
#   for(j in 1:nrow(new.d)){
#     if(gg.status.c.female[i,1]==new.d[j,1] & gg.status.c.female[i,2]==new.d[j,2]){
#       new.d[j,9] = gg.status.c.female[i,4]
#       break
#     }
#   }
# }
gg.status.f.male<-gg.status[gg.status[,3]=="Former" & gg.status[,8]=="2GEN", ]
# for(i in 1:nrow(gg.status.f.male)){
#   for(j in 1:nrow(new.d)){
#     if(gg.status.f.male[i,1]==new.d[j,1] & gg.status.f.male[i,2]==new.d[j,2]){
#       new.d[j,10] = gg.status.f.male[i,4]
#       break
#     }
#   }
# }
gg.status.f.female<-gg.status[gg.status[,3]=="Former" & gg.status[,8]=="3GEN", ]
# for(i in 1:nrow(gg.status.f.female)){
#   for(j in 1:nrow(new.d)){
#     if(gg.status.f.female[i,1]==new.d[j,1] & gg.status.f.female[i,2]==new.d[j,2]){
#       new.d[j,11] = gg.status.f.female[i,4]
#       break
#     }
#   }
# }
gg.status.n.male<-gg.status[gg.status[,3]=="Never" & gg.status[,8]=="2GEN", ]
# for(i in 1:nrow(gg.status.n.male)){
#   for(j in 1:nrow(new.d)){
#     if(gg.status.n.male[i,1]==new.d[j,1] & gg.status.n.male[i,2]==new.d[j,2]){
#       new.d[j,12] = gg.status.n.male[i,4]
#       break
#     }
#   }
# }
gg.status.n.female<-gg.status[gg.status[,3]=="Never" & gg.status[,8]=="3GEN", ]
# for(i in 1:nrow(gg.status.n.female)){
#   for(j in 1:nrow(new.d)){
#     if(gg.status.n.female[i,1]==new.d[j,1] & gg.status.n.female[i,2]==new.d[j,2]){
#       new.d[j,13] = gg.status.n.female[i,4]
#       break
#     }
#   }
# }
# # add smoking frequency and smoking status which have overal gender to the data frame
gg.status.c.all<-gg.status[gg.status[,3]=="Current" & gg.status[,8]=="1GEN", ]
# for(i in 1:nrow(gg.status.c.all)){
#   for(j in 1:nrow(new.d)){
#     if(gg.status.c.all[i,1]==new.d[j,1] & gg.status.c.all[i,2]==new.d[j,2]){
#       new.d[j,14] = gg.status.c.all[i,4]
#       break
#     }
#   }
# }
gg.status.f.all<-gg.status[gg.status[,3]=="Former" & gg.status[,8]=="1GEN", ]
# for(i in 1:nrow(gg.status.f.all)){
#   for(j in 1:nrow(new.d)){
#     if(gg.status.f.all[i,1]==new.d[j,1] & gg.status.f.all[i,2]==new.d[j,2]){
#       new.d[j,15] = gg.status.f.all[i,4]
#       break
#     }
#   }
# }
gg.status.n.all<-gg.status[gg.status[,3]=="Never" & gg.status[,8]=="1GEN", ]
# for(i in 1:nrow(gg.status.n.all)){
#   for(j in 1:nrow(new.d)){
#     if(gg.status.n.all[i,1]==new.d[j,1] & gg.status.n.all[i,2]==new.d[j,2]){
#       new.d[j,16] = gg.status.n.all[i,4]
#       break
#     }
#   }
# }
# new.d$FEA<-rep(0,nrow(new.d))
# new.d$FSA<-rep(0,nrow(new.d))
gg.frequecy.e<-gg.frequecy.s.all[gg.frequecy.s.all[,3]=="Every Day" & gg.frequecy.s.all[,8]=="1GEN" ,]
# for(i in 1:nrow(gg.frequecy.e)){
#   for(j in 1:nrow(new.d)){
#     if(gg.frequecy.e[i,1]==new.d[j,1] & gg.frequecy.e[i,2]==new.d[j,2]){
#       new.d[j,17] = gg.frequecy.e[i,4]
#       break
#     }
#   }
# }
gg.frequecy.s<-gg.frequecy.s.all[gg.frequecy.s.all[,3]=="Some Days" & gg.frequecy.s.all[,8]=="1GEN" ,]
# for(i in 1:nrow(gg.frequecy.s)){
#   for(j in 1:nrow(new.d)){
#     if(gg.frequecy.s[i,1]==new.d[j,1] & gg.frequecy.s[i,2]==new.d[j,2]){
#       new.d[j,18] = gg.frequecy.s[i,4]
#       break
#     }
#   }
# }
# ##add longtitude and latitude to the new.d
# 
# new.d$long<-rep(0,nrow(new.d))
# new.d$lat<-rep(0,nrow(new.d))
# 
# for(i in 1:length(t)){
#   l<-nrow(new.d[new.d[,2]==t[i],])
#   new.d[new.d[,2]==t[i],]$long<-rep(f[i,1],l)
#   new.d[new.d[,2]==t[i],]$lat<-rep(f[i,2],l)
# }
# 
# # we also create several longtitude and latitude variable so that we can show different variables on one map 
# new.d$lat1 =new.d$lat-0.75
# new.d$lat2 =new.d$lat-0.25
# new.d$lat3 =new.d$lat+0.25
# new.d$lat4 =new.d$lat+0.75
# 
# new.d$long<-rep(0,nrow(new.d))
# new.d$lat<-rep(0,nrow(new.d))
# new.d$long2 = rep(0,nrow(new.d))
# new.d$long3 = rep(0,nrow(new.d))
# 
# for(i in 1:length(t)){
#   l<-nrow(new.d[new.d[,2]==t[i],])
#   new.d[new.d[,2]==t[i],]$long<-rep(f[i,1],l)
#   new.d[new.d[,2]==t[i],]$lat<-rep(f[i,2],l)
#   new.d[new.d[,2]==t[i],]$long2<-rep(f[i,1],l)
#   new.d[new.d[,2]==t[i],]$long3<-rep(f[i,1],l)
# }
# # now we output this new data set so that we do not need to run the code above everytime we 
# # open this file
# install.packages("MASS")
# library(MASS)
# write.csv(new.d, file = "new.d.csv")
########################################################################################################
########################################################################################################
######################################code above is for creating new plot

## smoking frequency-everyday

#1996
every1 <- gg.frequecy.e[which(gg.frequecy.e$YEAR==1996),]
names(every1) <- tolower(names(every1))
colnames(every1)[2]<-"region"
every1$region<-tolower(every1$region)
every1<-every1[-c(3,5:12)]
choro.eve1 <- merge(states, every1, sort = FALSE, by = "region")
choro.eve1 <- choro.eve1[order(choro.eve1$order), ]
ggplot(choro.eve1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=75,name="percentage for \n all gender \n smoking every \n day")+
  geom_point(aes(x = long, y = lat1, size = FEM), data = new.d[which(new.d$Year == 1996),], alpha = .5,color = "black")+
  geom_point(aes(x = long, y = lat3, size = FEF), data = new.d[which(new.d$Year == 1996),], alpha = .5,color = "white")+
  scale_size(limits = c(50,90),name = "Percentage of \n male/female \n smoking \nevery day")+
  ggtitle("smoking frequency(everyday) for female, male and all in 1996")

#1997
every1 <- gg.frequecy.e[which(gg.frequecy.e$YEAR==1997),]
names(every1) <- tolower(names(every1))
colnames(every1)[2]<-"region"
every1$region<-tolower(every1$region)
every1<-every1[-c(3,5:12)]
choro.eve1 <- merge(states, every1, sort = FALSE, by = "region")
choro.eve1 <- choro.eve1[order(choro.eve1$order), ]
ggplot(choro.eve1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=75,name="percentage for \n all gender \n smoking every \n day")+
  geom_point(aes(x = long, y = lat1, size = FEM), data = new.d[which(new.d$Year == 1997),], alpha = .5,color = "black")+
  geom_point(aes(x = long, y = lat3, size = FEF), data = new.d[which(new.d$Year == 1997),], alpha = .5,color = "white")+
  scale_size(limits = c(50,90),name = "Percentage of \n male/female \n smoking \nevery day")+
  ggtitle("smoking frequency(everyday) for female, male and all in 1997")

#1998
every1 <- gg.frequecy.e[which(gg.frequecy.e$YEAR==1998),]
names(every1) <- tolower(names(every1))
colnames(every1)[2]<-"region"
every1$region<-tolower(every1$region)
every1<-every1[-c(3,5:12)]
choro.eve1 <- merge(states, every1, sort = FALSE, by = "region")
choro.eve1 <- choro.eve1[order(choro.eve1$order), ]
ggplot(choro.eve1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=75,name="percentage for \n all gender \n smoking every \n day")+
  geom_point(aes(x = long, y = lat1, size = FEM), data = new.d[which(new.d$Year == 1998),], alpha = .5,color = "black")+
  geom_point(aes(x = long, y = lat3, size = FEF), data = new.d[which(new.d$Year == 1998),], alpha = .5,color = "white")+
  scale_size(limits = c(50,90),name = "Percentage of \n male/female \n smoking \nevery day")+
  ggtitle("smoking frequency(everyday) for female, male and all in 1998")

#1999
every1 <- gg.frequecy.e[which(gg.frequecy.e$YEAR==1999),]
names(every1) <- tolower(names(every1))
colnames(every1)[2]<-"region"
every1$region<-tolower(every1$region)
every1<-every1[-c(3,5:12)]
choro.eve1 <- merge(states, every1, sort = FALSE, by = "region")
choro.eve1 <- choro.eve1[order(choro.eve1$order), ]
ggplot(choro.eve1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=75,name="percentage for \n all gender \n smoking every \n day")+
  geom_point(aes(x = long, y = lat1, size = FEM), data = new.d[which(new.d$Year == 1999),], alpha = .5,color = "black")+
  geom_point(aes(x = long, y = lat3, size = FEF), data = new.d[which(new.d$Year == 1999),], alpha = .5,color = "white")+
  scale_size(limits = c(50,90),name = "Percentage of \n male/female \n smoking \nevery day")+
  ggtitle("smoking frequency(everyday) for female, male and all in 1999")

#2000
every1 <- gg.frequecy.e[which(gg.frequecy.e$YEAR==2000),]
names(every1) <- tolower(names(every1))
colnames(every1)[2]<-"region"
every1$region<-tolower(every1$region)
every1<-every1[-c(3,5:12)]
choro.eve1 <- merge(states, every1, sort = FALSE, by = "region")
choro.eve1 <- choro.eve1[order(choro.eve1$order), ]
ggplot(choro.eve1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=75,name="percentage for \n all gender \n smoking every \n day")+
  geom_point(aes(x = long, y = lat1, size = FEM), data = new.d[which(new.d$Year == 2000),], alpha = .5,color = "black")+
  geom_point(aes(x = long, y = lat3, size = FEF), data = new.d[which(new.d$Year == 2000),], alpha = .5,color = "white")+
  scale_size(limits = c(50,90),name = "Percentage of \n male/female \n smoking \nevery day")+
  ggtitle("smoking frequency(everyday) for female, male and all in 2000")

#2001
every1 <- gg.frequecy.e[which(gg.frequecy.e$YEAR==2001),]
names(every1) <- tolower(names(every1))
colnames(every1)[2]<-"region"
every1$region<-tolower(every1$region)
every1<-every1[-c(3,5:12)]
choro.eve1 <- merge(states, every1, sort = FALSE, by = "region")
choro.eve1 <- choro.eve1[order(choro.eve1$order), ]
ggplot(choro.eve1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=75,name="percentage for \n all gender \n smoking every \n day")+
  geom_point(aes(x = long, y = lat1, size = FEM), data = new.d[which(new.d$Year == 2001),], alpha = .5,color = "black")+
  geom_point(aes(x = long, y = lat3, size = FEF), data = new.d[which(new.d$Year == 2001),], alpha = .5,color = "white")+
  scale_size(limits = c(50,90),name = "Percentage of \n male/female \n smoking \nevery day")+
  ggtitle("smoking frequency(everyday) for female, male and all in 2001")

#2002
every1 <- gg.frequecy.e[which(gg.frequecy.e$YEAR==2002),]
names(every1) <- tolower(names(every1))
colnames(every1)[2]<-"region"
every1$region<-tolower(every1$region)
every1<-every1[-c(3,5:12)]
choro.eve1 <- merge(states, every1, sort = FALSE, by = "region")
choro.eve1 <- choro.eve1[order(choro.eve1$order), ]
ggplot(choro.eve1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=75,name="percentage for \n all gender \n smoking every \n day")+
  geom_point(aes(x = long, y = lat1, size = FEM), data = new.d[which(new.d$Year == 2002),], alpha = .5,color = "black")+
  geom_point(aes(x = long, y = lat3, size = FEF), data = new.d[which(new.d$Year == 2002),], alpha = .5,color = "white")+
  scale_size(limits = c(50,90),name = "Percentage of \n male/female \n smoking \nevery day")+
  ggtitle("smoking frequency(everyday) for female, male and all in 2002")
#2003
every1 <- gg.frequecy.e[which(gg.frequecy.e$YEAR==2003),]
names(every1) <- tolower(names(every1))
colnames(every1)[2]<-"region"
every1$region<-tolower(every1$region)
every1<-every1[-c(3,5:12)]
choro.eve1 <- merge(states, every1, sort = FALSE, by = "region")
choro.eve1 <- choro.eve1[order(choro.eve1$order), ]
ggplot(choro.eve1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=75,name="percentage for \n all gender \n smoking every \n day")+
  geom_point(aes(x = long, y = lat1, size = FEM), data = new.d[which(new.d$Year == 2003),], alpha = .5,color = "black")+
  geom_point(aes(x = long, y = lat3, size = FEF), data = new.d[which(new.d$Year == 2003),], alpha = .5,color = "white")+
  scale_size(limits = c(50,90),name = "Percentage of \n male/female \n smoking \nevery day")+
  ggtitle("smoking frequency(everyday) for female, male and all in 2003")
#2004
every1 <- gg.frequecy.e[which(gg.frequecy.e$YEAR==2004),]
names(every1) <- tolower(names(every1))
colnames(every1)[2]<-"region"
every1$region<-tolower(every1$region)
every1<-every1[-c(3,5:12)]
choro.eve1 <- merge(states, every1, sort = FALSE, by = "region")
choro.eve1 <- choro.eve1[order(choro.eve1$order), ]
ggplot(choro.eve1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=75,name="percentage for \n all gender \n smoking every \n day")+
  geom_point(aes(x = long, y = lat1, size = FEM), data = new.d[which(new.d$Year == 2004),], alpha = .5,color = "black")+
  geom_point(aes(x = long, y = lat3, size = FEF), data = new.d[which(new.d$Year == 2004),], alpha = .5,color = "white")+
  scale_size(limits = c(50,90),name = "Percentage of \n male/female \n smoking \nevery day")+
  ggtitle("smoking frequency(everyday) for female, male and all in 2004")
#2005
every1 <- gg.frequecy.e[which(gg.frequecy.e$YEAR==2005),]
names(every1) <- tolower(names(every1))
colnames(every1)[2]<-"region"
every1$region<-tolower(every1$region)
every1<-every1[-c(3,5:12)]
choro.eve1 <- merge(states, every1, sort = FALSE, by = "region")
choro.eve1 <- choro.eve1[order(choro.eve1$order), ]
ggplot(choro.eve1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=75,name="percentage for \n all gender \n smoking every \n day")+
  geom_point(aes(x = long, y = lat1, size = FEM), data = new.d[which(new.d$Year == 2005),], alpha = .5,color = "black")+
  geom_point(aes(x = long, y = lat3, size = FEF), data = new.d[which(new.d$Year == 2005),], alpha = .5,color = "white")+
  scale_size(limits = c(50,90),name = "Percentage of \n male/female \n smoking \nevery day")+
  ggtitle("smoking frequency(everyday) for female, male and all in 2005")
#2006
every1 <- gg.frequecy.e[which(gg.frequecy.e$YEAR==2006),]
names(every1) <- tolower(names(every1))
colnames(every1)[2]<-"region"
every1$region<-tolower(every1$region)
every1<-every1[-c(3,5:12)]
choro.eve1 <- merge(states, every1, sort = FALSE, by = "region")
choro.eve1 <- choro.eve1[order(choro.eve1$order), ]
ggplot(choro.eve1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=75,name="percentage for \n all gender \n smoking every \n day")+
  geom_point(aes(x = long, y = lat1, size = FEM), data = new.d[which(new.d$Year == 2006),], alpha = .5,color = "black")+
  geom_point(aes(x = long, y = lat3, size = FEF), data = new.d[which(new.d$Year == 2006),], alpha = .5,color = "white")+
  scale_size(limits = c(50,90),name = "Percentage of \n male/female \n smoking \nevery day")+
  ggtitle("smoking frequency(everyday) for female, male and all in 2006")
#2007
every1 <- gg.frequecy.e[which(gg.frequecy.e$YEAR==2007),]
names(every1) <- tolower(names(every1))
colnames(every1)[2]<-"region"
every1$region<-tolower(every1$region)
every1<-every1[-c(3,5:12)]
choro.eve1 <- merge(states, every1, sort = FALSE, by = "region")
choro.eve1 <- choro.eve1[order(choro.eve1$order), ]
ggplot(choro.eve1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=75,name="percentage for \n all gender \n smoking every \n day")+
  geom_point(aes(x = long, y = lat1, size = FEM), data = new.d[which(new.d$Year == 2007),], alpha = .5,color = "black")+
  geom_point(aes(x = long, y = lat3, size = FEF), data = new.d[which(new.d$Year == 2007),], alpha = .5,color = "white")+
  scale_size(limits = c(50,90),name = "Percentage of \n male/female \n smoking \nevery day")+
  ggtitle("smoking frequency(everyday) for female, male and all in 2007")
#2008
every1 <- gg.frequecy.e[which(gg.frequecy.e$YEAR==2008),]
names(every1) <- tolower(names(every1))
colnames(every1)[2]<-"region"
every1$region<-tolower(every1$region)
every1<-every1[-c(3,5:12)]
choro.eve1 <- merge(states, every1, sort = FALSE, by = "region")
choro.eve1 <- choro.eve1[order(choro.eve1$order), ]
ggplot(choro.eve1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=75,name="percentage for \n all gender \n smoking every \n day")+
  geom_point(aes(x = long, y = lat1, size = FEM), data = new.d[which(new.d$Year == 2008),], alpha = .5,color = "black")+
  geom_point(aes(x = long, y = lat3, size = FEF), data = new.d[which(new.d$Year == 2008),], alpha = .5,color = "white")+
  scale_size(limits = c(50,90),name = "Percentage of \n male/female \n smoking \nevery day")+
  ggtitle("smoking frequency(everyday) for female, male and all in 2008")
#2009
every1 <- gg.frequecy.e[which(gg.frequecy.e$YEAR==2009),]
names(every1) <- tolower(names(every1))
colnames(every1)[2]<-"region"
every1$region<-tolower(every1$region)
every1<-every1[-c(3,5:12)]
choro.eve1 <- merge(states, every1, sort = FALSE, by = "region")
choro.eve1 <- choro.eve1[order(choro.eve1$order), ]
ggplot(choro.eve1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=75,name="percentage for \n all gender \n smoking every \n day")+
  geom_point(aes(x = long, y = lat1, size = FEM), data = new.d[which(new.d$Year == 2009),], alpha = .5,color = "black")+
  geom_point(aes(x = long, y = lat3, size = FEF), data = new.d[which(new.d$Year == 2009),], alpha = .5,color = "white")+
  scale_size(limits = c(50,90),name = "Percentage of \n male/female \n smoking \nevery day")+
  ggtitle("smoking frequency(everyday) for female, male and all in 2009")
#2010
every1 <- gg.frequecy.e[which(gg.frequecy.e$YEAR==2010),]
names(every1) <- tolower(names(every1))
colnames(every1)[2]<-"region"
every1$region<-tolower(every1$region)
every1<-every1[-c(3,5:12)]
choro.eve1 <- merge(states, every1, sort = FALSE, by = "region")
choro.eve1 <- choro.eve1[order(choro.eve1$order), ]
ggplot(choro.eve1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=75,name="percentage for \n all gender \n smoking every \n day")+
  geom_point(aes(x = long, y = lat1, size = FEM), data = new.d[which(new.d$Year == 2010),], alpha = .5,color = "black")+
  geom_point(aes(x = long, y = lat3, size = FEF), data = new.d[which(new.d$Year == 2010),], alpha = .5,color = "white")+
  scale_size(limits = c(50,90),name = "Percentage of \n male/female \n smoking \nevery day")+
  ggtitle("smoking frequency(everyday) for female, male and all in 2010")

# smoking frequency- some days
#############################################################################

# 1996

some1 <- gg.frequecy.s[which(gg.frequecy.s$YEAR==1996),]
names(some1) <- tolower(names(some1))
colnames(some1)[2]<-"region"
some1$region<-tolower(some1$region)
some1<-some1[-c(3,5:12)]
choro.some1 <- merge(states, some1, sort = FALSE, by = "region")
choro.some1 <- choro.some1[order(choro.some1$order), ]
ggplot(choro.some1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20,name="percentage for \n all gender \n smoking some\n day")+
  geom_point(aes(x = long3, y = lat1, size = FSM), data = new.d[which(new.d$Year == 1996),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = FSF), data = new.d[which(new.d$Year == 1996),], alpha = .5,color = "white")+
  scale_size(limits = c(0,40),name = "Percentage of \n male/female \n smoking \nsome days")+
  ggtitle("smoking frequency(some days) for female, male and all in 1996")

#1997

some2 <- gg.frequecy.s[which(gg.frequecy.s$YEAR==1997),]
names(some2) <- tolower(names(some2))
colnames(some2)[2]<-"region"
some2$region<-tolower(some2$region)
some2<-some2[-c(3,5:12)]
choro.some2 <- merge(states, some2, sort = FALSE, by = "region")
choro.some2 <- choro.some2[order(choro.some2$order), ]
ggplot(choro.some2,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20,name="percentage for \n all gender \n smoking some\n day")+
  geom_point(aes(x = long3, y = lat1, size = FSM), data = new.d[which(new.d$Year == 1997),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = FSF), data = new.d[which(new.d$Year == 1997),], alpha = .5,color = "white")+
  scale_size(limits = c(0,40),name = "Percentage of \n male/female \n smoking \nsome days")+
  ggtitle("smoking frequency(some days) for female, male and all in 1997")

#1998

some3 <- gg.frequecy.s[which(gg.frequecy.s$YEAR==1998),]
names(some3) <- tolower(names(some3))
colnames(some3)[2]<-"region"
some3$region<-tolower(some3$region)
some3<-some3[-c(3,5:12)]
choro.some3 <- merge(states, some3, sort = FALSE, by = "region")
choro.some3 <- choro.some3[order(choro.some3$order), ]
ggplot(choro.some3,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20,name="percentage for \n all gender \n smoking some\n day")+
  geom_point(aes(x = long3, y = lat1, size = FSM), data = new.d[which(new.d$Year == 1998),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = FSF), data = new.d[which(new.d$Year == 1998),], alpha = .5,color = "white")+
  scale_size(limits = c(0,40),name = "Percentage of \n male/female \n smoking \nsome days")+
  ggtitle("smoking frequency(some days) for female, male and all in 1998")

#1999

some4 <- gg.frequecy.s[which(gg.frequecy.s$YEAR==1999),]
names(some4) <- tolower(names(some4))
colnames(some4)[2]<-"region"
some4$region<-tolower(some4$region)
some4<-some4[-c(3,5:12)]
choro.some4 <- merge(states, some4, sort = FALSE, by = "region")
choro.some4 <- choro.some4[order(choro.some4$order), ]
ggplot(choro.some4,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20,name="percentage for \n all gender \n smoking some\n day")+
  geom_point(aes(x = long3, y = lat1, size = FSM), data = new.d[which(new.d$Year == 1999),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = FSF), data = new.d[which(new.d$Year == 1999),], alpha = .5,color = "white")+
  scale_size(limits = c(0,40),name = "Percentage of \n male/female \n smoking \nsome days")+
  ggtitle("smoking frequency(some days) for female, male and all in 1999")

#2000

some5 <- gg.frequecy.s[which(gg.frequecy.s$YEAR==2000),]
names(some5) <- tolower(names(some5))
colnames(some5)[2]<-"region"
some5$region<-tolower(some5$region)
some5<-some5[-c(3,5:12)]
choro.some5 <- merge(states, some5, sort = FALSE, by = "region")
choro.some5 <- choro.some5[order(choro.some5$order), ]
ggplot(choro.some5,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20,name="percentage for \n all gender \n smoking some\n day")+
  geom_point(aes(x = long3, y = lat1, size = FSM), data = new.d[which(new.d$Year == 2000),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = FSF), data = new.d[which(new.d$Year == 2000),], alpha = .5,color = "white")+
  scale_size(limits = c(0,40),name = "Percentage of \n male/female \n smoking \nsome days")+
  ggtitle("smoking frequency(some days) for female, male and all in 2000")

#2001

some6 <- gg.frequecy.s[which(gg.frequecy.s$YEAR==2001),]
names(some6) <- tolower(names(some6))
colnames(some6)[2]<-"region"
some6$region<-tolower(some6$region)
some6<-some6[-c(3,5:12)]
choro.eve6 <- merge(states, some6, sort = FALSE, by = "region")
choro.eve6 <- choro.eve6[order(choro.eve6$order), ]
ggplot(choro.eve6,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20,name="percentage for \n all gender \n smoking some \n day")+
  geom_point(aes(x = long3, y = lat1, size = FSM), data = new.d[which(new.d$Year == 2001),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = FSF), data = new.d[which(new.d$Year == 2001),], alpha = .5,color = "white")+
  scale_size(limits = c(0,40),name = "Percentage of \n male/female \n smoking \nsome days")+
  ggtitle("smoking frequency(some days) for female, male and all in 2001")

#2002

some8 <- gg.frequecy.s[which(gg.frequecy.s$YEAR==2002),]
names(some8) <- tolower(names(some8))
colnames(some8)[2]<-"region"
some8$region<-tolower(some8$region)
some8<-some8[-c(3,5:12)]
choro.some8 <- merge(states, some8, sort = FALSE, by = "region")
choro.some8 <- choro.some8[order(choro.some8$order), ]
ggplot(choro.some8,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20,name="percentage for \n all gender \n smoking some \n day")+
  geom_point(aes(x = long3, y = lat1, size = FSM), data = new.d[which(new.d$Year == 2002),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = FSF), data = new.d[which(new.d$Year == 2002),], alpha = .5,color = "white")+
  scale_size(limits = c(0,40),name = "Percentage of \n male/female \n smoking \nsome days")+
  ggtitle("smoking frequency(some days) for female, male and all in 2002")

#2003

some9 <- gg.frequecy.s[which(gg.frequecy.s$YEAR==2003),]
names(some9) <- tolower(names(some9))
colnames(some9)[2]<-"region"
some9$region<-tolower(some9$region)
some9<-some9[-c(3,5:12)]
choro.some9 <- merge(states, some9, sort = FALSE, by = "region")
choro.some9 <- choro.some9[order(choro.some9$order), ]
ggplot(choro.some9,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20,name="percentage for \n all gender \n smoking some \n day")+
  geom_point(aes(x = long3, y = lat1, size = FSM), data = new.d[which(new.d$Year == 2003),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = FSF), data = new.d[which(new.d$Year == 2003),], alpha = .5,color = "white")+
  scale_size(limits = c(0,40),name = "Percentage of \n male/female \n smoking \nsome days")+
  ggtitle("smoking frequency(some days) for female, male and all in 2003")

# 2004

some10 <- gg.frequecy.s[which(gg.frequecy.s$YEAR==2004),]
names(some10) <- tolower(names(some10))
colnames(some10)[2]<-"region"
some10$region<-tolower(some10$region)
some10<-some10[-c(3,5:12)]
choro.some10 <- merge(states, some10, sort = FALSE, by = "region")
choro.some10 <- choro.some10[order(choro.some10$order), ]
ggplot(choro.some10,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20,name="percentage for \n all gender \n smoking some \n day")+
  geom_point(aes(x = long3, y = lat1, size = FSM), data = new.d[which(new.d$Year == 2004),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = FSF), data = new.d[which(new.d$Year == 2004),], alpha = .5,color = "white")+
  scale_size(limits = c(0,40),name = "Percentage of \n male/female \n smoking \nsome days")+
  ggtitle("smoking frequency(some days) for female, male and all in 2004")

# 2005

some11 <- gg.frequecy.s[which(gg.frequecy.s$YEAR==2005),]
names(some11) <- tolower(names(some11))
colnames(some11)[2]<-"region"
some11$region<-tolower(some11$region)
some11<-some11[-c(3,5:12)]
choro.some11 <- merge(states, some11, sort = FALSE, by = "region")
choro.some11 <- choro.some11[order(choro.some11$order), ]
ggplot(choro.some11,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20,name="percentage for \n all gender \n smoking some\n day")+
  geom_point(aes(x = long3, y = lat1, size = FSM), data = new.d[which(new.d$Year == 2005),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = FSF), data = new.d[which(new.d$Year == 2005),], alpha = .5,color = "white")+
  scale_size(limits = c(0,40),name = "Percentage of \n male/female \n smoking \nsome days")+
  ggtitle("smoking frequency(some days) for female, male and all in 2005")

# 2006

some12 <- gg.frequecy.s[which(gg.frequecy.s$YEAR==2006),]
names(some12) <- tolower(names(some12))
colnames(some12)[2]<-"region"
some12$region<-tolower(some12$region)
some12<-some12[-c(3,5:12)]
choro.some12 <- merge(states, some12, sort = FALSE, by = "region")
choro.some12 <- choro.some12[order(choro.some12$order), ]
ggplot(choro.some12,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20,name="percentage for \n all gender \n smoking some\n day")+
  geom_point(aes(x = long3, y = lat1, size = FSM), data = new.d[which(new.d$Year == 2006),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = FSF), data = new.d[which(new.d$Year == 2006),], alpha = .5,color = "white")+
  scale_size(limits = c(0,40),name = "Percentage of \n male/female \n smoking \nsome days")+
  ggtitle("smoking frequency(some days) for female, male and all in 2006")

# 2007

some13 <- gg.frequecy.s[which(gg.frequecy.s$YEAR==2007),]
names(some13) <- tolower(names(some13))
colnames(some13)[2]<-"region"
some13$region<-tolower(some13$region)
some13<-some13[-c(3,5:12)]
choro.some13 <- merge(states, some13, sort = FALSE, by = "region")
choro.some13 <- choro.some13[order(choro.some13$order), ]
ggplot(choro.some13,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20,name="percentage for \n all gender \n smoking some\n day")+
  geom_point(aes(x = long3, y = lat1, size = FSM), data = new.d[which(new.d$Year == 2007),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = FSF), data = new.d[which(new.d$Year == 2007),], alpha = .5,color = "white")+
  scale_size(limits = c(0,40),name = "Percentage of \n male/female \n smoking \nsome days")+
  ggtitle("smoking frequency(some days) for female, male and all in 2007")

# 2008

some14 <- gg.frequecy.s[which(gg.frequecy.s$YEAR==2008),]
names(some14) <- tolower(names(some14))
colnames(some14)[2]<-"region"
some14$region<-tolower(some14$region)
some14<-some14[-c(3,5:12)]
choro.some14 <- merge(states, some14, sort = FALSE, by = "region")
choro.some14 <- choro.some14[order(choro.some14$order), ]
ggplot(choro.some14,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20,name="percentage for \n all gender \n smoking some\n day")+
  geom_point(aes(x = long3, y = lat1, size = FSM), data = new.d[which(new.d$Year == 2008),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = FSF), data = new.d[which(new.d$Year == 2008),], alpha = .5,color = "white")+
  scale_size(limits = c(0,40),name = "Percentage of \n male/female \n smoking \nsome days")+
  ggtitle("smoking frequency(some days) for female, male and all in 2008")

# 2009

some15 <- gg.frequecy.s[which(gg.frequecy.s$YEAR==2009),]
names(some15) <- tolower(names(some15))
colnames(some15)[2]<-"region"
some15$region<-tolower(some15$region)
some15<-some15[-c(3,5:12)]
choro.some15 <- merge(states, some15, sort = FALSE, by = "region")
choro.some15 <- choro.some15[order(choro.some15$order), ]
ggplot(choro.some15,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20,name="percentage for \n all gender \n smoking some\n day")+
  geom_point(aes(x = long3, y = lat1, size = FSM), data = new.d[which(new.d$Year == 2009),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = FSF), data = new.d[which(new.d$Year == 2009),], alpha = .5,color = "white")+
  scale_size(limits = c(0,40),name = "Percentage of \n male/female \n smoking \nsome days")+
  ggtitle("smoking frequency(some days) for female, male and all in 2009")

# 2010
some16 <- gg.frequecy.s[which(gg.frequecy.s$YEAR==2010),]
names(some16) <- tolower(names(some16))
colnames(some16)[2]<-"region"
some16$region<-tolower(some16$region)
some16<-some16[-c(3,5:12)]
choro.some16 <- merge(states, some16, sort = FALSE, by = "region")
choro.some16 <- choro.some16[order(choro.some16$order), ]
ggplot(choro.some16,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=20,name="percentage for \n all gender \n smoking some\n day")+
  geom_point(aes(x = long3, y = lat1, size = FSM), data = new.d[which(new.d$Year == 2010),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = FSF), data = new.d[which(new.d$Year == 2010),], alpha = .5,color = "white")+
  scale_size(limits = c(0,40),name = "Percentage of \n male/female \n smoking \nsome days")+
  ggtitle("smoking frequency(some days) for female, male and all in 2010")

###########################################################################################
# smoking status- Never

# 1996

never1 <- gg.status.n.all[which(gg.status.n.all$YEAR==1996),]
names(never1) <- tolower(names(never1))
colnames(never1)[2]<-"region"
never1$region<-tolower(never1$region)
never1<-never1[-c(3,5:12)]
choro.never1 <- merge(states, never1, sort = FALSE, by = "region")
choro.never1 <- choro.never1[order(choro.never1$order), ]
ggplot(choro.never1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=57,name = "Percentage of \n all gender \n who never smoke")+
  geom_point(aes(x = long3, y = lat1, size = SNM), data = new.d[which(new.d$Year == 1996),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SNF), data = new.d[which(new.d$Year == 1996),], alpha = .5,color = "white")+
  scale_size(limits = c(40,70),name = "Percentage of \n male/female \n who never smoke")+
  ggtitle("smoking status(Never) for male, female and all gender in 1996")

#1997

never2 <- gg.status.n.all[which(gg.status.n.all$YEAR==1997),]
names(never2) <- tolower(names(never2))
colnames(never2)[2]<-"region"
never2$region<-tolower(never2$region)
never2<-never2[-c(3,5:12)]
choro.never2 <- merge(states, never2, sort = FALSE, by = "region")
choro.never2 <- choro.never2[order(choro.never2$order), ]
ggplot(choro.never2,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=57,name = "Percentage of \n all gender \n who never smoke")+
  geom_point(aes(x = long3, y = lat1, size = SNM), data = new.d[which(new.d$Year == 1997),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SNF), data = new.d[which(new.d$Year == 1997),], alpha = .5,color = "white")+
  scale_size(limits = c(40,70),name = "Percentage of \n male/female \n who never smoke")+
  ggtitle("smoking status(Never) for male, female and all gender in 1997")

#1998

never3 <- gg.status.n.all[which(gg.status.n.all$YEAR==1998),]
names(never3) <- tolower(names(never3))
colnames(never3)[2]<-"region"
never3$region<-tolower(never3$region)
never3<-never3[-c(3,5:12)]
choro.never3 <- merge(states, never3, sort = FALSE, by = "region")
choro.never3 <- choro.never3[order(choro.never3$order), ]
ggplot(choro.never3,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=57,name = "Percentage of \n all gender \n who never smoke")+
  geom_point(aes(x = long3, y = lat1, size = SNM), data = new.d[which(new.d$Year == 1998),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SNF), data = new.d[which(new.d$Year == 1998),], alpha = .5,color = "white")+
  scale_size(limits = c(40,70),name = "Percentage of \n male/female \n who never smoke")+
  ggtitle("smoking status(Never) for male, female and all gender in 1998")

#1999

never4 <- gg.status.n.all[which(gg.status.n.all$YEAR==1999),]
names(never4) <- tolower(names(never4))
colnames(never4)[2]<-"region"
never4$region<-tolower(never4$region)
never4<-never4[-c(3,5:12)]
choro.never4 <- merge(states, never4, sort = FALSE, by = "region")
choro.never4 <- choro.never4[order(choro.never4$order), ]
ggplot(choro.never4,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=57,name = "Percentage of \n all gender \n who never smoke")+
  geom_point(aes(x = long3, y = lat1, size = SNM), data = new.d[which(new.d$Year == 1999),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SNF), data = new.d[which(new.d$Year == 1999),], alpha = .5,color = "white")+
  scale_size(limits = c(40,70),name = "Percentage of \n male/female \n who never smoke")+
  ggtitle("smoking status(Never) for male, female and all gender in 1999")

#2000

never5 <- gg.status.n.all[which(gg.status.n.all$YEAR==2000),]
names(never5) <- tolower(names(never5))
colnames(never5)[2]<-"region"
never5$region<-tolower(never5$region)
never5<-never5[-c(3,5:12)]
choro.never5 <- merge(states, never5, sort = FALSE, by = "region")
choro.never5 <- choro.never5[order(choro.never5$order), ]
ggplot(choro.never5,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=57,name = "Percentage of \n all gender \n who never smoke")+
  geom_point(aes(x = long3, y = lat1, size = SNM), data = new.d[which(new.d$Year == 2000),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SNF), data = new.d[which(new.d$Year == 2000),], alpha = .5,color = "white")+
  scale_size(limits = c(40,70),name = "Percentage of \n male/female \n who never smoke")+
  ggtitle("smoking status(Never) for male, female and all gender in 2000")

#2001

never6 <- gg.status.n.all[which(gg.status.n.all$YEAR==2001),]
names(never6) <- tolower(names(never6))
colnames(never6)[2]<-"region"
never6$region<-tolower(never6$region)
never6<-never6[-c(3,5:12)]
choro.eve6 <- merge(states, never6, sort = FALSE, by = "region")
choro.eve6 <- choro.eve6[order(choro.eve6$order), ]
ggplot(choro.eve6,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=57,name = "Percentage of \n all gender \n who never smoke")+
  geom_point(aes(x = long3, y = lat1, size = SNM), data = new.d[which(new.d$Year == 2001),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SNF), data = new.d[which(new.d$Year == 2001),], alpha = .5,color = "white")+
  scale_size(limits = c(40,70),name = "Percentage of \n male/female \n who never smoke")+
  ggtitle("smoking status(Never) for male, female and all gender in 2001")

#2002

never8 <- gg.status.n.all[which(gg.status.n.all$YEAR==2002),]
names(never8) <- tolower(names(never8))
colnames(never8)[2]<-"region"
never8$region<-tolower(never8$region)
never8<-never8[-c(3,5:12)]
choro.never8 <- merge(states, never8, sort = FALSE, by = "region")
choro.never8 <- choro.never8[order(choro.never8$order), ]
ggplot(choro.never8,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=57,name = "Percentage of \n all gender \n who never smoke")+
  geom_point(aes(x = long3, y = lat1, size = SNM), data = new.d[which(new.d$Year == 2002),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SNF), data = new.d[which(new.d$Year == 2002),], alpha = .5,color = "white")+
  scale_size(limits = c(40,70),name = "Percentage of \n male/female \n who never smoke")+
  ggtitle("smoking status(Never) for male, female and all gender in 2002")

#2003

never9 <- gg.status.n.all[which(gg.status.n.all$YEAR==2003),]
names(never9) <- tolower(names(never9))
colnames(never9)[2]<-"region"
never9$region<-tolower(never9$region)
never9<-never9[-c(3,5:12)]
choro.never9 <- merge(states, never9, sort = FALSE, by = "region")
choro.never9 <- choro.never9[order(choro.never9$order), ]
ggplot(choro.never9,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=57,name = "Percentage of \n all gender \n who never smoke")+
  geom_point(aes(x = long3, y = lat1, size = SNM), data = new.d[which(new.d$Year == 2003),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SNF), data = new.d[which(new.d$Year == 2003),], alpha = .5,color = "white")+
  scale_size(limits = c(40,70),name = "Percentage of \n male/female \n who never smoke")+
  ggtitle("smoking status(Never) for male, female and all gender in 2003")

# 2004

never10 <- gg.status.n.all[which(gg.status.n.all$YEAR==2004),]
names(never10) <- tolower(names(never10))
colnames(never10)[2]<-"region"
never10$region<-tolower(never10$region)
never10<-never10[-c(3,5:12)]
choro.never10 <- merge(states, never10, sort = FALSE, by = "region")
choro.never10 <- choro.never10[order(choro.never10$order), ]
ggplot(choro.never10,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=57,name = "Percentage of \n all gender \n who never smoke")+
  geom_point(aes(x = long3, y = lat1, size = SNM), data = new.d[which(new.d$Year == 2004),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SNF), data = new.d[which(new.d$Year == 2004),], alpha = .5,color = "white")+
  scale_size(limits = c(40,70),name = "Percentage of \n male/female \n who never smoke")+
  ggtitle("smoking status(Never) for male, female and all gender in 2004")

# 2005

never11 <- gg.status.n.all[which(gg.status.n.all$YEAR==2005),]
names(never11) <- tolower(names(never11))
colnames(never11)[2]<-"region"
never11$region<-tolower(never11$region)
never11<-never11[-c(3,5:12)]
choro.never11 <- merge(states, never11, sort = FALSE, by = "region")
choro.never11 <- choro.never11[order(choro.never11$order), ]
ggplot(choro.never11,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=57,name = "Percentage of \n all gender \n who never smoke")+
  geom_point(aes(x = long3, y = lat1, size = SNM), data = new.d[which(new.d$Year == 2005),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SNF), data = new.d[which(new.d$Year == 2005),], alpha = .5,color = "white")+
  scale_size(limits = c(40,70),name = "Percentage of \n male/female \n who never smoke")+
  ggtitle("smoking status(Never) for male, female and all gender in 2005")

# 2006

never12 <- gg.status.n.all[which(gg.status.n.all$YEAR==2006),]
names(never12) <- tolower(names(never12))
colnames(never12)[2]<-"region"
never12$region<-tolower(never12$region)
never12<-never12[-c(3,5:12)]
choro.never12 <- merge(states, never12, sort = FALSE, by = "region")
choro.never12 <- choro.never12[order(choro.never12$order), ]
ggplot(choro.never12,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=57,name = "Percentage of \n all gender \n who never smoke")+
  geom_point(aes(x = long3, y = lat1, size = SNM), data = new.d[which(new.d$Year == 2006),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SNF), data = new.d[which(new.d$Year == 2006),], alpha = .5,color = "white")+
  scale_size(limits = c(40,70),name = "Percentage of \n male/female \n who never smoke")+
  ggtitle("smoking status(Never) for male, female and all gender in 2006")

# 2007
never13 <- gg.status.n.all[which(gg.status.n.all$YEAR==2007),]
names(never13) <- tolower(names(never13))
colnames(never13)[2]<-"region"
never13$region<-tolower(never13$region)
never13<-never13[-c(3,5:12)]
choro.never13 <- merge(states, never13, sort = FALSE, by = "region")
choro.never13 <- choro.never13[order(choro.never13$order), ]
ggplot(choro.never13,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=57,name = "Percentage of \n all gender \n who never smoke")+
  geom_point(aes(x = long3, y = lat1, size = SNM), data = new.d[which(new.d$Year == 2007),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SNF), data = new.d[which(new.d$Year == 2007),], alpha = .5,color = "white")+
  scale_size(limits = c(40,70),name = "Percentage of \n male/female \n who never smoke")+
  ggtitle("smoking status(Never) for male, female and all gender in 2007")

# 2008
never14 <- gg.status.n.all[which(gg.status.n.all$YEAR==2008),]
names(never14) <- tolower(names(never14))
colnames(never14)[2]<-"region"
never14$region<-tolower(never14$region)
never14<-never14[-c(3,5:12)]
choro.never14 <- merge(states, never14, sort = FALSE, by = "region")
choro.never14 <- choro.never14[order(choro.never14$order), ]
ggplot(choro.never14,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=57,name = "Percentage of \n all gender \n who never smoke")+
  geom_point(aes(x = long3, y = lat1, size = SNM), data = new.d[which(new.d$Year == 2008),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SNF), data = new.d[which(new.d$Year == 2008),], alpha = .5,color = "white")+
  scale_size(limits = c(40,70),name = "Percentage of \n male/female \n who never smoke")+
  ggtitle("smoking status(Never) for male, female and all gender in 2008")

# 2009
never15 <- gg.status.n.all[which(gg.status.n.all$YEAR==2009),]
names(never15) <- tolower(names(never15))
colnames(never15)[2]<-"region"
never15$region<-tolower(never15$region)
never15<-never15[-c(3,5:12)]
choro.never15 <- merge(states, never15, sort = FALSE, by = "region")
choro.never15 <- choro.never15[order(choro.never15$order), ]
ggplot(choro.never15,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=57,name = "Percentage of \n all gender \n who never smoke")+
  geom_point(aes(x = long3, y = lat1, size = SNM), data = new.d[which(new.d$Year == 2009),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SNF), data = new.d[which(new.d$Year == 2009),], alpha = .5,color = "white")+
  scale_size(limits = c(40,70),name = "Percentage of \n male/female \n who never smoke")+
  ggtitle("smoking status(Never) for male, female and all gender in 2009")

# 2010
never16 <- gg.status.n.all[which(gg.status.n.all$YEAR==2010),]
names(never16) <- tolower(names(never16))
colnames(never16)[2]<-"region"
never16$region<-tolower(never16$region)
never16<-never16[-c(3,5:12)]
choro.never16 <- merge(states, never16, sort = FALSE, by = "region")
choro.never16 <- choro.never16[order(choro.never16$order), ]
ggplot(choro.never16,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=57,name = "Percentage of \n all gender \n who never smoke")+
  geom_point(aes(x = long3, y = lat1, size = SNM), data = new.d[which(new.d$Year == 2010),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SNF), data = new.d[which(new.d$Year == 2010),], alpha = .5,color = "white")+
  scale_size(limits = c(40,70),name = "Percentage of \n male/female \n who never smoke")+
  ggtitle("smoking status(Never) for male, female and all gender in 2010")

#################################################################################################
# smoking status- Former

# 1996
states <- map_data("state")
never1 <- gg.status.f.all[which(gg.status.f.all$YEAR==1996),]
names(never1) <- tolower(names(never1))
colnames(never1)[2]<-"region"
never1$region<-tolower(never1$region)
never1<-never1[-c(3,5:12)]
choro.never1 <- merge(states, never1, sort = FALSE, by = "region")
choro.never1 <- choro.never1[order(choro.never1$order), ]
ggplot(choro.never1,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=22,name = "Percentage of \n for all gender \n who were former smokers")+
  geom_point(aes(x = long3, y = lat1, size = SFM), data = new.d[which(new.d$Year == 1996),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SFF), data = new.d[which(new.d$Year == 1996),], alpha = .5,color = "white")+
  scale_size(limits = c(10,35),name = "Percentage of \n male/female \n who were former smokers")+
  ggtitle("Percentage of former smokers among all people in 1996")

#1997
states <- map_data("state")
never2 <- gg.status.f.all[which(gg.status.f.all$YEAR==1997),]
names(never2) <- tolower(names(never2))
colnames(never2)[2]<-"region"
never2$region<-tolower(never2$region)
never2<-never2[-c(3,5:12)]
choro.never2 <- merge(states, never2, sort = FALSE, by = "region")
choro.never2 <- choro.never2[order(choro.never2$order), ]
ggplot(choro.never2,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=22,name = "Percentage of \n for all gender \n who were former smokers")+
  geom_point(aes(x = long3, y = lat1, size = SFM), data = new.d[which(new.d$Year == 1997),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SFF), data = new.d[which(new.d$Year == 1997),], alpha = .5,color = "white")+
  scale_size(limits = c(10,35),name = "Percentage of \n male/female \n who were former smokers")+
  ggtitle("Percentage of former smokers among all people in 1997")

#1998
states <- map_data("state")
never3 <- gg.status.f.all[which(gg.status.f.all$YEAR==1998),]
names(never3) <- tolower(names(never3))
colnames(never3)[2]<-"region"
never3$region<-tolower(never3$region)
never3<-never3[-c(3,5:12)]
choro.never3 <- merge(states, never3, sort = FALSE, by = "region")
choro.never3 <- choro.never3[order(choro.never3$order), ]
ggplot(choro.never3,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=22,name = "Percentage of \n for all gender \n who were former smokers")+
  geom_point(aes(x = long3, y = lat1, size = SFM), data = new.d[which(new.d$Year == 1998),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SFF), data = new.d[which(new.d$Year == 1998),], alpha = .5,color = "white")+
  scale_size(limits = c(10,35),name = "Percentage of \n male/female \n who were former smokers")+
  ggtitle("Percentage of former smokers among all people in 1998")

#1999
states <- map_data("state")
never4 <- gg.status.f.all[which(gg.status.f.all$YEAR==1999),]
names(never4) <- tolower(names(never4))
colnames(never4)[2]<-"region"
never4$region<-tolower(never4$region)
never4<-never4[-c(3,5:12)]
choro.never4 <- merge(states, never4, sort = FALSE, by = "region")
choro.never4 <- choro.never4[order(choro.never4$order), ]
ggplot(choro.never4,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=22,name = "Percentage of \n for all gender \n who were former smokers")+
  geom_point(aes(x = long3, y = lat1, size = SFM), data = new.d[which(new.d$Year == 1999),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SFF), data = new.d[which(new.d$Year == 1999),], alpha = .5,color = "white")+
  scale_size(limits = c(10,35),name = "Percentage of \n male/female \n who were former smokers")+
  ggtitle("Percentage of former smokers among all people in 1999")

#2000
states <- map_data("state")
never5 <- gg.status.f.all[which(gg.status.f.all$YEAR==2000),]
names(never5) <- tolower(names(never5))
colnames(never5)[2]<-"region"
never5$region<-tolower(never5$region)
never5<-never5[-c(3,5:12)]
choro.never5 <- merge(states, never5, sort = FALSE, by = "region")
choro.never5 <- choro.never5[order(choro.never5$order), ]
ggplot(choro.never5,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=22,name = "Percentage of \n for all gender \n who were former smokers")+
  geom_point(aes(x = long3, y = lat1, size = SFM), data = new.d[which(new.d$Year == 2000),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SFF), data = new.d[which(new.d$Year == 2000),], alpha = .5,color = "white")+
  scale_size(limits = c(10,35),name = "Percentage of \n male/female \n who were former smokers")+
  ggtitle("Percentage of former smokers among all people in 2000")

#2001
states <- map_data("state")
never6 <- gg.status.f.all[which(gg.status.f.all$YEAR==2001),]
names(never6) <- tolower(names(never6))
colnames(never6)[2]<-"region"
never6$region<-tolower(never6$region)
never6<-never6[-c(3,5:12)]
choro.eve6 <- merge(states, never6, sort = FALSE, by = "region")
choro.eve6 <- choro.eve6[order(choro.eve6$order), ]
ggplot(choro.eve6,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=22,name = "Percentage of \n for all gender \n who were former smokers")+
  geom_point(aes(x = long3, y = lat1, size = SFM), data = new.d[which(new.d$Year == 2001),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SFF), data = new.d[which(new.d$Year == 2001),], alpha = .5,color = "white")+
  scale_size(limits = c(10,35),name = "Percentage of \n male/female \n who were former smokers")+
  ggtitle("Percentage of former smokers among all people in 2001")

#2002
states <- map_data("state")
never8 <- gg.status.f.all[which(gg.status.f.all$YEAR==2002),]
names(never8) <- tolower(names(never8))
colnames(never8)[2]<-"region"
never8$region<-tolower(never8$region)
never8<-never8[-c(3,5:12)]
choro.never8 <- merge(states, never8, sort = FALSE, by = "region")
choro.never8 <- choro.never8[order(choro.never8$order), ]
ggplot(choro.never8,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=22,name = "Percentage of \n for all gender \n who were former smokers")+
  geom_point(aes(x = long3, y = lat1, size = SFM), data = new.d[which(new.d$Year == 2002),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SFF), data = new.d[which(new.d$Year == 2002),], alpha = .5,color = "white")+
  scale_size(limits = c(10,35),name = "Percentage of \n male/female \n who were former smokers")+
  ggtitle("Percentage of former smokers among all people in 2002")

#2003
states <- map_data("state")
never9 <- gg.status.f.all[which(gg.status.f.all$YEAR==2003),]
names(never9) <- tolower(names(never9))
colnames(never9)[2]<-"region"
never9$region<-tolower(never9$region)
never9<-never9[-c(3,5:12)]
choro.never9 <- merge(states, never9, sort = FALSE, by = "region")
choro.never9 <- choro.never9[order(choro.never9$order), ]
ggplot(choro.never9,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=22,name = "Percentage of \n for all gender \n who were former smokers")+
  geom_point(aes(x = long3, y = lat1, size = SFM), data = new.d[which(new.d$Year == 2003),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SFF), data = new.d[which(new.d$Year == 2003),], alpha = .5,color = "white")+
  scale_size(limits = c(10,35),name = "Percentage of \n male/female \n who were former smokers")+
  ggtitle("Percentage of former smokers among all people in 2003")

# 2004
states <- map_data("state")
never10 <- gg.status.f.all[which(gg.status.f.all$YEAR==2004),]
names(never10) <- tolower(names(never10))
colnames(never10)[2]<-"region"
never10$region<-tolower(never10$region)
never10<-never10[-c(3,5:12)]
choro.never10 <- merge(states, never10, sort = FALSE, by = "region")
choro.never10 <- choro.never10[order(choro.never10$order), ]
ggplot(choro.never10,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=22,name = "Percentage of \n for all gender \n who were former smokers")+
  geom_point(aes(x = long3, y = lat1, size = SFM), data = new.d[which(new.d$Year == 2004),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SFF), data = new.d[which(new.d$Year == 2004),], alpha = .5,color = "white")+
  scale_size(limits = c(10,35),name = "Percentage of \n male/female \n who were former smokers")+
  ggtitle("Percentage of former smokers among all people in 2004")

# 2005
states <- map_data("state")
never11 <- gg.status.f.all[which(gg.status.f.all$YEAR==2005),]
names(never11) <- tolower(names(never11))
colnames(never11)[2]<-"region"
never11$region<-tolower(never11$region)
never11<-never11[-c(3,5:12)]
choro.never11 <- merge(states, never11, sort = FALSE, by = "region")
choro.never11 <- choro.never11[order(choro.never11$order), ]
ggplot(choro.never11,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=22,name = "Percentage of \n for all gender \n who were former smokers")+
  geom_point(aes(x = long3, y = lat1, size = SFM), data = new.d[which(new.d$Year == 2005),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SFF), data = new.d[which(new.d$Year == 2005),], alpha = .5,color = "white")+
  scale_size(limits = c(10,35),name = "Percentage of \n male/female \n who were former smokers")+
  ggtitle("Percentage of former smokers among all people in 2005")

# 2006
states <- map_data("state")
never12 <- gg.status.f.all[which(gg.status.f.all$YEAR==2006),]
names(never12) <- tolower(names(never12))
colnames(never12)[2]<-"region"
never12$region<-tolower(never12$region)
never12<-never12[-c(3,5:12)]
choro.never12 <- merge(states, never12, sort = FALSE, by = "region")
choro.never12 <- choro.never12[order(choro.never12$order), ]
ggplot(choro.never12,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=22,name = "Percentage of \n for all gender \n who were former smokers")+
  geom_point(aes(x = long3, y = lat1, size = SFM), data = new.d[which(new.d$Year == 2006),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SFF), data = new.d[which(new.d$Year == 2006),], alpha = .5,color = "white")+
  scale_size(limits = c(10,35),name = "Percentage of \n male/female \n who were former smokers")+
  ggtitle("Percentage of former smokers among all people in 2006")

# 2007
states <- map_data("state")
never13 <- gg.status.f.all[which(gg.status.f.all$YEAR==2007),]
names(never13) <- tolower(names(never13))
colnames(never13)[2]<-"region"
never13$region<-tolower(never13$region)
never13<-never13[-c(3,5:12)]
choro.never13 <- merge(states, never13, sort = FALSE, by = "region")
choro.never13 <- choro.never13[order(choro.never13$order), ]
ggplot(choro.never13,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=22,name = "Percentage of \n for all gender \n who were former smokers")+
  geom_point(aes(x = long3, y = lat1, size = SFM), data = new.d[which(new.d$Year == 2007),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SFF), data = new.d[which(new.d$Year == 2007),], alpha = .5,color = "white")+
  scale_size(limits = c(10,35),name = "Percentage of \n male/female \n who were former smokers")+
  ggtitle("Percentage of former smokers among all people in 2007")

# 2008
states <- map_data("state")
never14 <- gg.status.f.all[which(gg.status.f.all$YEAR==2008),]
names(never14) <- tolower(names(never14))
colnames(never14)[2]<-"region"
never14$region<-tolower(never14$region)
never14<-never14[-c(3,5:12)]
choro.never14 <- merge(states, never14, sort = FALSE, by = "region")
choro.never14 <- choro.never14[order(choro.never14$order), ]
ggplot(choro.never14,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=22,name = "Percentage of \n for all gender \n who were former smokers")+
  geom_point(aes(x = long3, y = lat1, size = SFM), data = new.d[which(new.d$Year == 2008),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SFF), data = new.d[which(new.d$Year == 2008),], alpha = .5,color = "white")+
  scale_size(limits = c(10,35),name = "Percentage of \n male/female \n who were former smokers")+
  ggtitle("Percentage of former smokers among all people in 2008")

# 2009
states <- map_data("state")
never15 <- gg.status.f.all[which(gg.status.f.all$YEAR==2009),]
names(never15) <- tolower(names(never15))
colnames(never15)[2]<-"region"
never15$region<-tolower(never15$region)
never15<-never15[-c(3,5:12)]
choro.never15 <- merge(states, never15, sort = FALSE, by = "region")
choro.never15 <- choro.never15[order(choro.never15$order), ]
ggplot(choro.never15,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=22,name = "Percentage of \n for all gender \n who were former smokers")+
  geom_point(aes(x = long3, y = lat1, size = SFM), data = new.d[which(new.d$Year == 2009),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SFF), data = new.d[which(new.d$Year == 2009),], alpha = .5,color = "white")+
  scale_size(limits = c(10,35),name = "Percentage of \n male/female \n who were former smokers")+
  ggtitle("Percentage of former smokers among all people in 2009")

# 2010
states <- map_data("state")
never16 <- gg.status.f.all[which(gg.status.f.all$YEAR==2010),]
names(never16) <- tolower(names(never16))
colnames(never16)[2]<-"region"
never16$region<-tolower(never16$region)
never16<-never16[-c(3,5:12)]
choro.never16 <- merge(states, never16, sort = FALSE, by = "region")
choro.never16 <- choro.never16[order(choro.never16$order), ]
ggplot(choro.never16,aes(x=long, y=lat)) +
  geom_polygon(aes(group = group, fill = data_value),color = "black") +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5,xlim = c(-130,-70),ylim = c(25,50))+scale_fill_gradient2(low="blue",mid="green",high="red",midpoint=22,name = "Percentage of \n for all gender \n who were former smokers")+
  geom_point(aes(x = long3, y = lat1, size = SFM), data = new.d[which(new.d$Year == 2010),], alpha = .5,color = "black")+
  geom_point(aes(x = long3, y = lat3, size = SFF), data = new.d[which(new.d$Year == 2010),], alpha = .5,color = "white")+
  scale_size(limits = c(10,35),name = "Percentage of \n male/female \n who were former smokers")+
  ggtitle("Percentage of former smokers among all people in 2010")

