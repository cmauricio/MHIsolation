##### general guidelines #####
# informed consent s_1 to s_2
# Sociodemographics s_3 to s_8
# Government question s_9 to s_18
# Days isolating yourself s_19
# Facilities closed s_20 & s_172
# Work allows to work from home s_22
# How many people are you isolated with s_23
# People you are isolated with s_24
# Government has implemented methods s_25
#
# How dangerous you think Covid is s_27 to s_33
# Sensitive questions s_34 to s_39
# Health condition s_40
# Psychological condition s_41
# Psychological questions s_42 to s_71
# 
# Stress scale s_72 to s_81
# Depression scale s_113 to s_115 & s_185 to s_190
# Anxiety scale s_133 to to s_142

library(readr)
library(psych)
library(likert)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gmodels) #for Crosstables
library(rcompanion) #for Cramers V

##### Score transformations #####
# for scoring PHQ-9

PHQ9score<-function(score) {
  answer<-""
  if (score<5) {
    answer<-"None"
  } else if (score>=5 & score<10) {
    answer<-"Mild"
  } else if (score>=10 & score<15) {
    answer <-"Moderate"
  } else if (score>=15 & score< 20) {
    answer <- "Moderately severe"
  } else if (score>=20) {
    answer <- "Severe"
  } else if (is.na(score)){
    answer <- NA
  }
  return(answer)
}


# for scoring PSS-10

PSS10score<-function(age,score) {
  answer<-""
  if (age>=18 & age<=29) {
    if (score>=8 & score<=20.4) {
      answer<-"Normal"
    } else if (score<8) {
      answer<-"Low stress"
    } else if (score>20.4) {
      answer<-"High stress"
    } 
  } else if (age>=30 & age<=44) {
    if (score>=6.8 & score<=19.2) {
      answer<-"Normal"
    } else if (score<6.8) {
      answer<-"Low stress"
    } else if (score>19.2) {
      answer<-"High stress"
    } 
  } else if (age>=45 & age<=54) {
    if (score>=6.5 & score<=18.7) {
      answer<-"Normal"
    } else if (score<6.5) {
      answer<-"Low stress"
    } else if (score>18.7) {
      answer<-"High stress"
    }
  } else if (age>=55 & age<=64) {
    if (score>=5 & score<=18.8) {
      answer<-"Normal"
    } else if (score<5) {
      answer<-"Low stress"
    } else if (score>18.8) {
      answer<-"High stress"
    }
  } else if (age>=65) {
    if (score>=5.7 & score<=18.3) {
      answer<-"Normal"
    } else if (score<5.7) {
      answer<-"Low stress"
    } else if (score>18.3) {
      answer<-"High stress"
    } else if (is.na(score)){
      answer <- NA
    }
  }
  return(answer)
}

AgeInters<-function(age) {
  inters<-""
  if (age>=18 & age<25) {
    inters<-"18-24"
  } else if (age>=25 & age<30) {
    inters<-"25-29"
  } else if (age>=30 & age<35) {
    inters<-"30-34" 
  } else if (age>=35) {
    inters<-"35 or more"
  } 
  return(inters)
}

##### Loading the pre-processed data #####
dataurl<-"https://raw.githubusercontent.com/cmauricio/MHIsolation/master/Data/cleandata.csv" #Getting Dataset from github
cleandata<-read.csv2(url(dataurl), header=T, na.strings=c(""," ", NA), stringsAsFactors = F)
##### analysis of socio demographics #####

# subsetting the data from the tidy database
SocDemData<-select(cleandata, "Gender"=s_3, "Age"=s_4, "Country"=s_5, 
                   "Education"=s_6, "Employment"=s_7, "EmploymentCOVID"=s_8, "SurveyLang"=lang)

# replacing values for categories
GenderCat<-c('Male', 
             'Female', 
             'Other', 
             'Dont wish to tell')
SocDemData$Gender<-as.factor(SocDemData$Gender)
levels(SocDemData$Gender)<-GenderCat


EduCat<-c('Primary school',
          'Secondary school',
          'Highschool',
          'Technical studies or similar',
          'Undergraduate',
          'Postgraduate or higher',
          'None',
          'Do not wish to tell')
SocDemData$Education<-as.factor(SocDemData$Education)
levels(SocDemData$Education)<-EduCat

SocDemData$Age<-as.integer(SocDemData$Age) #to ensure the data has the gategories they should have

SocDemData$Country<-as.factor(SocDemData$Country) #to ensure the data has the gategories they should have

LangCat<-c('English',
           'Spanish',
           'Danish',
           'Italian',
           'German',
           'Thai',
           'Hungarian',
           'French')
SocDemData$SurveyLang<-as.factor(SocDemData$SurveyLang)
levels(SocDemData$SurveyLang)<-LangCat

YesNoCat<-c("Yes","No")
SocDemData$Employment<-as.factor(SocDemData$Employment)
levels(SocDemData$Employment)<-YesNoCat

EmployCOVIDCat<- c("Yes","No", "Into some extent", "It does not apply")
SocDemData$EmploymentCOVID<-as.factor(SocDemData$EmploymentCOVID)
levels(SocDemData$EmploymentCOVID)<-EmployCOVIDCat

# Frequency analysis #

#Gender split
GenderSplit<-count(SocDemData, Gender)

#Education split
EducationSplit<-count(SocDemData, Education)

#Age average

SocDemData<-filter(SocDemData,Age<100)

AgeAvg<-mean(SocDemData$Age)
AgeSD<-sd(SocDemData$Age)

#Country split
CountrySplit<-count(SocDemData,Country)

#Percentage according to country // n is the number of occurences, some packages name it as freq instead
CountrySplit$perc<-CountrySplit$n/sum(CountrySplit$n)*100


#Language split
LangSplit=count(SocDemData, SurveyLang)

#Percentage according to Language
LangSplit$perc<-LangSplit$n/sum(LangSplit$n)*100


#Employment split
EmploySplit=count(SocDemData, Employment)


#Employment affected by covid distribution
EmployCOVIDSplit=count(SocDemData, EmploymentCOVID)


##### Days isolating yourself s_19 #####

IsolationDays<-select(cleandata, "Days_in_isolation"=s_19)

# We chose 100 as a cut as by the time the survey took place the amount of isolation days
# from the first country taking measures was less than 100. So data over 100 was considered invalid 
IsolationDays<-subset(IsolationDays,Days_in_isolation<100)


# Isolation days average
IsoDaysAv<-mean(as.integer(IsolationDays$Days_in_isolation))


##### Health condition s_40 #####

HealthCondition<-select(cleandata, s_40)
names(HealthCondition)[1]<-"Health_risk_group"

HealthCondition$Health_risk_group<-as.factor(HealthCondition$Health_risk_group)
levels(HealthCondition$Health_risk_group)<-YesNoCat


##### Psychological condition s_41 #####

MentalHealthCondition<-select(cleandata, s_41)
names(MentalHealthCondition)[1]<-"Mental_Health_group"

MentalHealthCondition$Mental_Health_group<-as.factor(MentalHealthCondition$Mental_Health_group)
levels(MentalHealthCondition$Mental_Health_group)<-YesNoCat


##### Psychological questions s_42 to s_71 #####

MentalHealthQData<-select(cleandata, "Sad"=s_42, "Stressed"=s_43, "Angry"=s_44, "Happy"=s_45, 
                          "Calm"=s_46, "Bored"=s_47, "Entertained"=s_48, "Physically_ill"=s_49, 
                          "Mentally_ill"=s_50, "Lonely"=s_51, "Confused"=s_52, "Anguish"=s_53, 
                          "Restless"=s_54, "Cannot_form_thoughts"=s_55, "Foggy_mind"=s_56, 
                          "Communication_troubles"=s_57, "Sleep_more"=s_58, "Sleep_less"=s_59, 
                          "Bad_memory"=s_60, "Tired"=s_61, "Have_no_time"=s_62, "Have_more_time"=s_63,
                          "Time_for_family"=s_64, "Time_for_me"=s_65, "Carrying_burden"=s_66, 
                          "Worried"=s_67, "Guilty"=s_68, "Hopeless"=s_69, "Stressful_cohabitants"=s_70, 
                          "Good_cohabitants"=s_71)

# Reeplacing the "it doesn't apply" abswer for NAs so they are not counted in the data
MentalHealthQData<-MentalHealthQData%>%dplyr::na_if(7)

MentalHealthQData<-na.omit(MentalHealthQData)


# Replacing values for categories
Likert7_agreeNA<-c('Strongly disagree',
                   'Disagree',
                   'Slightly disagree',
                   'Slightly agree',
                   'Agree',
                   'Strongly agree')

# Assignation of variables 
for (Y in 1:ncol(MentalHealthQData)){
  MentalHealthQData[,Y]<-as.factor(MentalHealthQData[,Y])
  levels(MentalHealthQData[,Y])<-Likert7_agreeNA
}


PsyLikertGraph<-likert(MentalHealthQData)
likert.bar.plot(PsyLikertGraph)

summary(MentalHealthQData=="It does not apply")



##### Depression scale s_113 to s_190 PHQ-9 

# subsetting the data from the tidy database
PHQ9<-select(cleandata, "Item_1"=s_113, "Item_2"=s_114, "Item_3"=s_115, "Item_4"=s_185, 
             "Item_5"=s_184, "Item_6"=s_116, "Item_7"=s_187, "Item_8"=s_188, "Item_9"=s_190)


PHQ9<-na.omit(PHQ9)

# Standarising the scores according to the scale 
for (y in 1:ncol(PHQ9)) {
  PHQ9[,y]<-PHQ9[,y]-1
}

# Transforming the scale into integers  
for (w in 1:ncol(PHQ9)){
  PHQ9[,w]<-as.integer(PHQ9[,w])
}

# Summing rows to get the total score per participant
PHQ9$Score<-rowSums(PHQ9[1:9])
PHQ9$Evaluation<-""

# Running our custom function to evaluate the scores of the test
for (i in 1:nrow(PHQ9)) {
  PHQ9[i,11]<-PHQ9score(PHQ9[i,10])
} 

PHQC<-count(PHQ9, Evaluation)
PHQPlot<-ggplot(PHQC, aes(reorder(Evaluation, n),n))+
  coord_flip()+
  geom_col(aes(fill=n))+
  labs(Evaluation="Evaluation", y="Frequency")+
  theme_minimal()
PHQPlot



### This part is to work with factors and other types of analyses

# replacing values for categories
Likert4_days<-c('Not at all',
                'Several days',
                'More than half of the days',
                'Nearly every day')

for (z in 1:9){
  PHQ9[,z]<-as.factor(PHQ9[,z])
  factor(PHQ9[,z],
         levels=Likert4_days,
         ordered=T)
}

PHQ9$Evaluation<-as.factor(PHQ9$Evaluation)


PHQ9LikertGraph<-likert(PHQ9[,1:9])
likert.bar.plot(PHQ9LikertGraph)+
  theme_minimal()+
  theme(legend.position='bottom')





##### Stress scale s_133 to to s_142 PSS-10 #####

PSS10<-select(cleandata, "Item_1"=s_133, "Item_2"=s_134, "Item_3"=s_135, "Item_4"=s_136, 
              "Item_5"=s_137, "Item_6"=s_138, "Item_7"=s_139, "Item_8"=s_140, 
              "Item_9"=s_141, "Item_10"=s_142, "Age"=s_4)

PSS10<-na.omit(PSS10)

# Standarising the scores according to the scale 
for (y in 1:10) {
  PSS10[,y]<-PSS10[,y]-1
}

# Transforming the scale into integers  
for (w in 1:10){
  PSS10[,w]<-as.integer(PSS10[,w])
}

# Inverting items 4, 5, 7, and 8
for (j in 1:10) {
  for (it in c(4,5,7,8)) {
    if (PSS10[j,it] == 0) {
      PSS10[j,it]<-4
    } else if (PSS10[j,it]==1) {
      PSS10[j,it]<-3
    } else if (PSS10[j,it]==3) {
      PSS10[j,it]<-1
    } else if (PSS10[j,it]==4) {
      PSS10[j,it]<-0
    }
  }
}

# Summing rows to get the total score per participant
PSS10$Score<-rowSums(PSS10[1:10])
PSS10$Evaluation<-""

# Running our custom function to evaluate the scores of the test
for (i in 1:nrow(PSS10)) {
  PSS10[i,13]<-PSS10score(PSS10[i,11], PSS10[i,12])
} 

PSSC<-count(PSS10, Evaluation)
PSSPlot<-ggplot(PSSC, aes(reorder(Evaluation, n),n))+
  coord_flip()+
  geom_col(aes(fill=n))+
  labs(Evaluation="Evaluation", y="Frequency")+
  theme_minimal()
PSSPlot


### This part is to work with factors and other types of analyses

# replacing values for categories
Likert5_never<-c('Never',
                 'Almost never',
                 'Sometimes',
                 'Often',
                 'Very often')

for (z in 1:10){
  PSS10[,z]<-as.factor(PSS10[,z])
  factor(PSS10[,z],
         levels=Likert5_never,
         ordered=T)
}


PSS10$Evaluation<-as.factor(PSS10$Evaluation)

PSS10LikertGraph<-likert(PSS10[,1:10])
likert.bar.plot(PSS10LikertGraph)+
  theme_minimal()+
  theme(legend.position='bottom')






##### Follow-up analyses #####


##### Factor Analysis was carried in SPSS



##### Chi-squared Analyses #####


# Working with the PSS data

ChiData<-select(cleandata, 'Gender'=s_3, 'Age'=s_4, 'Education'=s_6, 'Employment'=s_7, 
                'Afraid_losing_job'=s_74, 'Afraid_finantial_lose'=s_75, 'Afraid_no_income'=s_76, 'Health_diagnosis'=s_40, 'Mental_diagnosis'=s_41,
                'phq1'=s_113, 'phq2'=s_114, 'phq3'=s_115, 'phq4'=s_185, 'phq5'=s_184, 'phq6'=s_116, 'phq7'=s_187, 'phq8'=s_188, 'phq9'=s_190,
                'pss1'=s_133, 'pss2'=s_134, 'pss3'=s_135, 'pss4'=s_136, 'pss5'=s_137, 'pss6'=s_138, 'pss7'=s_139, 'pss8'=s_140, 'pss9'=s_141, 'pss10'=s_142)

ChiData<-na.omit(ChiData)

# replacing values for categories
GenderCat<-c('Male', 
             'Female', 
             'Other', 
             'Dont wish to tell')
ChiData$Gender<-as.factor(ChiData$Gender)
levels(ChiData$Gender)<-GenderCat

ChiData$Age<-as.integer(ChiData$Age) #to ensure the data has the gategories they should have

EduCat<-c('Primary school',
          'Secondary school',
          'Highschool',
          'Technical studies or similar',
          'Undergraduate',
          'Postgraduate or higher',
          'None',
          'Do not wish to tell')
ChiData$Education<-as.factor(ChiData$Education)
levels(ChiData$Education)<-EduCat


YesNoCat<-c("Yes","No")
ChiData$Employment<-as.factor(ChiData$Employment)
levels(ChiData$Employment)<-YesNoCat
ChiData$Health_diagnosis<-as.factor(ChiData$Health_diagnosis)
levels(ChiData$Health_diagnosis)<-YesNoCat
ChiData$Mental_diagnosis<-as.factor(ChiData$Mental_diagnosis)
levels(ChiData$Mental_diagnosis)<-YesNoCat

Likert6_agree<-c('Strongly disagree',
                 'Disagree',
                 'Slightly disagree',
                 'Slightly agree',
                 'Agree',
                 'Strongly agree')
ChiData$Afraid_losing_job<-as.factor(ChiData$Afraid_losing_job)
levels(ChiData$Afraid_losing_job)<-Likert6_agree
ChiData$Afraid_finantial_lose<-as.factor(ChiData$Afraid_finantial_lose)
levels(ChiData$Afraid_finantial_lose)<-Likert6_agree
ChiData$Afraid_no_income<-as.factor(ChiData$Afraid_no_income)
levels(ChiData$Afraid_no_income)<-Likert6_agree


# Transforming the scale into integers  
for (w in 10:18){
  ChiData[,w]<-as.integer(ChiData[,w])
}

# Summing rows to get the total score per participant
ChiData$PHQScore<-rowSums(ChiData[10:18])
ChiData$PHQEvaluation<-""

# Running our custom function to evaluate the scores of the test
for (i in 1:nrow(ChiData)) {
  ChiData[i,30]<-PHQ9score(ChiData[i,29])
} 

ChiData$PHQEvaluation<-factor(ChiData$PHQEvaluation, levels=c('None', 'Mild', 'Moderate', 'Moderately severe', 'Severe'))

### This part is to work with factors and other types of analyses

# Standarising the scores according to the scale 
for (y in 19:28) {
  ChiData[,y]<-ChiData[,y]-1
}

# Transforming the scale into integers  
for (w in 19:28){
  ChiData[,w]<-as.integer(ChiData[,w])
}

# Inverting items 4, 5, 7, and 8
for (j in 19:28) {
  for (it in c(20,21,23,24)) {
    if (ChiData[j,it] == 0) {
      ChiData[j,it]<-4
    } else if (ChiData[j,it]==1) {
      ChiData[j,it]<-3
    } else if (ChiData[j,it]==3) {
      ChiData[j,it]<-1
    } else if (ChiData[j,it]==4) {
      ChiData[j,it]<-0
    }
  }
}

# Summing rows to get the total score per participant
for (w in 19:28){
  ChiData[,w]<-as.integer(ChiData[,w])
}

ChiData$PSSScore<-rowSums(ChiData[19:28])
ChiData$PSSEvaluation<-""

# Running our custom function to evaluate the scores of the test
for (i in 1:nrow(ChiData)) {
  ChiData[i,32]<-PSS10score(ChiData[i,2], ChiData[i,31])
} 

ChiData$PSSEvaluation<-factor(ChiData$PSSEvaluation, levels=c('Low stress', 'Normal', 'High stress'))



a<-lapply(ChiData$Age,AgeInters)
a<-unlist(a)
ChiData<-mutate(ChiData,AgeInterv=a)

ChiData<-na.omit(ChiData)



chisq.test(ChiData$PHQEvaluation, ChiData$Gender)
cramerV(ChiData$PHQEvaluation, ChiData$Gender)
chisq.test(ChiData$PHQEvaluation, ChiData$AgeInterv)
cramerV(ChiData$PHQEvaluation, ChiData$AgeInterv)
chisq.test(ChiData$PHQEvaluation, ChiData$Education)
cramerV(ChiData$PHQEvaluation, ChiData$Education)
chisq.test(ChiData$PHQEvaluation, ChiData$Employment)
cramerV(ChiData$PHQEvaluation, ChiData$Employment)
chisq.test(ChiData$PHQEvaluation, ChiData$Afraid_losing_job)
cramerV(ChiData$PHQEvaluation, ChiData$Afraid_losing_job)
chisq.test(ChiData$PHQEvaluation, ChiData$Afraid_finantial_lose)
cramerV(ChiData$PHQEvaluation, ChiData$Afraid_finantial_lose)
chisq.test(ChiData$PHQEvaluation, ChiData$Afraid_no_income)
cramerV(ChiData$PHQEvaluation, ChiData$Afraid_no_income)
chisq.test(ChiData$PHQEvaluation, ChiData$Health_diagnosis)
cramerV(ChiData$PHQEvaluation, ChiData$Health_diagnosis)
chisq.test(ChiData$PHQEvaluation, ChiData$Mental_diagnosis)
cramerV(ChiData$PHQEvaluation, ChiData$Mental_diagnosis)



chisq.test(ChiData$PSSEvaluation, ChiData$Gender)
cramerV(ChiData$PSSEvaluation, ChiData$Gender)
chisq.test(ChiData$PSSEvaluation, ChiData$AgeInterv)
cramerV(ChiData$PSSEvaluation, ChiData$AgeInterv)
chisq.test(ChiData$PSSEvaluation, ChiData$Education)
cramerV(ChiData$PSSEvaluation, ChiData$Education)
chisq.test(ChiData$PSSEvaluation, ChiData$Employment)
cramerV(ChiData$PSSEvaluation, ChiData$Employment)
chisq.test(ChiData$PSSEvaluation, ChiData$Afraid_losing_job)
cramerV(ChiData$PSSEvaluation, ChiData$Afraid_losing_job)
chisq.test(ChiData$PSSEvaluation, ChiData$Afraid_finantial_lose)
cramerV(ChiData$PSSEvaluation, ChiData$Afraid_finantial_lose)
chisq.test(ChiData$PSSEvaluation, ChiData$Afraid_no_income)
cramerV(ChiData$PSSEvaluation, ChiData$Afraid_no_income)
chisq.test(ChiData$PSSEvaluation, ChiData$Health_diagnosis)
cramerV(ChiData$PSSEvaluation, ChiData$Health_diagnosis)
chisq.test(ChiData$PSSEvaluation, ChiData$Mental_diagnosis)
cramerV(ChiData$PSSEvaluation, ChiData$Mental_diagnosis)



CrossTable(x=ChiData$PHQEvaluation, y=ChiData$Gender, fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")
CrossTable(x=ChiData$PHQEvaluation, y=ChiData$AgeInterv , fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")
CrossTable(x=ChiData$PHQEvaluation, y=ChiData$Education, max.width = 1, fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")
CrossTable(x=ChiData$PHQEvaluation, y=ChiData$Employment , fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")
CrossTable(x=ChiData$PHQEvaluation, y=ChiData$Afraid_losing_job , fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")
CrossTable(x=ChiData$PHQEvaluation, y=ChiData$Afraid_finantial_lose , fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")
CrossTable(x=ChiData$PHQEvaluation, y=ChiData$Afraid_no_income , fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")
CrossTable(x=ChiData$PHQEvaluation, y=ChiData$Health_diagnosis , fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")
CrossTable(x=ChiData$PHQEvaluation, y=ChiData$Mental_diagnosis , fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")

CrossTable(x=ChiData$PSSEvaluation, y=ChiData$Gender , fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")
CrossTable(x=ChiData$PSSEvaluation, y=ChiData$AgeInterv , fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")
CrossTable(x=ChiData$PSSEvaluation, y=ChiData$Education , max.width = 1, fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")
CrossTable(x=ChiData$PSSEvaluation, y=ChiData$Employment , fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")
CrossTable(x=ChiData$PSSEvaluation, y=ChiData$Afraid_losing_job , fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")
CrossTable(x=ChiData$PSSEvaluation, y=ChiData$Afraid_finantial_lose , fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")
CrossTable(x=ChiData$PSSEvaluation, y=ChiData$Afraid_no_income , fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")
CrossTable(x=ChiData$PSSEvaluation, y=ChiData$Health_diagnosis, fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")
CrossTable(x=ChiData$PSSEvaluation, y=ChiData$Mental_diagnosis , fisher=T, chisq=T, expected=T, sresid=T, format="SPSS")




