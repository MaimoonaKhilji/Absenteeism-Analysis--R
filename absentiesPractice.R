library("tidyverse")
library("modeest")
library("psych")
data = read.csv("Absenteeism_data case study.csv")
head(data)

describe(data)

#there is no missing data
any(is.na(data))

#maximum transport expense of employee
m=max(data$Transportation.Expense)
print(c("maximum transport expense :  ",m), quote = FALSE)

print("list of employee who pay maximum transport expense")
data[data$Transportation.Expense==m,]

#list of those employess whose absenties are maximum
data[data$Absenteeism.Time.in.Hours== max(data$Absenteeism.Time.in.Hours),]

cor(data$Age,data$Body.Mass.Index)
cor(data$Distance.to.Work,data$Transportation.Expense)
cor(data$Daily.Work.Load.Average,data$Body.Mass.Index)
cor(data$Pets,data$Children)
cor(data$Pets,data$Education)
cor(data$Education,data$Children)
cor(data$Education,data$Daily.Work.Load.Average)
cor(data$Transportation.Expense,data$Daily.Work.Load.Average)


ggplot(data,aes(x=Pets))+geom_bar()


ggplot() +aes(x = data$Pets, fill = "Pets") + geom_bar()
ggplot() + aes(x = data$Children, fill = "Children") + geom_bar()

# converting variables to factors

col <- c(1:12)
absenteeism_at_work_factored <- data
absenteeism_at_work_factored[col] <- lapply(absenteeism_at_work_factored[col], 
                                            factor)

# converting codes to meaningful information

absenteeism_at_work_factored <- absenteeism_at_work_factored %>%
  mutate(Education = fct_recode(Education,highschool="1",graduate="2",
                                postgraduate="3",master.And.doctrate="4"))

absenteeism_at_work_factored <- absenteeism_at_work_factored %>%
  mutate(Reason.for.Absence = fct_recode(Reason.for.Absence,`Infectious, 
                                         parasitic diseases`="0", `Neoplasms`="1",
                                         `Diseases of the blood`="2",
                                         `Endocrine and metabolic diseases`="3",
                                         `Mental and behavioural disorders`="4",
                                         `Diseases of the nervous system`="5",
                                         `Diseases of the eye and adnexa`="6",
                                         `Diseases of the ear and mastoid process`="7",
                                         `Diseases of the circulatory system`="8",
                                         `Diseases of the respiratory system`="9",
                                         `Diseases of the digestive system`="10", 
                                         `Diseases of the skin and subcutaneous tissue`="11",
                                         `Diseases of the musculoskeletal system and connective tissue`="12",
                                         `Diseases of the genitourinary system`="13",
                                         `Pregnancy, childbirth and the puerperium`="14",
                                         `Certain conditions originating in the perinatal`="15", 
                                         `Congenital malformations, deformations and chromosomal abnormalities`= "16",
                                         `Symptoms, signs and abnormal clinical  findings`="17", 
                                         `Injury, poisoning and certain other consequences of external causes`= "18",
                                         `causes of morbidity and mortality`="19", 
                                         `Factors influencing health status and contact with health services`="21",
                                         `patient follow-up`="22",
                                         `medical consultation`="23",
                                         `blood donation`="24", 
                                         `laboratory examination`="25", 
                                         `unjustified absence`="26",
                                         `physiotherapy`="27", 
                                         `dental consultation`="28"))

absenteeism_at_work_factored <- absenteeism_at_work_factored %>%
  mutate(Reason.for.Absence = fct_recode(Reason.for.Absence,`Infectious, parasitic diseases`="0", `Neoplasms`="1",`Diseases of the blood`="2",`Endocrine and metabolic diseases`="3",`Mental and behavioural disorders`="4",`Diseases of the nervous system`="5",`Diseases of the eye and adnexa`="6",`Diseases of the ear and mastoid process`="7",`Diseases of the circulatory system`="8",`Diseases of the respiratory system`="9",`Diseases of the digestive system`="10", `Diseases of the skin and subcutaneous tissue`="11",`Diseases of the musculoskeletal system and connective tissue`="12", `Diseases of the genitourinary system`="13",`Pregnancy, childbirth and the puerperium`="14",`Certain conditions originating in the perinatal`="15",  `Congenital malformations, deformations and chromosomal abnormalities`= "16",`Symptoms, signs and abnormal clinical  findings`="17", `Injury, poisoning and certain other consequences of external causes`= "18",`causes of morbidity and mortality`="19", `Factors influencing health status and contact with health services`="21",`patient follow-up`="22",`medical consultation`="23",`blood donation`="24", `laboratory examination`="25", `unjustified absence`="26", `physiotherapy`="27", `dental consultation`="28"))


absent <- as.data.frame(absenteeism_at_work_factored %>% dplyr::select(everything()) %>% dplyr::filter(as.numeric(Absenteeism.Time.in.Hours) > 0))
Reason <-  as.data.frame(absent %>% group_by(Reason.for.Absence) %>% dplyr::summarise(count= n(), percent = round(count*100/nrow(absent),1))%>% arrange(desc(count)))

Reason %>%
  ggplot() + 
  aes(x = reorder(Reason.for.Absence,percent), 
      y= percent, fill= Reason.for.Absence) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  theme(legend.position='none') +  
  geom_text(aes(label = percent), vjust = 0.5, hjust = 1.1) + 
  xlab('Reason for absence')


