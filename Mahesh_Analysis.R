library(tidyverse)
library(gt)
library(htmltools)
library(kableExtra)
library(gridExtra)

# Reading in Data ---------------------------------------------------------
z.2019.AIS <- read.csv("data/2019/PUF_AISDIAGNOSIS.csv")
z.2020.AIS <- read.csv("data/2020/PUF_AISDIAGNOSIS.csv")
z.2021.AIS <- read.csv("data/2021/PUF_AISDIAGNOSIS.csv")
z.AIS <- bind_rows(z.2019.AIS, z.2020.AIS)
z.AIS <- bind_rows(z.AIS, z.2021.AIS)
rm(z.2021.AIS, z.2020.AIS, z.2019.AIS)

z.2019.trauma <- read.csv("data/2019/PUF_TRAUMA.csv")
z.2020.trauma <- read.csv("data/2020/PUF_TRAUMA.csv")
z.2021.trauma <- read.csv("data/2021/PUF_TRAUMA.csv")
z.trauma <- bind_rows(z.2019.trauma, z.2020.trauma)
z.2021.trauma$TEACHINGSTATUS <- as.character(z.2021.trauma$TEACHINGSTATUS)
z.trauma <- bind_rows(z.trauma, z.2021.trauma)
rm(z.2021.trauma, z.2020.trauma, z.2019.trauma)

z.2019.hospitalevents <- read.csv("data/2019/PUF_HOSPITALEVENTS.csv")
z.2020.hospitalevents <- read.csv("data/2020/PUF_HOSPITALEVENTS.csv")
z.2021.hospitalevents <- read.csv("data/2021/PUF_HOSPITALEVENTS.csv")
z.hospitalevents <- bind_rows(z.2020.hospitalevents, z.2019.hospitalevents)
z.hospitalevents <- bind_rows(z.hospitalevents, z.2020.hospitalevents)
z.hospitalevents <- bind_rows(z.hospitalevents, z.2021.hospitalevents)
rm(z.2021.hospitalevents, z.2020.hospitalevents, z.2019.hospitalevents)

z.2019.preexisting <- read.csv("data/2019/PUF_PREEXISTINGCONDITIONS.csv")
z.2020.preexisting <- read.csv("data/2020/PUF_PREEXISTINGCONDITIONS.csv")
z.2021.preexisting <- read.csv("data/2021/PUF_PREEXISTINGCONDITIONS.csv")
z.preexisting <- bind_rows(z.2019.preexisting, z.2020.preexisting)
z.preexisting <- bind_rows(z.preexisting, z.2021.preexisting)
rm(z.2021.preexisting, z.2020.preexisting, z.2019.preexisting)

z.2019.ICD <- read.csv("data/2019/PUF_ICDDIAGNOSIS.csv")
z.2020.ICD <- read.csv("data/2020/PUF_ICDDIAGNOSIS.csv")
z.2021.ICD <- read.csv("data/2021/PUF_ICDDIAGNOSIS.csv")
z.ICD <- bind_rows(z.2019.ICD, z.2020.ICD)
z.ICD <- bind_rows(z.ICD, z.2021.ICD)
rm(z.2021.ICD, z.2020.ICD, z.2019.ICD)

z.2019.ICDProc <- read.csv("data/2019/PUF_ICDPROCEDURE.csv")
z.2020.ICDProc <- read.csv("data/2020/PUF_ICDPROCEDURE.csv")
z.2021.ICDProc <- read.csv("data/2021/PUF_ICDPROCEDURE.csv")
z.ICDProc <- bind_rows(z.2019.ICDProc, z.2020.ICDProc)
z.ICDProc <- bind_rows(z.ICDProc, z.2021.ICDProc)
rm(z.2021.ICDProc, z.2020.ICDProc, z.2019.ICDProc)

z.lookup <- read.csv("data/PUF_TRAUMA_LOOKUP.csv")
z.AISlookup <- read.csv("data/PUF_AISDIAGNOSIS_LOOKUP.csv")
z.ICDlookup <- read.csv("data/PUF_ICDDIAGNOSIS_LOOKUP.csv")
z.ICDProclookup <- read.csv("data/PUF_ICDPROCEDURE_LOOKUP.csv")


# Selecting Data ----------------------------------------------------------

# 1. Filter for Epidural Hematomas by AIS diagnosis
epiduralcodes <- df.AISlookup %>%
  filter(str_detect(AISDESCRIPTION, 'epidural')) %>%
  filter(!str_detect(AISDESCRIPTION, 'spinal'))

epiduralonly <- df.AIS %>%
  filter(AISPreDot %in% epiduralcodes$AISPREDOT)


# 2. Selecting for inclusion / exclusion criteria 

trauma <- df.trauma %>%
  filter(inc_key %in% epiduralonly$inc_key) %>%
  filter(TOTALGCS < 8) %>%
  select(inc_key, AgeYears, SEX, AMERICANINDIAN, ASIAN, BLACK, PACIFICISLANDER, RACEOTHER, WHITE, RACE_NA, RACE_UK, ETHNICITY, WORKRELATED, EMSGCSEYE, EMSGCSMOTOR, EMSGCSVERBAL, EMSTOTALGCS, EMSDispatchHrs, EMSDepartureHrs, EMSARRIVALHRS, EDDISCHARGEHRS,
         PRIMARYMETHODPAYMENT, GCSEYE, GCSVERBAL, GCSMOTOR,TOTALGCS, TOTALICULOS, TOTALVENTDAYS, FINALDISCHARGEHRS, HOSPITALARRIVALHRS, HOSPITALDISCHARGEHRS, TBIHIGHESTTOTALGCS,
         TBIGCSMOTOR, TBIPUPILLARYRESPONSE, TBIMIDLINESHIFT, WITHDRAWALLSTHRS, ISS, HOSPITALTYPE, Bedsize, HOSPDISCHARGEDISPOSITION) %>%
  pivot_longer(cols = AMERICANINDIAN:RACE_UK, names_to = "race", values_to = "value") %>%
  filter(value == 1) %>%
  select(-value)


# 3. Finding all epidural procedures specific to our patient population
# TODO - add craniotomies
# TODO - See if I can select for more patients here by not excluding everything

epiduralprocedures <- df.ICDProc %>%
  filter(Inc_Key %in% trauma$inc_key)

epiduralprocedures <- merge(epiduralprocedures, df.ICDProclookup, by = "ICDPROCEDURECODE")

epiduralprocedures <- epiduralprocedures %>% filter(ICDProcedureCode_Desc %in% 
                                                      c("Drainage of Intracranial Epidural Space with Drainage Device, Open Approach",
                                                        "Drainage of Intracranial Epidural Space, Open Approach"))

filter(str_detect(ICDProcedureCode_Desc, 'Drainage of Intracranial Epidural')) %>%
  select(Inc_Key, HOSPITALPROCEDURESTARTHRS, ICDPROCEDURECODE, ICDProcedureCode_Desc)

epiduralprocedures <- epiduralprocedures %>%
  group_by(Inc_Key) %>%
  filter(n() == 1) %>%
  ungroup()

# Combining the procedure data with the inclusion/exclusion data into one dataframe
final_df <-
  merge(trauma, epiduralprocedures, by.x = "inc_key", by.y = "Inc_Key") %>%
  filter(inc_key != "210060159183")



# 5. Finding the Pre-Existing Conditions for a specific patient population

df.preexistingvariables <- df.preexisting %>%
  filter(Inc_Key %in% final_df$inc_key)
df.preexistingvariables <- df.preexistingvariables[,1:3]
df.preexistingvariables <- drop_na(df.preexistingvariables)
df.preexistingvariables <- df.preexistingvariables %>%
  filter(PREEXISTINGCONDITIONANSWER == 1)

preexistinglookup <- df.lookup %>%
  filter(FmtName == 'PreExistingCondition')
preexistinglookup <- preexistinglookup %>%
  select(Start, Label)

df.preexistingvariables <- merge(df.preexistingvariables, preexistinglookup, by.x = 'PREEXISTINGCONDITION', by.y = 'Start') %>%
  select(Inc_Key, Label)

# Pivot the data
df.preexistingvariables <- df.preexistingvariables %>%
  mutate(Value = 1) %>%
  pivot_wider(names_from = Label, values_from = Value, values_fill = list(Value = 0)) %>%
  column_to_rownames(var="Inc_Key")

df.preexistingvariables <- tibble::rownames_to_column(df.preexistingvariables, "inc_key")

# Combining the prexisting data + epidural procedures + inclusion/exclusion critera into one database
final_df <- merge(df.preexistingvariables, final_df, all.y = TRUE)





# Zoning in Further s s
final_df <- final_df %>%
  select(-AgeYears)

final_df$TwoHourCutoff <- ifelse(final_df$HOSPITALPROCEDURESTARTHRS < 2.0, 1, 0)

final_df$Black <- ifelse(final_df$race == "BLACK", 1, 0)
final_df$White <- ifelse(final_df$race == "WHITE", 1, 0)
final_df$RaceOther <- ifelse(final_df$race == "RACEOTHER", 1, 0)



a.final_df <- na.omit(final_df)


final_df <- final_df %>%
  select(SEX:TwoHourCutoff)




write.csv(final_df, "df_10-16.csv")

generalmodel <- glm(TwoHourCutoff ~ + `Black` + `White` + `RaceOther`, family=binomial(link='logit'), data = a.final_df)

summary(generalmodel)










# 5. Creating tables 

by.race <- filtered_epiduralprocedures %>%
  filter(race != "RACE_NA" & race != "RACE_UK" & race != "RACEOTHER") %>%
  group_by(race) %>%
  summarise("Average Procedure Start Time (Hrs)" = mean(HOSPITALPROCEDURESTARTHRS, na.rm = TRUE))
colnames(by.race)[1] <- "Race"

race_mapping <- c("AMERICANINDIAN" = "American Indian", "ASIAN" = "Asian", "BLACK" = "Black", 
                     "WHITE" = "White")
by.race$Race<- race_mapping[as.character(by.race$Race)]
by.race$`Average Procedure Start Time (Hrs)` <- round(by.race$`Average Procedure Start Time (Hrs)`, 2)


filtered_epiduralprocedures <- filtered_epiduralprocedures[!is.na(filtered_epiduralprocedures$PRIMARYMETHODPAYMENT), ]
by.paymentmethod <- filtered_epiduralprocedures %>%
  group_by(PRIMARYMETHODPAYMENT) %>%
  summarise("Average Procedure Start Time (Hrs)" = mean(HOSPITALPROCEDURESTARTHRS, na.rm = TRUE))
colnames(by.paymentmethod)[1] <- "Payment Method"
payment_mapping <- c("1" = "Medicaid", "2" = "Not Billed", "3" = "Self-Pay", 
                     "4" = "Private/Commerical Insurance", "6" = "Medicare", "7" = "Other Government", "10" = "Other")
by.paymentmethod$`Payment Method` <- payment_mapping[as.character(by.paymentmethod$`Payment Method`)]
by.paymentmethod


filtered_epiduralprocedures <- filtered_epiduralprocedures[!is.na(filtered_epiduralprocedures$ETHNICITY), ]
by.hispanic <- filtered_epiduralprocedures %>%
  group_by(ETHNICITY) %>%
  summarise("Average Procedure Start Time (Hrs)" = mean(HOSPITALPROCEDURESTARTHRS, na.rm = TRUE))
colnames(by.hispanic)[1] <- "Ethnicity"
ethnicity_mapping <- c("1" = "Hispanic", "2" = "Not Hispanic")
by.hispanic$Ethnicity <- ethnicity_mapping[as.character(by.hispanic$Ethnicity)]
by.hispanic


filtered_epiduralprocedures <- filtered_epiduralprocedures[!is.na(filtered_epiduralprocedures$SEX), ]
by.sex <- filtered_epiduralprocedures %>%
  group_by(SEX) %>%
  summarise("Average Procedure Start Time (Hrs)" = mean(HOSPITALPROCEDURESTARTHRS, na.rm = TRUE))
colnames(by.sex)[1] <- "Sex"
sex_mapping <- c("1" = "Male", "2" = "Female", "3" = "Non-binary")
by.sex$Sex <- sex_mapping[as.character(by.sex$Sex)]
by.sex
by.sex$`Average Procedure Start Time (Hrs)` <- round(by.sex$`Average Procedure Start Time (Hrs)`, 2)






# Make table for race, ethnicity, sex, and primary payment method

table_race <- by.race %>% 
  kbl(caption = "Start Time Across Race") %>%
  kable_classic(full_width = F, html_font = "Cambria")


table_sex <- by.sex %>% 
  kbl(caption = "Start Time Across Sex") %>%
  kable_classic(full_width = F, html_font = "Cambria")




combined_html <- tagList(
  div(style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 20px;",
      div(HTML(as_raw_html(table_race))),
      div(HTML(as_raw_html(table_sex)))
  )
)

# Display in RStudio Viewer
browsable(combined_html)


# Save this code for later
mu <- filtered_epiduralprocedures %>%
  filter(race != "RACE_NA" & race != "RACE_UK" & race != "RACEOTHER") %>%
  group_by(race, ICDProcedureCode_Desc) %>%
  summarise( = mean(HOSPITALPROCEDURESTARTHRS, na.rm = TRUE)) %>%
  pivot_wider(names_from = ICDProcedureCode_Desc, values_from = grp.mean)


mu

test.black <- filtered_epiduralprocedures %>%
  filter(race == "BLACK")
test.black <- table(test.black$ICDProcedureCode_Desc) / nrow(test.black)

test.white <- filtered_epiduralprocedures %>%
  filter(race == "WHITE")
test.white <- table(test.white$ICDProcedureCode_Desc) / nrow(test.white)

test.asian <- filtered_epiduralprocedures %>%
  filter(race == "ASIAN")
test.asian <- table(test.asian$ICDProcedureCode_Desc) / nrow(test.asian)


# Controlling for the following variables



final_df <- merge(epiduralprocedures, trauma, by.x = "Inc_Key", by.y = "inc_key")
         
write.csv(final_df, "timetodrainagesurgery.csv")


# Controlling for prexisiting conditions

preexistinglookup <- df.lookup %>%
  filter(FmtName == "PreExistingCondition")

preexistinglookup <- preexistinglookup %>%
  filter(Label %in% c("Bleeding Disorder", "Pregnancy", "Peripheral Arterial Diseas",
                      "Anticoagulant Therapy", "Dementia", "Hypertension", "History of Peripheral Vascualr Disease",
                      "Cerebrovascular Accident"))

preexistingconditions <- df.combined.preexisting %>%
  filter(PREEXISTINGCONDITION %in% preexistinglookup$Start) %>%
  filter(Inc_Key %in% neuroonly$inc_key) %>%
  select(Inc_Key, PREEXISTINGCONDITION, PREEXISTINGCONDITIONANSWER) %>%
  merge(preexistinglookup, by.x = "PREEXISTINGCONDITION", by.y = "Start") %>%
  select(Inc_Key, Label, PREEXISTINGCONDITIONANSWER)


preexistingconditions <- preexistingconditions %>%
  pivot_wider(names_from = Label, values_from = PREEXISTINGCONDITIONANSWER) %>%
  na.omit()


preexistingconditions[preexistingconditions == 2] <- 0
