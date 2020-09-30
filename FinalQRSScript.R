library(plyr)
library(dplyr)
library(tidyr)
library(fastDummies)

#Stay Schedule
StaySchedule <- read.csv(file = "C:/Users/crsru/Documents/DFM Project/Stay Schedule 2019 Q2.csv", stringsAsFactors = FALSE, header = TRUE, na.strings = "")
ShelterCareFilter <- c("Shelter Care")
StayScheduleUpd <- filter(StaySchedule, Check.Out.Type %in% ShelterCareFilter)

book.date <- as.POSIXct(StayScheduleUpd$Actual.Start, format = "%m/%d/%Y %H:%M")
release.date <- as.POSIXct(StayScheduleUpd$Actual.End, format = "%m/%d/%Y %H:%M")
booking.difference <- difftime(release.date, book.date, units = c("mins"))
StayScheduleUpd$booking.filter <- ifelse(booking.difference < 5, 1, 0)

#Overcount on duplicated inmates
stay.schedule.filtered <- subset(StayScheduleUpd, booking.filter == 0)

ethnicity2 <- stay.schedule.filtered$Race.Ethnicity..JDAI.Reporting...05.30.18.13.49.
other <- is.na(ethnicity2)
white <- gsub("Caucasian", "White", ethnicity2, ignore.case = TRUE)
black <- gsub("African American", "Black", ethnicity2, ignore.case = TRUE)
amindian <- gsub("Native American", "American Indian", ethnicity2, ignore.case = TRUE)
hispanic <- gsub("Hispanic or Latino", "Hispanic", ethnicity2, ignore.case = TRUE)

stay.schedule.filtered$ethnicity.filter <- ifelse(is.na(ethnicity2), "Other", 
                                                  ifelse(ethnicity2 == "Caucasian", white,
                                                         ifelse(ethnicity2 == "African American", black, 
                                                                ifelse(ethnicity2 == "Native American", amindian,
                                                                       ifelse(ethnicity2 == "Asian", "Asian",
                                                                              ifelse(ethnicity2 == "Hispanic", "Hispanic", "Other"))))))
quarter.start <- as.POSIXct("04/01/2019 00:01", format = "%m/%d/%Y %H:%M")
quarter.end <- as.POSIXct("06/30/2019 23:59", format = "%m/%d/%Y %H:%M")
actual.start <- as.POSIXct(stay.schedule.filtered$Actual.Start, format = "%m/%d/%Y %H:%M") 
actual.end <-  as.POSIXct(stay.schedule.filtered$Actual.End, format = "%m/%d/%Y %H:%M")

stay.schedule.filtered$start <- ifelse(quarter.start > actual.start, "Start",
                                       ifelse(quarter.end > actual.start & actual.start > quarter.start, "Entry", "Other"))
stay.schedule.filtered$released <- ifelse(quarter.end > actual.end, "Exit", "End")

stay.race.start.count <- plyr::count(stay.schedule.filtered, c("ethnicity.filter", "start"))
stay.race.release.count <- plyr::count(stay.schedule.filtered, c("ethnicity.filter", "released"))

write.csv(stay.race.start.count, file = "StayScheduleStart_Race.csv")
write.csv(stay.race.release.count, file = "StayScehduleRelease_Race.csv")
-------------------------------------------------------------------------------------------
#Read Alternatives

Alternatives <- read.csv(file = "C:/Users/crsru_000/Desktop/DFM Project/Alternatives 2019 Q2.csv", stringsAsFactors = FALSE, header = TRUE, na.strings = "")
CountyFilter3 <- c("Minnehaha")
alternatives.filtered <- filter(Alternatives, County. %in% CountyFilter3)    

quarter.start.alternative <- as.POSIXct("04/01/2019", format = "%m/%d/%Y")
quarter.end.alternative <- as.POSIXct("06/30/2019", format = "%m/%d/%Y")
alternative.start <- as.POSIXct(alternatives.filtered$Start.Date., format = "%m/%d/%Y")
alternative.end <- as.POSIXct(alternatives.filtered$End.Date., format = "%m/%d/%Y")

alternatives.filtered$start <- ifelse(quarter.start.alternative > alternative.start, "Start", "Entry")
alternatives.filtered$released <- ifelse(is.na(alternative.end),"Held", "Exit")

ethnicity <- alternatives.filtered$Race.Ethnicity.

white1 <- gsub("Caucasian", "White", alternatives.filtered$Race.Ethnicity., ignore.case = TRUE)
black1 <- gsub("African American", "Black", alternatives.filtered$Race.Ethnicity., ignore.case = TRUE)
amindian1 <- gsub("Native American", "American Indian", alternatives.filtered$Race.Ethnicity., ignore.case = TRUE)
hispanic1 <- gsub("Hispanic or Latino", "Hispanic", alternatives.filtered$Race.Ethnicity., ignore.case = TRUE)

alternatives.filtered$ethnicity.filter <- ifelse(ethnicity == "Caucasian", white1,
                                                 ifelse(ethnicity == "African American", black1, 
                                                        ifelse(ethnicity == "Native American", amindian1,
                                                               ifelse(ethnicity == "Asian", "Asian",
                                                                      ifelse(ethnicity == "Hispanic", "Hispanic", 
                                                                             ifelse(ethnicity == "Hispanic or Latino", hispanic1,
                                                                                    ifelse(ethnicity == is.na(ethnicity), blank, "Other")))))))
alternatives.filtered$HD.EM.ERC <- ifelse(alternatives.filtered$EM. == "Yes" & alternatives.filtered$EM.ERC. == "Yes", "Yes", "No")
alternatives.filtered$actual.program <- ifelse(alternatives.filtered$HD.EM.ERC == "Yes", "HD/EM and ERC",
                                               ifelse(alternatives.filtered$EM. == "Yes", "HD/EM", "ERC"))
race.start <- plyr::count(alternatives.filtered, c("actual.program", "ethnicity.filter", "start"))
alt.program.race.start <- tidyr::spread(race.start, "start", "freq")
race.release <- plyr::count(alternatives.filtered, c("actual.program", "ethnicity.filter", "released"))
alt.program.race.release <- tidyr::spread(race.release, "released", "freq")
sex.start <-plyr::count(alternatives.filtered, c("actual.program", "Sex","start"))
alt.program.sex.start <- tidyr::spread(sex.start, "start", "freq")
sex.release <- plyr::count(alternatives.filtered, c("actual.program", "Sex","released"))
alt.program.sex.release <-tidyr::spread(sex.release, "released", "freq")

#fta.exits <- subset(alternatives.filtered, FTA.Exits. == 1)
reoffense.exits <- subset(alternatives.filtered, Reoffense.Exits. == 1)
tech.exits <- subset(alternatives.filtered, TechViolations.NonComp.Exits.== 1)
succesful.exits <- subset(alternatives.filtered, Successful.Exits. == 1)

#sex.fta.exit<-plyr::count(fta.exits, c("actual.program", "Sex","FTA.Exits.")) 
sex.reoffense.exit<-plyr::count(reoffense.exits, c("actual.program", "Sex")) 
sex.tech.exit<-plyr::count(tech.exits, c("actual.program", "Sex")) 
sex.success.exit<-plyr::count(succesful.exits, c("actual.program", "Sex")) 

#fta.sex <- spread(sex.fta.exit, "Sex", "freq")
reoffense.sex <- spread(sex.reoffense.exit, "Sex", "freq")
tech.sex <- spread(sex.tech.exit, "Sex", "freq")
success.sex <- spread(sex.success.exit, "Sex", "freq")

sex.exit.1 <- merge(tech.sex, success.sex, by = "actual.program", all = TRUE)

alt.program.sex.exit <- merge(reoffense.sex, sex.exit.1, by = "actual.program", all = TRUE)
colnames(alt.program.sex.exit) <-c("actual.program", "Reoffense (F)", "Reoffense (M)",
                                   "Tech (F)", "Tech (M)", "Successful (F)", "Successful (M)")

#race.fta.exit <- plyr::count(fta.exits, c("actual.program", "ethnicity.filter"))
race.reoffense.exit <- plyr::count(reoffense.exits, c("actual.program", "ethnicity.filter"))
race.tech.exit <- plyr::count(tech.exits, c("actual.program", "ethnicity.filter"))
race.success.exit <- plyr::count(succesful.exits, c("actual.program", "ethnicity.filter"))

#fta.race <- tidyr::spread(race.fta.exit, "actual.program", "freq")
reoffense.race <- tidyr::spread(race.reoffense.exit, "actual.program", "freq")
tech.race <- tidyr::spread(race.tech.exit, "actual.program", "freq")
success.race <- tidyr::spread(race.success.exit, "actual.program", "freq")

race.exit.1 <- merge(tech.race, success.race, by = "ethnicity.filter", all = TRUE)

alt.program.race.exit <- merge(reoffense.race, race.exit.1, by = "ethnicity.filter", all = TRUE)
colnames(alt.program.race.exit) <- c("ethnicity.filter", "Reoffense (ERC)", "Reoffense (EM)", "Reoffense (ERC/EM)",
                                     "Tech (ERC)", "Tech (EM)", "Tech (EM/ERC)",
                                     "Successful (ERC)", "Successful (EM)")

write.csv(alt.program.sex.start, file = "AlternativesSex_Start.csv")
write.csv(alt.program.sex.release, file = "AlternativesSex_Release.csv")
write.csv(alt.program.race.start, file = "AlternativesRace_Start.csv")
write.csv(alt.program.race.release, file = "AlternativesRace_Release.csv")
write.csv(alt.program.sex.exit, file = "AlternativesSex_ExitType.csv")
write.csv(alt.program.race.exit, file = "AlternativesRace_ExitType.csv")
----------------------------------------------------------------------------------
#Detention_Population Tab
admin.rel <- read.csv(file = "c:/Users/crsru_000/Desktop/DFM Project/AdmissionReleases2019Q2.csv", stringsAsFactors = FALSE, header=TRUE, na.strings = "")
CountyFilter2 <- c("Minnehaha County")
admission.release <- filter(admin.rel, Initial.Held.For %in% CountyFilter2)

#Convert Race/Ethnicity into One Column for 
EthRace <- admission.release$Race.Ethnicity
white2 <- gsub("Caucasian", "White", admission.release$Race.Ethnicity, ignore.case = TRUE)
black2 <- gsub("African American", "Black", admission.release$Race.Ethnicity, ignore.case = TRUE)
amindian2 <- gsub("Native American", "American Indian", admission.release$Race.Ethnicity, ignore.case = TRUE)
admission.release$ActualEthnicity <- ifelse(EthRace == "Caucasian", white2,
                                            ifelse(EthRace == "African American", black2, 
                                                   ifelse(EthRace == "Native American", amindian2,
                                                          ifelse(EthRace == "Asian", "Asian",
                                                                 ifelse(EthRace == "Hispanic", "Hispanic",
                                                                        ifelse(EthRace == is.na(EthRace), "NA", "Other"))))))
#Convert Warrants to Court Order
CourtOrder <- gsub("Warrant", "Court Order", admission.release$Most.Serious.Offense.Category)
admission.release$MostSeriousOffenseUpd <- ifelse(admission.release$Most.Serious.Offense.Category == "Warrant", CourtOrder, 
                                                  ifelse(admission.release$Primary.Hold.Reason == "Mental Health Hold", "*Mental Health Hold", adminRelUpd$Most.Serious.Offense.Category))
#create variables for booking date, quarter start and end, releases, held  
bookdate <- as.POSIXct(admission.release$Booking.Date.Time, format = "%m/%d/%Y %H:%M")
releases1 <- as.POSIXct(admission.release$Release.Date, format = "%m/%d/%Y %H:%M")

#Defines Each Quarter start and end
quarter.start <- as.POSIXct("04/01/2019 00:01", format = "%m/%d/%Y %H:%M")
quarter.end <- as.POSIXct("06/30/2019 23:59", format = "%m/%d/%Y %H:%M")
admission.release$QuarterHold <- ifelse(quarter.start >= bookdate, "Before", 
                                        ifelse(quarter.end >= bookdate & bookdate >= quarter.start, "Admitted", "Other"))
admission.release$QuarterReleases <- ifelse(is.na(releases1), "Held", "Released") 

#Subset for Quarter Start
admission.release$QuarterStart <- ifelse(admission.release$QuarterHold == "Before", 1, 0)
quarter.start.data <- subset(admission.release, QuarterStart == 1)

quarter.start.sex <- plyr::count(quarter.start.data, "Sex") 
quarter.start.race <- plyr::count(quarter.start.data, "ActualEthnicity")
quarter.start.offense <- plyr::count(quarter.start.data, "MostSeriousOffenseUpd")  

#Subset for Quarter Admissions
quarter.admission.data <- subset(admission.release, QuarterHold == "Admitted")

quarter.admission.sex <- plyr::count(quarter.admission.data, "Sex")
quarter.admission.race <- plyr::count(quarter.admission.data, "ActualEthnicity")
quarter.admission.offense <-plyr::count(quarter.admission.data, "MostSeriousOffenseUpd")  

#Subset for Quarter Releases
quarter.release.data <- subset(admission.release, QuarterReleases == "Released")

quarter.release.sex <- plyr::count(quarter.release.data, "Sex")
quarter.release.race <- plyr::count(quarter.release.data, "ActualEthnicity")
quarter.release.offense <- plyr::count(quarter.release.data, "MostSeriousOffenseUpd")

#Subset for Quarter End
quarter.end.data <- subset(admission.release, QuarterReleases == "Held")

quarter.end.sex <- plyr::count(quarter.end.data,"Sex")
quarter.end.race <- plyr::count(quarter.end.data, "ActualEthnicity")
quarter.end.offense <- plyr::count(quarter.end.data, "MostSeriousOffenseUpd")

#Count Data
detention.sex.count1 <- merge(quarter.start.sex, quarter.admission.sex, by = "Sex", all = TRUE) 
detention.sex.count2 <- merge(quarter.release.sex, quarter.end.sex, by = "Sex", all = TRUE)
detention.sex.count <- merge(detention.sex.count1, detention.sex.count2, by = "Sex", all = TRUE)
colnames(detention.sex.count) <-c("Sex", "Start", "Admission", "Release", "End")

detention.race.count1 <- merge(quarter.start.race, quarter.admission.race, by = "ActualEthnicity", all = TRUE) 
detention.race.count2 <- merge(quarter.release.race, quarter.end.race, by = "ActualEthnicity", all = TRUE) 
detention.race.count <- merge(detention.race.count1, detention.race.count2, by = "ActualEthnicity", all = TRUE)
colnames(detention.race.count) <- c("Race", "Start", "Admission", "Release", "End")

detention.offense.count1 <- merge(quarter.start.offense, quarter.admission.offense, by = "MostSeriousOffenseUpd", all = TRUE)
detention.offense.count2 <- merge(quarter.release.offense, quarter.end.offense, by = "MostSeriousOffenseUpd", all = TRUE)
detention.offense.count <- merge(detention.admission.count1, detention.admission.count2, by = "x", all = TRUE)
colnames(detention.offense.count) <- c("Offense", "Start", "Admission", "Release", "End")

write.csv(detention.sex.count, file = "QuarterlyDetentionSexcount.csv")
write.csv(detention.race.count, file = "QuarterlyDetentionRacecount.csv")
write.csv(detention.offense.count, file = "QuarterlyDetentionOffensecount.csv")

---------------------------------------------------------------------------------------------
#Referrals Screened
referrals.screened <- read.csv(file = "C:/Users/crsru_000/Desktop/DFM Project/RAI Indicated-Actual Q2.csv", stringsAsFactors = FALSE, header=TRUE)
RefFilter <- c("Minnehaha Juvenile Detention Center")
ref.screened.filter <- filter(referrals.screened, IntakeCntr %in% RefFilter)

ref.screened.upd <- dummy_cols(ref.screened.filter,select_columns = c("Race"),split = ",")
ref.screened.upd$AMINDprelim <- rowSums(ref.screened.upd[paste0("Race_",c("Indian"))])
ref.screened.upd$ASIANprelim <- rowSums(ref.screened.upd[paste0("Race_",c("Asian","Hawaiian"))])
ref.screened.upd$BLACKprelim <- rowSums(ref.screened.upd[paste0("Race_",c("Black"))])
ref.screened.upd$HISPprelim <- rowSums(ref.screened.upd[paste0("Race_",c("Hispanic"))])
ref.screened.upd$WHITEprelim <- rowSums(ref.screened.upd[paste0("Race_",c("White"))])
ref.screened.upd$OTHERprelim <- rowSums(ref.screened.upd[paste0("Race_",c("Unavailable","Other","Multiracial"))])

ref.screened.upd$Racefinal <- ifelse((23 + max.col(ref.screened.upd[,24:29],"first") == 25),"American Indian",
                                     ifelse((23 + max.col(ref.screened.upd[,24:29],"first") == 26),"Asian",
                                            ifelse((23 + max.col(ref.screened.upd[,24:29],"first") == 27),"Black",
                                                   ifelse((23 + max.col(ref.screened.upd[,24:29],"first") == 28),"Hispanic",
                                                          ifelse((23 + max.col(ref.screened.upd[,24:29],"first") == 29),"White","Other")))))

referral.race.count <- plyr::count(ref.screened.upd, c("IndicatedDecision", "ActualDecision", "Racefinal"))
referral.sex.count <- plyr::count(ref.screened.upd, c("IndicatedDecision", "ActualDecision", "Gender"))

ref.sex.count <- tidyr::spread(referral.sex.count, "ActualDecision", "freq")
ref.race.count <- tidyr::spread(referral.race.count, "ActualDecision", "freq")

write.csv(ref.sex.count, file = "ReferralsScreened_Sex.csv")
write.csv(ref.race.count, file = "ReferralsScreened_Race.csv")
----------------------------------------------------------------------------------
#RAI Overrides Script
rai.overrides<-read.csv(file = "C:/Users/crsru_000/Desktop/DFM Project/RAI Overrides Q2.csv", stringsAsFactors = FALSE, header = TRUE, na.strings = "")

rai.overrides<-dummy_cols(rai.overrides,select_columns = c("Race"),split = ",")
rai.overrides$AMINDprelim <- rowSums(rai.overrides[paste0("Race_",c("Indian"))])
rai.overrides$ASIANprelim <- rowSums(rai.overrides[paste0("Race_",c("Asian","Hawaiian"))])
rai.overrides$BLACKprelim <- rowSums(rai.overrides[paste0("Race_",c("Black"))])
rai.overrides$HISPprelim <- rowSums(rai.overrides[paste0("Race_",c("Hispanic"))])
rai.overrides$WHITEprelim <- rowSums(rai.overrides[paste0("Race_",c("White"))])
rai.overrides$OTHERprelim <- rowSums(rai.overrides[paste0("Race_",c("Unavailable","Other","Multiracial"))])

rai.overrides$Racefinal <- ifelse((17 + max.col(rai.overrides[,18:23],"first") == 19),"American Indian",
                                  ifelse((17 + max.col(rai.overrides[,18:23],"first") == 20),"Asian",
                                         ifelse((17 + max.col(rai.overrides[,18:23],"first") == 21),"Black",
                                                ifelse((17 + max.col(rai.overrides[,18:23],"first") == 22),"Hispanic",
                                                       ifelse((17 + max.col(rai.overrides[,18:23],"first") == 23),"White","Other")))))

rai.overrides$Comment <- ifelse(rai.overrides$OverrideUpReason == "NA", 1, 0)

overrides.race.count<-plyr::count(rai.overrides, c("OverrideOutcome","Racefinal"))                                                     
OR.count.race <- tidyr::spread(overrides.race.count,"OverrideOutcome","freq")

overrides.sex.count <-plyr::count(rai.overrides, c("OverrideOutcome","Gender"))
OR.count.sex <-tidyr::spread(overrides.sex.count, "Gender", "freq")

override.reason.count <- plyr:: count(rai.overrides, c("OverrideUpReason", "Comment"))
OR.count.reason <- tidyr::spread(override.reason.count,"Comment", "freq")

write.csv(OR.count.sex, file = "OverrideCount_Sex.csv")
write.csv(OR.count.race, file = "OverrideCount_Race.csv")
write.csv(OR.count.reason, file = "OverrideCount_Reason.csv")

