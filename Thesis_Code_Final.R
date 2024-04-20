library(tidyverse)
library(readxl)
library(gt)
library(rsample)
library(glue)
library(gtsummary)
library(modelsummary)
library(lubridate)
library(profvis)
library(purrr)
library(sjlabelled)
library(kableExtra)
library(httr)
library(jsonlite)
library(sf)
library(tigris)
library(geosphere)
library(AER)
library(reshape2)
library(webshot)

inmate_release_offenses_cps_1 <- read_excel("data/Inmate_Release.xlsx", "Inmate Release Offenses CPS_1")
inmate_active_offenses_cps <- read_excel("data/FDOC_Database_Active.XLSX", "Inmate Active Offenses CPS")
inmate_release_root <- read_excel("data/Inmate_Release.xlsx", "Inmate Release Root")
inmate_active_root <- read_excel("data/FDOC_Database_Active.XLSX", "Inmate Active Root")
inmate_release_incarhist <- read_excel("data/Inmate_Release.xlsx", "Inmate Release Incarhist")
inmate_active_incarhist <- read_excel("data/FDOC_Database_Active.XLSX", "Inmate Active Incarhist")

visits_2012 <- read_csv("visitation records/2012.csv")
visits_2013 <- read_csv("visitation records/2013.csv")
visits_2014 <- read_csv("visitation records/2014.csv")
visits_2015 <- read_csv("visitation records/2015.csv")
visits_2016 <- read_csv("visitation records/2016.csv")
visits_2017 <- read_csv("visitation records/2017.csv")
visits_2018 <- read_csv("visitation records/2018.csv")
visits_2019 <- read_csv("visitation records/2019.csv")


##Remove offenses for which the literature finds extremely high recidivism
filtered_release_data <- inmate_release_offenses_cps_1 |>
  filter(!grepl("Murder|Manslaughter|Sex|Rape|Molest|SXOFDR|Sex Batt|Sex Transf|Sex.Bat| 
                Obscene|Sex Misc|Sex Organs|SX Trade|Sex Trafficking|Prostitution|
                Sex Photo|Porn|Sex Pred|Impreg|Sx|HIV-Positive Has Sex|Voyeur",
                adjudicationcharge_descr, ignore.case = TRUE))

summary(filtered_release_data)


##include release date and additional demographic variables
filtered_release_data <- left_join(filtered_release_data, inmate_release_root, by = "DCNumber")
filtered_release_data <- filtered_release_data |>
  mutate(
    PrisonTermInMonths = (as.numeric(substr(prisonterm, 1, 3)) * 12) +  
      as.numeric(substr(prisonterm, 4, 5)) +            
      as.numeric(substr(prisonterm, 6, 7)) / 30          
  )
filtered_release_data <- filtered_release_data |>
  mutate(
    BirthDate = as.Date(BirthDate),
    DateAdjudicated = as.Date(DateAdjudicated),
    Age_At_Adjudication = as.numeric(difftime(DateAdjudicated, BirthDate, units = "days")) / 365.25
  ) |>
  filter(!is.na(Age_At_Adjudication))


##create column for how many visits each person received
visits_2012 <- visits_2012 |>
  group_by(InmateNo) |>
  mutate(VisitCount_2012 = n())

visits_2013 <- visits_2013 |>
  group_by(InmateNo) |>
  mutate(VisitCount_2013 = n())

visits_2014 <- visits_2014 |>
  group_by(InmateNo) |>
  mutate(VisitCount_2014 = n())

visits_2015 <- visits_2015 |>
  group_by(InmateNo) |>
  mutate(VisitCount_2015 = n())

visits_2016 <- visits_2016 |>
  group_by(InmateNo) |>
  mutate(VisitCount_2016 = n())

visits_2017 <- visits_2017 |>
  group_by(InmateNo) |>
  mutate(VisitCount_2017 = n())

visits_2018 <- visits_2018 |>
  group_by(InmateNo) |>
  mutate(VisitCount_2018 = n())

visits_2019 <- visits_2019 |>
  group_by(InmateNo) |>
  mutate(VisitCount_2019 = n())

##separate visit count and inmate no (one observation per inmate) to join into new dataset
visit_counts_2012 <- visits_2012 %>%
  distinct(InmateNo, .keep_all = TRUE) %>%
  select(InmateNo, VisitCount_2012)

visit_counts_2013 <- visits_2013 %>%
  distinct(InmateNo, .keep_all = TRUE) %>%
  select(InmateNo, VisitCount_2013)

visit_counts_2014 <- visits_2014 %>%
  distinct(InmateNo, .keep_all = TRUE) %>%
  select(InmateNo, VisitCount_2014)

visit_counts_2015 <- visits_2015 %>%
  distinct(InmateNo, .keep_all = TRUE) %>%
  select(InmateNo, VisitCount_2015)

visit_counts_2016 <- visits_2016 %>%
  distinct(InmateNo, .keep_all = TRUE) %>%
  select(InmateNo, VisitCount_2016)

visit_counts_2017 <- visits_2017 %>%
  distinct(InmateNo, .keep_all = TRUE) %>%
  select(InmateNo, VisitCount_2017)

visit_counts_2018 <- visits_2018 %>%
  distinct(InmateNo, .keep_all = TRUE) %>%
  select(InmateNo, VisitCount_2018)

visit_counts_2019 <- visits_2019 %>%
  distinct(InmateNo, .keep_all = TRUE) %>%
  select(InmateNo, VisitCount_2019)

##make new data set
all_visit_counts <- visit_counts_2012 %>%
  full_join(visit_counts_2013, by = "InmateNo") %>%
  full_join(visit_counts_2014, by = "InmateNo") %>%
  full_join(visit_counts_2015, by = "InmateNo") %>%
  full_join(visit_counts_2016, by = "InmateNo") %>%
  full_join(visit_counts_2017, by = "InmateNo") %>%
  full_join(visit_counts_2018, by = "InmateNo") %>%
  full_join(visit_counts_2019, by = "InmateNo")

##correct format errors
all_visit_counts <- all_visit_counts %>%
  filter(!grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", InmateNo),
         !grepl("#", InmateNo),
         !grepl("/", InmateNo),
         !is.na(InmateNo))

##create total visit count column
all_visit_counts <- all_visit_counts %>%
  mutate(across(starts_with("VisitCount"), as.numeric))

all_visit_counts <- all_visit_counts %>%
  group_by(InmateNo) %>%
  mutate(
    Visits12_16 = sum(across(starts_with("VisitCount_2012"):starts_with("VisitCount_2016")), na.rm = TRUE),
    Visits12_19 = sum(across(starts_with("VisitCount_2012"):starts_with("VisitCount_2019")), na.rm = TRUE)
  ) %>%
  ungroup()


##add 2012-2016 visit count to full data set
filtered_release_data <- left_join(filtered_release_data, all_visit_counts, by = c("DCNumber" = "InmateNo"))


##Filter only for people who were in prison at some point between 2012 and 2019
inmates_12_19 <- filtered_release_data %>%
  filter(!((DateAdjudicated > as.Date("2019-01-01")) | (PrisonReleaseDate < as.Date("2012-01-01"))))


##make monthly visit columns
filtered_release_data <- filtered_release_data %>%
  mutate(Monthly_V_12_16 = Visits12_16 / PrisonTermInMonths)
filtered_release_data <- filtered_release_data %>%
  mutate(Monthly_V_12_19 = Visits12_19 / PrisonTermInMonths)

inmates_12_19 <- inmates_12_19 %>%
  mutate_at(vars(VisitCount_2012:VisitCount_2019, Visits12_19, Monthly_V_12_16, Monthly_V_12_19), ~ifelse(is.na(.), 0, .))


##create previous record column to control for previous incarcerations
inmate_subset <- inmate_release_incarhist %>%
  select(DCNumber, ReleaseDate) %>%
  filter(!is.na(ReleaseDate))

result <- inmates_12_19 %>%
  left_join(inmate_subset, by = "DCNumber") %>%
  filter(ReleaseDate < DateAdjudicated) %>%
  group_by(DCNumber) %>%
  summarize(prev_rec = max(!is.na(ReleaseDate)), .groups = "drop")

inmates_12_19 <- left_join(inmates_12_19, result, by = "DCNumber")

inmates_12_19 <- inmates_12_19 %>%
  mutate(prev_rec = coalesce(prev_rec, 0))


##make recidivate column
inmates_12_19 <- inmates_12_19 %>%
  arrange(DCNumber, DateAdjudicated) %>%
  group_by(DCNumber) %>%
  mutate(first_release = ifelse(row_number() == 1, DateAdjudicated + (PrisonTermInMonths * 30.44), NA))

    # Fill any NA values in the first_release column with the last non-NA value within each DCNumber group
inmates_12_19 <- inmates_12_19 %>%
  group_by(DCNumber) %>%
  fill(first_release, .direction = "downup")

    # Convert the first_release column to a Date format
inmates_12_19$first_release <- as.Date(inmates_12_19$first_release)


inmates_12_19 <- inmates_12_19 %>%
  group_by(DCNumber) %>%
  mutate(recidivate = as.integer(
    any(DateAdjudicated > first_release) | DCNumber %in% inmate_active_root$DCNumber
  ))

recidivate_counts <- table(inmates_12_19$recidivate)
print(recidivate_counts)


## num of visits / [prisontermlength - (2012 - dateAdjucated) + (releasedate - 2019)]
inmates_12_19 <- inmates_12_19 %>%
  # Step 1
  mutate(dateAdjucatedAdjusted = pmin(DateAdjudicated, as.Date("2012-01-01")),
         releaseDateAdjusted = pmax(PrisonReleaseDate, as.Date("2019-01-01")),
         # Step 2
         PrisonTermMonths = as.numeric(difftime(as.POSIXct(releaseDateAdjusted), as.POSIXct(dateAdjucatedAdjusted), units = "days")) / 30,
         # Step 3
         MonthlyVisits = Visits12_19 / PrisonTermMonths)


inmates_12_19 <- inmates_12_19 %>%
  mutate(MonthlyVisits = replace_na(MonthlyVisits, 0))


###creating charge_type column
inmates_12_19 <- inmates_12_19 %>%
  mutate(charge_type = case_when(
    grepl("oxycod|fentanyl|BARBITUATE|Narc|Opium|ROHYPNL|BRING CONTR.SUB.INTO STATE|INJECT/INHALE CON.SUBST|cocaine|coc.|POSS/DIST.CHEM.TO MFG.SUB|marijuana|TRAFF CANN|cannabis|ghb|meth|heroin|her.|mdma|hydrocodone|amph|drug|drg|gram|control.sub.|SCH I & II|W/H INFO.FROM PRESCRIBER", adjudicationcharge_descr, ignore.case = TRUE) ~ "drug",
    grepl("fraud|MISREPRESENT|COUNTERFEIT|MEDICAID PRVDR FRD|SELL GOODS W/ FAKE LABEL|False Info|forge|launder|worthless checks|FALS INF. TO PWNBRKR|FALSE STATMT INSURANCE CO.|FALSE INFO SEC MET RECYCL", adjudicationcharge_descr, ignore.case = TRUE) ~ "fraud",
    grepl("violent|human smuggling|HUM TFK|HUMAN TRAFFICK|armed|assault|ASLT|GR.BOD.HARM|BODILY HARM|agg.stalk|abuse|battery|deadly weapon|aggravated|kidnap|ROBB. GUN OR DEADLY WPN|ASSLT|human trafficking|shoot|batt|CONC WPN", adjudicationcharge_descr, ignore.case = TRUE) ~ "violent",
    grepl("ROBB. NO GUN/DDLY.WPN|larceny|BURGLARY:CONTROLLD SUBSTANCE|PROP. STOLEN|theft|robbery|BURGL-FORCED ENTRY-NONRESIDENC|BURGUNOCCSTRUC/CV OR ATT.|BURG/DWELL/OCCUP.CONVEY|BURGL UNOCC DWELLING|BURGL OCCUPIED DWELLING", adjudicationcharge_descr, ignore.case = TRUE) ~ "theft",
    grepl("mur|manslaughter|homicide|Mansl", adjudicationcharge_descr, ignore.case = TRUE) ~ "homicide",
    grepl("resist|flee|obstruct crime investigation|JURY TAMPERING|OBSTRUCTING JUSTICE|FAIL.TO APPEAR/FEL.BAIL|PERJURY|witness", adjudicationcharge_descr, ignore.case = TRUE) ~ "resisting/obstr justice",
    grepl("escape", adjudicationcharge_descr, ignore.case = TRUE) ~ "escape",
    grepl("property|vandal|ARSON,WILLFUL DAMA.STRUCT.|PROP.DAMAGE", adjudicationcharge_descr, ignore.case = TRUE) ~ "property damage",
    grepl("TRAFFIC OFFENSE-OTHER|driv|POSSES VEH W/ILL LIC PLATE|DUI|DRV W/LIC PERM REVOKED", adjudicationcharge_descr, ignore.case = TRUE) ~ "traffic violation",
    grepl("CARRYING CONCEALED FIREARM|POSSESS ILLEGAL WEAPON|THREATENS TO USE ANY FIREARM|ALTER ID ON FIREARM|SECURE FIREARM FROM MINOR|POSS.FIREARM BY FELON|INTRODUCE WEAPON/FIREARM", adjudicationcharge_descr, ignore.case = TRUE) ~ "gun violation",
    TRUE ~ "miscellaneous"
  )) %>%
  select(adjudicationcharge_descr, charge_type, everything())

charge_type_counts <- table(inmates_12_19$charge_type, useNA = "ifany")
print(charge_type_counts)

##Make binary visit variable
inmates_12_19 <- inmates_12_19 %>%
  mutate(Visits_bin = ifelse(MonthlyVisits == 0, 0, 1))


##Make binary sentence length variable (above and below 5 years)
inmates_12_19$five_plus <- as.numeric(inmates_12_19$PrisonTermInMonths >= 60)

inmates_12_19 <- inmates_12_19 %>%
  mutate_at(vars(VisitCount_2012:Monthly_V_12_19, Visits_bin), ~replace_na(., 0))

write.csv(inmates_12_19, "inmates_12_19_2.csv", row.names = FALSE)

##Make subset of data that only include each individual once in the regression
inmates_12_19 <- inmates_12_19 %>%
  arrange(DCNumber, DateAdjudicated, desc(PrisonTermInMonths))

unique_12_19 <- inmates_12_19 %>%
  group_by(DCNumber) %>%
  slice(1)

unique_12_19 <- unique_12_19 %>%
  ungroup()

unique_12_19 <- unique_12_19[!(unique_12_19$prisonterm %in% c(9999998, 9999999)), ]


##Reference categories for regressions
columns_to_convert <- c("race_descr", "Sex", "custody_description", "charge_type")
inmates_12_19 <- inmates_12_19 %>%
  mutate_at(all_of(columns_to_convert), as.factor)
unique_12_19 <- unique_12_19 %>%
  mutate_at(all_of(columns_to_convert), as.factor)

inmates_12_19$race_descr <- relevel(inmates_12_19$race_descr, ref = "WHITE")
inmates_12_19$Sex <- relevel(inmates_12_19$Sex, ref = "M")
inmates_12_19$custody_description <- relevel(inmates_12_19$custody_description, ref = "COMMUNITY")
inmates_12_19$charge_type <- relevel(inmates_12_19$charge_type, ref = "homicide")

unique_12_19$race_descr <- relevel(unique_12_19$race_descr, ref = "WHITE")
unique_12_19$Sex <- relevel(unique_12_19$Sex, ref = "M")
unique_12_19$custody_description <- relevel(unique_12_19$custody_description, ref = "COMMUNITY")
unique_12_19$charge_type <- relevel(unique_12_19$charge_type, ref = "homicide")


variable_labels <- c(
  "PrisonTermInMonths" = "Prison Term In Months",
  "MonthlyVisits" = "Monthly Visits",
  "race_descrBLACK" = "Race: Black",
  "race_descrHISPANIC" = "Race: Hispanic",
  "race_descrAMERICAN INDIAN OR PACIFIC ISL" = "Race: Native/Pacific Islander",
  "race_descrASIAN OR PACIFIC ISLANDER" = "Race: Asian/Pacific Islander",
  "race_descrALL OTHERS/UNKNOWN" = "Race: Other/Unknown",
  "race_descrWHITE" = "Race: White",
  "SexF" = "Sex: Female",
  "Age_At_Adjudication" = "Age At Adjudication",
  "custody_descriptionCOMMUNITY" = "Custody: Community",
  "custody_descriptionCLOSE" = "Custody: Close",
  "custody_descriptionMAXIMUM" = "Custody: Maximum",
  "custody_descriptionMEDIUM" = "Custody: Medium",
  "custody_descriptionMINIMUM" = "Custody: Minimum",
  "custody_descriptionN/A" = "Custody: N/A",
  "charge_typedrug" = "Charge: Drug",
  "charge_typeescape" = "Charge: Escape",
  "charge_typehomicide" = "Charge : Homicide",
  "charge_typefraud" = "Charge: Fraud",
  "charge_typegun violation" = "Charge: Gun Violation",
  "charge_typemiscellaneous" = "Charge: Miscellaneous",
  "charge_typeproperty damage" = "Charge: Property Damage",
  "charge_typeresisting/obstr justice" = "Charge: Resisting/Obstructing Justice",
  "charge_typetheft" = "Charge: Theft",
  "charge_typetraffic violation" = "Charge: Traffic Violation",
  "charge_typeviolent" = "Charge: Violent",
  "Visits_bin" = "Visits (Dummy)",
  "five_plus" = "Sentence Greater than 5 years",
  "prev_rec" = "Previous Record"
)


filtered_data <- unique_12_19[unique_12_19$PrisonTermInMonths <= 350,]
ggplot(filtered_data, aes(x = PrisonTermInMonths, fill = factor(recidivate))) +
  geom_density(alpha = 0.5) +
  labs(x = "Incarceration Length (months)",
       y = "Density",
       fill = "Recidivism") +
  ggtitle("Conditional Density Plot of Incarceration Length by Recidivism (Up to 350 months)")

ggplot(unique_12_19, aes(x = factor(recidivate), y = MonthlyVisits)) +
  geom_boxplot() +
  labs(x = "Recidivated (0 = No, 1 = Yes)",
       y = "Monthly Visits",
       fill = "Recidivism") +
  ggtitle("Distribution of Visitation by Recidivism")


##Regressions
model_1 <- lm(recidivate ~ MonthlyVisits + County + race_descr + Sex + custody_description + Age_At_Adjudication + charge_type + five_plus + prev_rec, data = inmates_12_19)
model_2 <- lm(recidivate ~ Visits_bin + County + race_descr + Sex + custody_description + Age_At_Adjudication + charge_type + five_plus + prev_rec, data = inmates_12_19)
sumtab_1 <- modelsummary(list(model_1, model_2), stars = TRUE, coef_omit = "County|race_descr|Sex", coef_rename = variable_labels, gof_map = c("nobs", "rmse", "r.squared"))

model_3 <- lm(recidivate ~ PrisonTermInMonths + County + race_descr + Sex + custody_description + Age_At_Adjudication + charge_type + five_plus + prev_rec, data = inmates_12_19)
sumtab_2 <- modelsummary(model_3, stars = TRUE, coef_omit = "County|race_descr|Sex", coef_rename = variable_labels, gof_map = c("nobs", "rmse", "r.squared"))


model_4 <- lm(MonthlyVisits ~ PrisonTermInMonths + County + race_descr + Sex + custody_description + Age_At_Adjudication + charge_type + five_plus + prev_rec, data = inmates_12_19)
model_5 <- lm(Visits_bin ~ PrisonTermInMonths + County + race_descr + Sex + custody_description + Age_At_Adjudication + charge_type + five_plus + prev_rec, data = inmates_12_19)
sumtab_3 <- modelsummary(list(model_4, model_5), stars = TRUE, coef_omit = "County|race_descr|Sex", coef_rename = variable_labels, gof_map = c("nobs", "rmse", "r.squared"))



model_6 <- lm(recidivate ~ MonthlyVisits + County + race_descr + Sex + custody_description + Age_At_Adjudication + charge_type + five_plus + prev_rec, data = unique_12_19)
model_7 <- lm(recidivate ~ Visits_bin + County + race_descr + Sex + custody_description + Age_At_Adjudication + charge_type + five_plus + prev_rec, data = unique_12_19)
sumtab_4 <- modelsummary(list(model_6, model_7), stars = TRUE, coef_omit = "County|race_descr|Sex|custody_description|charge_type", coef_rename = variable_labels, gof_map = c("nobs", "rmse", "r.squared")) 
          
html_table <- kable(
  sumtab_4,
  format = "html", 
  table.attr = 'class="table table-striped table-bordered table-hover"',
  escape = FALSE
) %>%
  kable_styling(full_width = FALSE)
save_kable(html_table, file = "sumtab4.png", density = 300)

model_8 <- lm(recidivate ~ PrisonTermInMonths + County + race_descr + Sex + custody_description + Age_At_Adjudication + charge_type + five_plus + prev_rec, data = unique_12_19)
sumtab_5 <- modelsummary(model_8, stars = TRUE, coef_omit = "County|race_descr|Sex|custody_description|charge_type", coef_rename = variable_labels, gof_map = c("nobs", "rmse", "r.squared"))

html_table <- kable(
  sumtab_5,
  format = "html", 
  table.attr = 'class="table table-striped table-bordered table-hover"',
  escape = FALSE
) %>%
  kable_styling(full_width = FALSE)
kableExtra::save_kable(html_table, "sumtab5.png", density = 300)


model_9 <- lm(MonthlyVisits ~ PrisonTermInMonths + County + race_descr + Sex + custody_description + Age_At_Adjudication + charge_type + five_plus + prev_rec, data = unique_12_19)
model_10 <- lm(Visits_bin ~ PrisonTermInMonths + County + race_descr + Sex + custody_description + Age_At_Adjudication + charge_type + five_plus + prev_rec, data = unique_12_19)
sumtab_6 <- modelsummary(list(model_9, model_10), stars = TRUE, coef_omit = "County|race_descr|Sex|custody_description|charge_type", coef_rename = variable_labels, gof_map = c("nobs", "rmse", "r.squared"))

html_table <- kable(
  sumtab_6,
  format = "html", 
  table.attr = 'class="table table-striped table-bordered table-hover"',
  escape = FALSE
) %>%
  kable_styling(full_width = FALSE)
kableExtra::save_kable(html_table, "sumtab6.png")


### Distance model
dist_inmate <- merge(inmate_active_root, inmate_active_offenses_cps, by = "DCNumber")
dist_inmate <- dist_inmate %>%
  mutate(charge_type = case_when(
    grepl("oxycod|fentanyl|BARBITUATE|Narc|Opium|ROHYPNL|BRING CONTR.SUB.INTO STATE|INJECT/INHALE CON.SUBST|cocaine|coc.|POSS/DIST.CHEM.TO MFG.SUB|marijuana|TRAFF CANN|cannabis|ghb|meth|heroin|her.|mdma|hydrocodone|amph|drug|drg|gram|control.sub.|SCH I & II|W/H INFO.FROM PRESCRIBER", adjudicationcharge_descr, ignore.case = TRUE) ~ "drug",
    grepl("fraud|MISREPRESENT|COUNTERFEIT|MEDICAID PRVDR FRD|SELL GOODS W/ FAKE LABEL|False Info|forge|launder|worthless checks|FALS INF. TO PWNBRKR|FALSE STATMT INSURANCE CO.|FALSE INFO SEC MET RECYCL", adjudicationcharge_descr, ignore.case = TRUE) ~ "fraud",
    grepl("violent|human smuggling|HUM TFK|HUMAN TRAFFICK|armed|assault|ASLT|GR.BOD.HARM|BODILY HARM|agg.stalk|abuse|battery|deadly weapon|aggravated|kidnap|ROBB. GUN OR DEADLY WPN|ASSLT|human trafficking|shoot|batt|CONC WPN", adjudicationcharge_descr, ignore.case = TRUE) ~ "violent",
    grepl("ROBB. NO GUN/DDLY.WPN|larceny|BURGLARY:CONTROLLD SUBSTANCE|PROP. STOLEN|theft|robbery|BURGL-FORCED ENTRY-NONRESIDENC|BURGUNOCCSTRUC/CV OR ATT.|BURG/DWELL/OCCUP.CONVEY|BURGL UNOCC DWELLING|BURGL OCCUPIED DWELLING", adjudicationcharge_descr, ignore.case = TRUE) ~ "theft",
    grepl("mur|manslaughter|homicide|Mansl", adjudicationcharge_descr, ignore.case = TRUE) ~ "homicide",
    grepl("resist|flee|obstruct crime investigation|JURY TAMPERING|OBSTRUCTING JUSTICE|FAIL.TO APPEAR/FEL.BAIL|PERJURY|witness", adjudicationcharge_descr, ignore.case = TRUE) ~ "resisting/obstr justice",
    grepl("escape", adjudicationcharge_descr, ignore.case = TRUE) ~ "escape",
    grepl("property|vandal|ARSON,WILLFUL DAMA.STRUCT.|PROP.DAMAGE", adjudicationcharge_descr, ignore.case = TRUE) ~ "property damage",
    grepl("TRAFFIC OFFENSE-OTHER|driv|POSSES VEH W/ILL LIC PLATE|DUI|DRV W/LIC PERM REVOKED", adjudicationcharge_descr, ignore.case = TRUE) ~ "traffic violation",
    grepl("CARRYING CONCEALED FIREARM|POSSESS ILLEGAL WEAPON|THREATENS TO USE ANY FIREARM|ALTER ID ON FIREARM|SECURE FIREARM FROM MINOR|POSS.FIREARM BY FELON|INTRODUCE WEAPON/FIREARM", adjudicationcharge_descr, ignore.case = TRUE) ~ "gun violation",
    TRUE ~ "miscellaneous"
  )) %>%
  select(adjudicationcharge_descr, charge_type, everything())

dist_inmate <- dist_inmate |>
  mutate(
    PrisonTermInMonths = (as.numeric(substr(prisonterm, 1, 3)) * 12) +  
      as.numeric(substr(prisonterm, 4, 5)) +            
      as.numeric(substr(prisonterm, 6, 7)) / 30          
  )

dist_inmate$five_plus <- as.numeric(dist_inmate$PrisonTermInMonths >= 60)

dist_inmate <- dist_inmate %>%
  arrange(DCNumber, DateAdjudicated, desc(PrisonTermInMonths)) |>
  group_by(DCNumber) %>%
  slice(1)

columns_to_merge <- c("MonthlyVisits", "Visits_bin")
dist_inmate <- merge(dist_inmate, unique_12_19[, c("DCNumber", columns_to_merge)], 
                     by = "DCNumber", all.x = TRUE)

dist_inmate[is.na(dist_inmate)] <- 0

dist_inmate$prev_rec <- ifelse(dist_inmate$DCNumber %in% inmate_active_incarhist$DCNumber, 1, 0)
dist_inmate <- dist_inmate[dist_inmate$custody_description != 0, ]

dist_inmate <- dist_inmate |>
  mutate(
    BirthDate = as.Date(BirthDate),
    DateAdjudicated = as.Date(DateAdjudicated),
    Age_At_Adjudication = as.numeric(difftime(DateAdjudicated, BirthDate, units = "days")) / 365.25
  ) |>
  filter(!is.na(Age_At_Adjudication))

dist_inmate <- dist_inmate %>% 
  filter(charge_type == "violent")

write_csv(dist_inmate, "dist_inmate.csv")

get_coordinates <- function(facility_name) {
  coordinates <- switch(
    facility_name,
    "S.F.R.C." = "25.811928378185065, -80.43322262005965",
    "EVERGLADES C.I." = "25.749791921610463, -80.49200044889707",
    "DESOTO ANNEX" = "27.20591392187175, -81.66339554202546",
    "MARTIN C.I." = "27.197752043323256, -80.48264757583546",
    "CFRC-MAIN" = "28.46354991885772, -81.15420097763634",
    "OKEECHOBEE C.I." = "27.39947420495905, -80.78415364883992",
    "LAWTEY C.I." = "30.032891035360667, -82.0713436757294",
    "HAMILTON ANNEX" = "30.50525739821907, -82.9630828761486",
    "UNION C.I." = "30.069812188199077, -82.19272819106905",
    "SUWANNEE C.I" = "30.23827225611861, -82.86890106222714",
    "POLK C.I." = "28.191655185822896, -81.78538133531697",
    "WALTON C.I." = "30.778266175987934, -86.10277997569978",
    "BAKER RE-ENTRY CENTR" = "30.233036779049563, -82.31773903339206",
    "R.M.C.- MAIN UNIT" = "29.989385186182957, -82.35828314874277",
    "APALACHEE EAST UNIT" = "30.712504679178625, -84.88593619104343",
    "COLUMBIA C.I." = "30.193378714057378, -82.50272468921726",
    "JACKSONVILLE BRIDGE" = "30.33108308078785, -81.7486905757176",
    "COLUMBIA ANNEX" = "30.193406533911713, -82.50275687572307",
    "CENTURY C.I." = "30.95685538831546, -87.2915164045278",
    "TAYLOR C.I." = "30.040271451693332, -83.71509490456438",
    "AVON PARK C.I." = "27.673824046189633, -81.35715968519419",
    "BRIDGES OF JACKSONVI" = "30.33108308078785, -81.7486905757176",
    "REENTRY CTR OF OCALA" = "29.323272404632046, -82.11818471480582",
    "NWFRC MAIN UNIT." = "30.518053870644373, -85.65957421803962",
    "CHARLOTTE C.I." = "26.805128107836616, -81.90348571534106",
    "TTH OF DINSMORE" = "30.474359428796223, -81.78640778920614",
    "FRANKLIN C.I." = "29.88021356525263, -84.64582588922961",
    "ORANGE" = "28.506741648318616, -81.41432710092941",
    "FL.WOMENS RECPN.CTR" = "29.310106682716516, -82.18849209294542",
    "SUNCOAST C.R.C.(FEM)" = "27.865371305574325, -82.62539130464674",
    "LOWELL ANNEX" = "29.314182970322456, -82.19916477760422",
    "HOLMES C.I." = "30.758817948869613, -85.66992383152417",
    "WAKULLA C.I." = "30.261323711241992, -84.17431305853242",
    "BRIDGES OF LAKE CITY" = "30.20042369702774, -82.65451182333085",
    "HOMESTEAD C.I." = "25.413938355657862, -80.4984874912377",
    "SAGO PALM RE-ENTRY C" = "26.840336444783567, -80.63565732002441",
    "DADE C.I." = "25.415541334978997, -80.49993910473184",
    "HARDEE C.I." = "27.590620235624826, -82.01171799101574",
    "ZEPHYRHILLS C.I." = "28.198097208836426, -82.1970137892935",
    "JACKSON C.I." = "30.968902361029613, -85.16790374685678",
    "OKALOOSA WORK CAMP" = "30.6949126945615, -86.52405148919728",
    "NWFRC ANNEX." = "30.517812828368964, -85.65687961988654",
    "PENSACOLA C.R.C." = "30.443516466380103, -87.23554536221897",
    "SANTA ROSA C.I." = "30.645503874517814, -86.9683412161877",
    "QUINCY ANNEX" = "30.55284951020214, -84.59111960639095",
    "SUWANNEE C.I. ANNEX" = "30.23881091901909, -82.86739641989757",
    "LANCASTER C.I." = "29.610701628893207, -82.86531397389903",
    "MOORE HAVEN C.F." = "26.838584586127993, -81.12012867584818",
    "GRACEVILLE C.F." = "30.944403215316317, -85.48933851802249",
    "TALLAHASSEE C.R.C" = "30.408452828289622, -84.30582860454982",
    "CALHOUN C.I." = "30.408891539310787, -85.06474008920873",
    "MTC-MIAMI NORTH CRC" = "25.811434163205373, -80.31252513355396",
    "GADSDEN RE-ENTRY CTR" = "30.546955350762772, -84.49500304687372",
    "WAKULLA ANNEX" = "30.26134224500216, -84.17434524503823",
    "SUMTER C.I." = "28.61774002039959, -82.20459186228955",
    "JEFFERSON C.I." = "30.51019711107286, -83.79835161988683",
    "CFRC-EAST" = "28.46362537434629, -81.15412587578946",
    "TTH OF BARTOW" = "27.901870518735958, -81.82235200649228",
    "FEDERAL CUSTODY" = NA,
    "BLACKWATER C.F." = "30.645780000225777, -86.9529814468698",
    "MARTIN WORK CAMP" = "27.204914442275342, -80.48258430282365",
    "LIBERTY C.I." = "30.46569186234193, -84.86514415057077",
    "LOWELL WORK CAMP" = "29.311538086243118, -82.19817238161116",
    "SOUTH BAY C.F." = "26.657955021141607, -80.71883789304242",
    "GULF C.I." = "30.174377294481207, -85.26390312899676",
    "HARDEE WORK CAMP" = "27.59391422868129, -82.00724573349186",
    "KISSIMMEE C.R.C." = "28.336658013063794, -81.38936033126097",
    "LAKE C.I." = "28.62283308272089, -81.7658053064657",
    "GADSDEN C.F." = "30.58972788234993, -84.65458081988365",
    "ORLANDO BRIDGE" = "28.570504445828007, -81.42800540646758",
    "AVON PARK WORK CAMP" = "27.669454580978044, -81.35945762935465",
    "MAYO C.I. ANNEX" = "30.132997528330463, -83.30524906407815",
    "ST. PETE C.R.C." = "27.762248189706337, -82.69071814697993",
    "DESOTO WORK CAMP" = "27.203791460295665, -81.66517964699999",
    "MARION C.I." = "29.306303391273435, -82.17685243342817",
    "MADISON C.I." = "30.443056668609856, -83.42109996037212",
    "FLORIDA STATE PRISON" = "30.058245710861293, -82.18255808922264",
    "BRIDGES OF COCOA" = "28.44655824383574, -80.77503253346063",
    "TTH OF KISSIMMEE" = "28.30429507514491, -81.39573870278379",
    "POLK WORK CAMP" = "28.19169301020971, -81.78538133531697",
    "WALTON WORK CAMP" = "30.777322162894137, -86.1103575333703",
    "ORLANDO C.R.C." = "28.463587646608747, -81.15422243530686",
    "PUTNAM C.I." = "29.64542338495768, -81.5879983585566",
    "TURNING POINT C.R.C" = "26.229500176235153, -80.13000000470446",
    "CROSS CITY C.I." = "29.63479990973737, -83.09875126225077",
    "SANTA ROSA ANNEX" = "30.64552233535522, -86.96844850454038",
    "TFF-Ft. PIERCE CRC" = "27.396334745251423, -80.33727548747572",
    "BAY C.F." = "30.26681060476266, -85.50553773323928", 
    "EVERGLADES RE-ENTRY" = "25.75008207316947, -80.49497407773228",
    "LOWELL C.I." = "29.313959752883335, -82.19908700275948",
    "CROSS CITY EAST UNIT" = "29.638198876799194, -83.08673273156846",
    "MTC-PANAMA CITY CRC" = "30.214098412554304, -85.67205479119272",
    "REALITY HOUSE" = "29.14130383741784, -81.14343523528139",
    "APALACHEE WEST UNIT" = "30.71308540960057, -84.88598990084229",
    "LAKE CITY C.F." = "30.193972914158905, -82.5073942423291",
    "S.F.R.C SOUTH UNIT" = "25.81246890727873, -80.43326560374662",
    "HAMILTON C.I." = "30.505406199171663, -82.96310813960869",
    "CFRC-SOUTH" = "28.464294675929107, -81.15452290963239",
    "OKALOOSA C.I." = "31.21842746551553, -86.47447606916035",
    "UNION WORK CAMP" = "30.070712443299225, -82.19243857851836",
    "R.M.C.- WEST UNIT" = "29.995043002644046, -82.36806173198411",
    "TOMOKA C.I." = "29.151681010901235, -81.15659105027834",
    "HOLLYWOOD C.R.C." = "26.004223235033262, -80.25646850364298",
    "BREVARD" = "28.448703743003524, -80.781453248831",
    "TOMOKA WORK CAMP" = "29.15598745609317, -81.1596153404544",
    "SUMTER WORK CAMP" = "28.614315063854942, -82.20432607941554",
    "LARGO R.P." = "27.896848126857925, -82.70548238723359",
    "BRIDGES OF SANTA FE" = "29.683524547303186, -82.28642141683737",
    "CENTRAL OFFICE" = "30.437319231357694, -84.27921442804889",
    "LANCASTER W.C." = "29.610925120215946, -82.86505654812257",
    "JACKSON WORK CAMP" = "30.96984102517034, -85.16305723930878",
    "MARION WORK CAMP" = "29.30409043297753, -82.1812648630977",
    "R.M.C WORK CAMP" = "30.008053907485326, -82.36582121741176",
    "CROSS CITY WORK CAMP" = "29.635194357086895, -83.09883611882682",
    "HOLMES WORK CAMP" = "30.7592047893908, -85.66990243944514",
    "CENTURY WORK CAMP" = "31.085230910253447, -87.27653513752315",
    "TOMOKA CRC" = "29.139978466327285, -81.12650344046409",
    "BRIDGES OF ORLANDO" = "28.570221595798184, -81.43056324025275",
    "MADISON WORK CAMP" = "30.444116366679385, -83.4239156856733",
    "LOXAHATCHEE R.P." = "26.684253768919593, -80.19320367314138",
    "HERNANDO C.I." = "28.483801704495917, -82.45397471017726",
    "BRADENTON BRIDGE" = "27.45682563696308, -82.53087586255671",
    "W.PALM BEACH C.R.C." = "26.683988956122, -80.19266814190112",
    "TTH OF TARPON SPRING" = "28.168511002003285, -82.76890040241544",
    "SHISA HOUSE WEST" = "30.44711171568642, -84.28660173169705",
    "OPA LOCKA C.R.C." = "25.89837657440035, -80.2889332423332",
    "COLLIER" = "26.15536820088173, -81.77323636904725",
    "ATLANTIC C.R.C." = "26.68360526378663, -80.19271994190153",
    "FRANKLIN" = "31.94333248318393, -84.77367444763503",
    "BAY" = "32.12999306548944, -85.37006647669662",
    "LEON" = "30.437507898369784, -84.33531775499357",
    "MIAMI-DADE" = "25.788747630517236, -80.21629126408361",
    "MARTIN" = "27.172384766495256, -80.24188884957523",
    "ESCAMBIA" = "30.44377857167923, -87.2382568396481",
    "PINELLAS" = "27.903315526544823, -82.70535047753478",
    "HILLSBOROUGH" = "27.969345568511706, -82.37283891168516",
    "VOLUSIA" = "29.141336962653178, -81.14935785859032",
    "PALM BEACH" = "26.670979216087648, -80.09250625725022",
    "NEW RIVER CI" = "30.272514205214748, -82.22230816455239",
    NA
  )
  return(coordinates)
}

dist_inmate <- dist_inmate %>%
  mutate(facility_coord = sapply(FACILITY_description, get_coordinates))

dist_inmate <- dist_inmate[!is.na(dist_inmate$facility_coord), ]

counties <- counties("FL", cb = TRUE)
centroids <- st_centroid(counties)
county_names <- counties$NAME
centroid_data <- data.frame(County = county_names, st_coordinates(centroids))

centroid_data$County <- toupper(centroid_data$County)
centroid_data$centroid_coords <- paste(centroid_data$X, centroid_data$Y, sep = ", ")

dist_inmate <- merge(dist_inmate, centroid_data, by = "County", all.x = TRUE)

centroid_coords <- strsplit(as.character(dist_inmate$centroid_coords), ", ")
centroid_coords <- do.call(rbind, centroid_coords)

dist_inmate <- separate(dist_inmate, facility_coord, into = c("facility_coord_lat", "facility_coord_lon"), sep = ", ", convert = TRUE)

dist_inmate <- dist_inmate %>%
  rename(centroid_lon = X, centroid_lat = Y)

# Convert the new columns to numeric
dist_inmate$centroid_lat <- as.numeric(dist_inmate$centroid_lat)
dist_inmate$centroid_lon <- as.numeric(dist_inmate$centroid_lon)

dist_inmate <- dist_inmate[complete.cases(dist_inmate$centroid_lat, dist_inmate$centroid_lon, dist_inmate$facility_coord_lat, dist_inmate$facility_coord_lon), ]

dist_inmate$Distance <- distHaversine(p1 = dist_inmate[, c("facility_coord_lon", "facility_coord_lat")],
                                      p2 = dist_inmate[, c("centroid_lon", "centroid_lat")])

dist_inmate$Distance <- dist_inmate$Distance / 1000

write_csv(dist_inmate, "dist_inmate_2.csv")

iv_model <- ivreg(MonthlyVisits ~ Distance | County + race_descr + Sex + custody_description + Age_At_Adjudication + five_plus + prev_rec, data = dist_inmate)
summary(iv_model)

iv_model_2 <- ivreg(Visits_bin ~ Distance | County + race_descr + Sex + custody_description + Age_At_Adjudication + five_plus + prev_rec, data = dist_inmate)
summary(iv_model_2)

sumtab_7 <- modelsummary(list(iv_model, iv_model_2), stars = TRUE, coef_omit = "County|race_descr|Sex|custody_description", gof_map = c("nobs", "rmse", "r.squared"))

html_table <- kable(
  sumtab_7,
  format = "html", 
  table.attr = 'class="table table-striped table-bordered table-hover"',
  escape = FALSE
) %>%
  kable_styling(full_width = FALSE)
kableExtra::save_kable(html_table, "sumtab7.png")


###Summary statistics
summary_whole <- unique_12_19 %>%
  summarize(
    "Age Minimum" = min(Age_At_Adjudication),
    "Age Mean" = mean(Age_At_Adjudication),
    "Age Max" = max(Age_At_Adjudication),
    "Age SD" = sd(Age_At_Adjudication),
    "Prison Term Minimum" = min(PrisonTermInMonths),
    "Prison Term Mean" = mean(PrisonTermInMonths),
    "Prison Term Maximum" = max(PrisonTermInMonths),
    "Prison Term SD" = sd(PrisonTermInMonths)  
  )

summary_with_visits <- unique_12_19 %>%
  filter(Visits_bin == 1) %>%
  summarize(
    "Age Minimum" = min(Age_At_Adjudication),
    "Age Mean" = mean(Age_At_Adjudication),
    "Age Max" = max(Age_At_Adjudication),
    "Age SD" = sd(Age_At_Adjudication),
    "Prison Term Minimum" = min(PrisonTermInMonths),
    "Prison Term Mean" = mean(PrisonTermInMonths),
    "Prison Term Maximum" = max(PrisonTermInMonths),
    "Prison Term SD" = sd(PrisonTermInMonths)  
  )

summary_stats <- bind_rows(
  data.frame(Sample = "Whole Sample", summary_whole),
  data.frame(Sample = "With Visits", summary_with_visits)
)

summary_stats_df <- as.data.frame(summary_stats)
summary_stats_df[, -1] <- round(summary_stats_df[, -1], 2)

colnames(summary_stats_df) <- c("", "Min", "Mean", "Max", "SD",
                                "Min", "Mean", "Max", "SD")

kable(summary_stats_df, align = "c") %>%
  kable_styling(full_width = FALSE)

table <- kable(summary_stats_df, align = "c")

styled_table <- kable(summary_stats_df, align = "c", format = "html") %>%
  # Add spanner for "Age"
  add_header_above(c("", "Age" = 4, "Prison Term" = 4))

styled_table <- kable_styling(styled_table, full_width = FALSE)

print(styled_table)




whole_sample_race_props <- prop.table(table(unique_12_19$Race)) * 100
whole_sample_race_props <- round(whole_sample_race_props, 2)

with_visits_sample_race <- unique_12_19[unique_12_19$Visits_bin == 1, ]
with_visits_sample_race_props <- prop.table(table(with_visits_sample_race$Race)) * 100
with_visits_sample_race_props <- round(with_visits_sample_race_props, 2)

whole_sample_race_df <- data.frame(Race = names(whole_sample_race_props),
                                   Whole_Sample_Prop = as.vector(whole_sample_race_props))

with_visits_sample_race_df <- data.frame(Race = names(with_visits_sample_race_props),
                                         With_Visits_Prop = as.vector(with_visits_sample_race_props))

combined_race_df <- merge(whole_sample_race_df, with_visits_sample_race_df, by = "Race", all = TRUE)
combined_race_df[is.na(combined_race_df)] <- 0
names(combined_race_df) <- c("Race", "Whole Sample", "With Visits")

combined_race_df$Race[combined_race_df$Race == "A"] <- "Asian"
combined_race_df$Race[combined_race_df$Race == "B"] <- "Black"
combined_race_df$Race[combined_race_df$Race == "H"] <- "Hispanic"
combined_race_df$Race[combined_race_df$Race == "I"] <- "Indigenous"
combined_race_df$Race[combined_race_df$Race == "U"] <- "Unknown"
combined_race_df$Race[combined_race_df$Race == "W"] <- "White"


kable(combined_race_df, align = "c", caption = "Percentage of Each Race in the Samples") %>%
  kable_styling(full_width = FALSE)



whole_sample_sex_props <- prop.table(table(unique_12_19$Sex)) * 100
whole_sample_sex_props <- round(whole_sample_sex_props, 2)

with_visits_sample_sex <- unique_12_19[unique_12_19$Visits_bin == 1, ]
with_visits_sample_sex_props <- prop.table(table(with_visits_sample_sex$Sex)) * 100
with_visits_sample_sex_props <- round(with_visits_sample_sex_props, 2)

whole_sample_sex_df <- data.frame(Sex = names(whole_sample_sex_props),
                                  Whole_Sample_Prop = as.vector(whole_sample_sex_props))

with_visits_sample_sex_df <- data.frame(Sex = names(with_visits_sample_sex_props),
                                        With_Visits_Prop = as.vector(with_visits_sample_sex_props))

combined_sex_df <- merge(whole_sample_sex_df, with_visits_sample_sex_df, by = "Sex", all = TRUE)
combined_sex_df[is.na(combined_sex_df)] <- 0
combined_sex_df$Sex[combined_sex_df$Sex == "F"] <- "Female"
combined_sex_df$Sex[combined_sex_df$Sex == "M"] <- "Male"
names(combined_sex_df) <- c("Sex", "Whole Sample", "With Visits")

kable(combined_sex_df, align = "c", caption = "Percentage of Each Sex in the Samples") %>%
  kable_styling(full_width = FALSE)



whole_sample_charge_type_props <- prop.table(table(unique_12_19$charge_type)) * 100
whole_sample_charge_type_props <- round(whole_sample_charge_type_props, 2)

with_visits_sample_charge_type <- unique_12_19[unique_12_19$Visits_bin == 1, ]
with_visits_sample_charge_type_props <- prop.table(table(with_visits_sample_charge_type$charge_type)) * 100
with_visits_sample_charge_type_props <- round(with_visits_sample_charge_type_props, 2)

whole_sample_charge_type_df <- data.frame(charge_type = names(whole_sample_charge_type_props),
                                          Whole_Sample_Prop = as.vector(whole_sample_charge_type_props))

with_visits_sample_charge_type_df <- data.frame(charge_type = names(with_visits_sample_charge_type_props),
                                                With_Visits_Prop = as.vector(with_visits_sample_charge_type_props))
combined_charge_type_df <- merge(whole_sample_charge_type_df, with_visits_sample_charge_type_df, by = "charge_type", all = TRUE)
combined_charge_type_df[is.na(combined_charge_type_df)] <- 0
names(combined_charge_type_df) <- c("Charge Type", "Whole Sample", "With Visits")
combined_charge_type_df$`Charge Type`[combined_charge_type_df$`Charge Type` == "drug"] <- "Drug"
combined_charge_type_df$`Charge Type`[combined_charge_type_df$`Charge Type` == "escape"] <- "Escape"
combined_charge_type_df$`Charge Type`[combined_charge_type_df$`Charge Type` == "fraud"] <- "Fraud"
combined_charge_type_df$`Charge Type`[combined_charge_type_df$`Charge Type` == "gun violation"] <- "Gun Violation"
combined_charge_type_df$`Charge Type`[combined_charge_type_df$`Charge Type` == "homicide"] <- "Homicide"
combined_charge_type_df$`Charge Type`[combined_charge_type_df$`Charge Type` == "miscellaneous"] <- "Miscellaneous"
combined_charge_type_df$`Charge Type`[combined_charge_type_df$`Charge Type` == "property damage"] <- "Property Damage"
combined_charge_type_df$`Charge Type`[combined_charge_type_df$`Charge Type` == "resisting/obstr justice"] <- "Resisting/Obstructing Justice"
combined_charge_type_df$`Charge Type`[combined_charge_type_df$`Charge Type` == "theft"] <- "Theft"
combined_charge_type_df$`Charge Type`[combined_charge_type_df$`Charge Type` == "traffic violation"] <- "Traffic Violation"
combined_charge_type_df$`Charge Type`[combined_charge_type_df$`Charge Type` == "violent"] <- "Violent"


kable(combined_charge_type_df, align = "c", caption = "Percentage of Each Charge in the Samples") %>%
  kable_styling(full_width = FALSE)

combined_race_df$category <- "Race"
combined_sex_df$category <- "Sex"
combined_charge_type_df$category <- "Charge Type"

colnames(combined_race_df) <- c("Variable", "Demographic", "Whole Sample", "With Visits")
colnames(combined_sex_df) <- c("Variable", "Demographic", "Whole Sample", "With Visits")
colnames(combined_charge_type_df) <- c("Variable", "Demographic", "Whole Sample", "With Visits")

combined_df <- rbind(combined_race_df, combined_sex_df, combined_charge_type_df)
combined_table <- gt(combined_df)

styled_table <- kable(combined_df, align = "c", caption = "Summary Statistics") %>%
  kable_styling(full_width = FALSE)

styled_table <- kable(
  styled_table,
  format = "html", 
  table.attr = 'class="table table-striped table-bordered table-hover"',
  escape = FALSE
) %>%
  kable_styling(full_width = FALSE)
kableExtra::save_kable(styled_table, "styled_table.png")
