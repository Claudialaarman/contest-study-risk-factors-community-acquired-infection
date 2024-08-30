#####################################################################
###########Sensitivity analyses days since vaccination################
#####################################################################

source(file = "/rivm/r/COVID-19/Onderzoek/CONTEST STUDY/8. Analysis/Claudia/PloS ONE/load_packages.R")
source("Data_analysis/01_Initialiseren/initialiseren_datums.R")

#############Symptomatic cases vs symptomatic controls######################


#loading data
contest <- "Data/Cleaned/Contest_data_cleaned_20220406_1030.rds" %>%
  readRDS()

#######filtering for population for analyses#########

#filtering on the time period 1-6-2021 t/m 28-02-2022
contest <- contest %>%
  filter(dateentry >= as.Date("2021-06-01")) %>%
  filter(dateentry <= as.Date("2022-02-28"))


contest_nieuw <- contest


#filtering on symptomatic persons

table (contest_nieuw$symptomatic)

sum(is.na(contest_nieuw$symptomatic))


contest_nieuw1<- contest_nieuw%>%
  filter  (symptomatic %in% c("1"))




#filtering on fully vaccinated & unvaccinated
table (contest_nieuw1$vaccinatiestatus)
sum(is.na(contest_nieuw1$vaccinatiestatus))

contest_nieuw3 <- contest_nieuw1 %>%
  filter  (vaccinatiestatus %in% c("Fully vaccinated", "Unvaccinated") | is.na(vaccinatiestatus))




#filtering on maximum of two doses
table (contest_nieuw3$coronavaccinatie_hoeveel_combined)
sum(is.na(contest_nieuw3$coronavaccinatie_hoeveel_combined))

contest_nieuw4 <- contest_nieuw3%>%
  filter  (coronavaccinatie_hoeveel_combined %in% c ("1 dose", "2 doses") | is.na(coronavaccinatie_hoeveel_combined))




#filtering on people who were not contaminated with SARS-CoV-2 in their household
table (contest_nieuw4$contactpersooncorona_waar)
sum(is.na(contest_nieuw4$contactpersooncorona_waar))


contest_nieuw5 <- contest_nieuw4 %>%
  filter (contactpersooncorona_waar %in% c ("0", "1", "3", "4", "5", "6", "7"))


#filtering on people who did not have a positive self-administered test
table (contest_nieuw5$reden_testen)
sum(is.na(contest_nieuw5$reden_testen))


contest_nieuw6 <- contest_nieuw5%>%
  filter (reden_testen %in% c("1", "1;2", "1;2;3", "1;2;3;4", "1;2;3;7", "1;2;4", "1;2;4;5", "1;2;5", "1;2;5;7", "1;2;7", "1;3", "1;3;4", "1;3;5", "1;3;7", "1;4", "1;4;5", "1;4;5;7", "1;4;7", "1;5", "1;5;7", "1;7", "2", "2;3", "2;3;4", "2;3;5", "2;4", "2;5", "2;7", "3", "3;4", "3;4;5", "3;5", "4", "4;5", "4;7", "5", "5;7", "7")| is.na(reden_testen))


#crosstable on vaccinated and unvaccinated and their test result
table (contest_nieuw6$coronavaccinatie_x, contest_nieuw6$uitslag)

#check for people with a positive self-administered test in the open field of the question reason of testing
contest_nieuw_select <- select (contest_nieuw6, reden_testen_and)

#view(contest_nieuw_select)




#filter people out with a positive self-administered in the open field of the question reason of testing
contest_nieuw7 <- contest_nieuw6[-c(6, 103, 302, 348, 517, 538, 915, 999, 1073, 1422, 1513, 1956, 2243, 5508), ]


#check filtering
contest_nieuw_select2 <- select (contest_nieuw7, reden_testen_and)

#view(contest_nieuw_select2)





####Data cleaning and management#########

#gender
contest_nieuw7 <- contest_nieuw7 %>% mutate(
  sex = case_when(
    geslacht_x == "Man"  ~ 1,
    geslacht_x == "Vrouw" ~ 0 # removing Other since that is too small a number
  ) %>% as_factor())

contest_nieuw7$sex <- factor(contest_nieuw7$sex,
                             levels = c(0,1),
                             labels = c("Female", "Male"))

table(contest_nieuw7$sex)


# educational level
# dropping Other since too small a number for glm later on

contest_nieuw7 <- contest_nieuw7 %>% mutate(educationlevel_new = case_when(
  educationlevel_new == "Low"  ~ 1,
  educationlevel_new == "Middle" ~ 2,
  educationlevel_new == "High"  ~ 3
) %>% factor(levels = c(1,2,3), labels = c( "Low", "Middle", "High")))

table(contest_nieuw7$educationlevel_new, useNA = "always")

# agegroup
class(contest_nieuw7$agegroup)
table(contest_nieuw7$agegroup, useNA = "always")

contest_nieuw7 <- contest_nieuw7 %>%  mutate(agegroup = case_when(
  agegroup == "18-29"  ~ 1,
  agegroup == "30-44"  ~ 2,
  agegroup == "45-59"  ~ 3,
  agegroup == "60-69"  ~ 4,
  agegroup == "70+"    ~ 5,
))

mean(contest_nieuw7$age)
median(contest_nieuw7$age)


contest_nieuw7$agegroup <- factor(contest_nieuw7$agegroup,
                                  levels = c(1,2,3,4,5),
                                  labels = c( "18-29", "30-44", "45-59", "60-69", "70+"))

class(contest_nieuw7$agegroup)

table(contest_nieuw7$age)

#household size
#totalhousehold_cat_new gebruiken


#Living with children (0-12)
contest_nieuw7 <- contest_nieuw7 %>%
  mutate(livingwithchildren_0_12 = case_when(
    mensensamen_jonger4 >= "1" | mensensamen_4_12 >= "1" ~ 1,
    huishouden_hoeveel < "1" ~ 0,
    TRUE ~ 0
  ) %>% factor(levels = c(0,1),
               labels = c("Not living with children 0-12", "Living with children 0-12")))

table(contest_nieuw7$livingwithchildren_0_12)


#Living with children (13-18)
contest_nieuw7 <- contest_nieuw7 %>%
  mutate(livingwithchildren_13_18 = case_when(
    mensensamen_13_18 >= "1" ~ 1,
    huishouden_hoeveel < "1" ~ 0,
    TRUE ~ 0
  ) %>% factor(levels = c(0,1),
               labels = c("Not living with children 13-18", "Living with children 13-18")))

table(contest_nieuw7$livingwithchildren_13_18)



#work environment
#went to work in the previous 14 days

contest_nieuw7 <- contest_nieuw7 %>%
  mutate(went_to_work = case_when (
    werk_gegaan_x == "Nooit" ~ "0",
    werk_gegaan_x == "1-2 dagen" ~ "1",
    werk_gegaan_x == "3-5 dagen" ~ "1",
    werk_gegaan_x == "6-8 dagen" ~ "2",
    werk_gegaan_x == "9-10 dagen" ~ "2",
    werk_gegaan_x == "Meer dan 10 dagen" ~ "3",
    werk_gegaan_x == "Niet van toepassing" ~ "4",
    TRUE ~ "4"))



contest_nieuw7 $went_to_work <- factor(contest_nieuw7$went_to_work,
                                       levels = c(0,1,2,3,4),
                                       labels = c("Did not go to work", "1-5 days", "6-10 days", ">10 days", "Not applicable"))



#Work re-categorisation
contest_nieuw7 <- contest_nieuw7 %>%
  mutate(werk_welkesector_cat_old = case_when (
    werk_welkesector_old_x == "Gezondheidszorg met contact met patienten" ~ "HCW contact with patients",
    werk_welkesector_old_x == "Gezondheidszorg zonder contact met pati?nten" ~ "HCW no contact with patients",
    werk_welkesector_old_x == "Sociaal of maatschappelijk werker" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Mantelzorger" ~ "Contact profession",
    werk_welkesector_old_x == "Onderwijs en kinderopvang" ~ "Daycare - Education",
    werk_welkesector_old_x == "Openbaar vervoer" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Politie, marechaussee, brandweer, Dienst Justiti?le Inrichtingen, of BOA" ~ "Contact profession",
    werk_welkesector_old_x == "Transport (luchtvaart, haven, wegtansport, scheepvaart)" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Afvalverwerking" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Vleesverwerkende industrie (zoals een slachthuis)" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Landbouw (o.a. fruit- en groenteteelt, kwekerijen en glastuinbouw)" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Supermarkt medewerker" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Winkelmedewerker (anders dan supermarkt)" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Schoonmaakbranche" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Kapster, schoonheidsspecialist, manicure of pedicure" ~ "Contact profession",
    werk_welkesector_old_x == "Fabrieksmedewerker" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Medewerker in pretpark dierentuin of speeltuin" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Horeca (restaurant, caf?, bar etc.)" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Sportinstructeur (personal) trainer of scheidsrechter" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Rij-instructeur" ~ "Contact profession",
    werk_welkesector_old_x == "Sekswerker" ~ "Contact profession",
    werk_welkesector_old_x == "Kantoorfunctie" ~ "Other profession (likely remote)",
    werk_welkesector_old_x == "Anders" ~ "Other",
    werk_x == "Nee"  ~ "Does not work",
    TRUE ~ "NA"))


#(7 categories)
contest_nieuw7$werk_welkesector_cat_old<- factor(contest_nieuw7$werk_welkesector_cat_old, levels=c("Other",
                                                                                                   "HCW contact with patients",
                                                                                                   "HCW no contact with patients",
                                                                                                   "Daycare - Education",
                                                                                                   "Contact profession",
                                                                                                   "Other profession (not possible remote)",
                                                                                                   "Other profession (likely remote)",
                                                                                                   "Does not work"))




#only taking both HCW categories
contest_nieuw7 <- contest_nieuw7 %>%  mutate(hcw_other = case_when(
  werksector_no1 == "Gezondheidszorg zonder patiëntencontact of met patientencontact maar op meer dan 1.5m afstand" ~ 1,
  werksector_no2 == "Gezondheidszorg zonder patiëntencontact of met patientencontact maar op meer dan 1.5m afstand" ~ 1,
  werksector_no3 == "Gezondheidszorg zonder patiëntencontact of met patientencontact maar op meer dan 1.5m afstand" ~ 1,
  werk_welkesector_old_x == "Gezondheidszorg zonder contact met pati?nten"  ~ 1,
  TRUE   ~ 0 
))

contest_nieuw7$hcw_other <-factor(contest_nieuw7$hcw_other,
                                  levels = c(0,1),
                                  labels = c("No", "Yes"))


contest_nieuw7 <- contest_nieuw7 %>%  mutate(hcw_cat = case_when(
  hcw =="Yes"        ~ 1,
  hcw_other == "Yes" ~ 1,
  TRUE               ~ 0 
))

contest_nieuw7$hcw_cat <- factor(contest_nieuw7$hcw_cat,
                                 levels = c(0,1),
                                 labels = c( "No", "Yes"))

# contact work/study

# trying to combine all into one
contest_nieuw7 <- contest_nieuw7 %>% mutate(work_study_contacts = case_when(
  # you don't work and are not a student
  werk_gegaan_contact_x == "Nee" & student_gegaan_old_x == "Nee"      ~ 0,    # did not have close contact at work or universitiy
  werk_gegaan_contactmensen_x == "1-4 mensen"             ~ 1 ,           # you went to work and had close contact with 1-4 people
  student_gegaan_hoeveelmen_old_x == "1-4 mensen"         ~ 1,                # you went to study and had close contact with 1-4 people
  werk_gegaan_contactmensen_x == "5-9 mensen"             ~ 2,
  student_gegaan_hoeveelmen_old_x == "5-9 mensen"         ~ 2 ,
  werk_gegaan_contactmensen_x == "10-19 mensen"           ~ 3,
  student_gegaan_hoeveelmen_old_x == "10-19 mensen"       ~ 3 ,
  werk_gegaan_contactmensen_x == "20 of meer mensen"      ~ 4 ,
  student_gegaan_hoeveelmen_old_x == "20 of meer mensen"   ~ 4,
  TRUE                                                       ~ 0
) %>% factor( levels = c( 0, 1,2,3,4),
              labels = c( "zero contacts" , "1-4 contacts", " 5-9 contacts", "10-19 contacts", "20 or more contacts")) )


#locations inside
# busy locations insides
contest_nieuw7 <- contest_nieuw7 %>% mutate(busylocations_inside = case_when(
  bijeenkomst_binnen_old_x == "Nee"               ~ 0 , # not been to busy locations inside
  locaties_binnen == 2                            ~ 0 , # not been to busy locations inside
  locaties_binnen == 3                            ~ 1 , # been to busy location inside new variable
  bijeenkomst_binnen_old_x == "Ja" &
    (drukkelocatie_andere1_af_old_x == "Nee" |
       drukkelocatie_andere2_af_old_x == "Nee" |
       drukkelocatie_andere3_af_old_x == "Nee" |
       drukkelocatie_kerk_afs_old_x == "Nee" |
       drukkelocatie_feest_afs_old_x == "Nee" )        ~ 1, # been to a location inside (old variable) and could not keep 1.5 m distance
  TRUE       ~  0 )  %>%
    factor(levels = c(0,1),
           labels = c("No", "Yes")) )

# check
table(contest_nieuw7$busylocations_inside, contest_nieuw7$bijeenkomst_binnen_old_x, useNA = "always")

table(contest_nieuw7$busylocations_inside, contest_nieuw7$locaties_binnen, useNA = "always")

table(contest_nieuw7$busylocations_inside, useNA = "always")

table(contest_nieuw7$locaties_buiten_combined)

#locations outside
# busy locations outside
contest_nieuw7 <- contest_nieuw7 %>%
  mutate(busylocations_outside = case_when(
    locaties_buiten_combined == "Nee"  ~ 0,
    locaties_buiten_combined == "Weet ik niet" ~ 0,
    locaties_buiten_combined == "Ja"  ~ 1,
    TRUE ~ 0
  ) %>% factor(levels = c (0,1),
               labels = c( "No", "Yes")) )
# check
table(contest_nieuw7$busylocations_outside, contest_nieuw7$locaties_buiten_combined, useNA = "always")

#mouth masks
#WHO categories for wearing facemasks everywhere outside the house
contest_nieuw7 <- contest_nieuw7 %>%
  mutate(facemask_who_v1 = case_when(
    mond_overalbuitenshuis_x  == "Altijd"  ~ 0,
    mond_overalbuitenshuis_x  == "Meestal" ~ 0,
    mond_overalbuitenshuis_x  == "Regelmatig" ~ 0,
    mond_overalbuitenshuis_x  == "Soms"    ~ 1,
    mond_overalbuitenshuis_x  == "Zelden"  ~ 1,
    mond_overalbuitenshuis_x  == "Nooit"   ~ 1
  ) %>% factor(levels = c(0,1), labels = c ("Always/Mostly", "Sometimes/Rarely/Never")))


contest_nieuw7 <- contest_nieuw7 %>%
  mutate(facemask_who_v2 = case_when(
    mond_openbareruimtesbin_x == "Altijd"  ~ 0,
    mond_openbareruimtesbin_x == "Meestal" ~ 0,
    mond_openbareruimtesbin_x == "Regelmatig" ~ 0,
    mond_openbareruimtesbin_x == "Soms"    ~ 1,
    mond_openbareruimtesbin_x == "Zelden"  ~ 1,
    mond_openbareruimtesbin_x == "Nooit"   ~ 1
  ) %>% factor(levels = c(0,1), labels = c ("Always/Mostly",  "Sometimes/Rarely/Never")))


# making a variable for week of test
contest_nieuw7$week_test = week(ymd(contest_nieuw7$datum_test))
# check
table(contest_nieuw7$week_test)

########Filtering on vaccinated people####################

contest_vacc <- contest_nieuw7 %>%
  filter(contest_nieuw7$coronavaccinatie %in% c("3"))



######making extra variables for analyses####
#mutate result to get give negative 0 and positive 1
contest_vacc1 <- contest_vacc %>%
  mutate(uitslag_01 = case_when(
    uitslag == "negatief" ~ 0,
    uitslag == "positief" ~ 1)
    %>% factor(levels = c(0, 1), labels = c("Negative", "Positive")))

table (contest_vacc1$uitslag_01, contest_vacc1$uitslag)

#categorizing week of testing

contest_vacc1 <- contest_vacc1 %>%
  mutate.(week_test_01 = case_when.(
    week_test == 22 ~ 1,
    week_test == 23 ~ 2,
    week_test == 24 ~ 3,
    week_test == 25 ~ 4,
    week_test == 26 ~ 5,
    week_test == 27 ~ 6,
    week_test == 28 ~ 7,
    week_test == 29 ~ 8,
    week_test == 30 ~ 9,
    week_test == 31 ~ 10,
    week_test == 32 ~ 11,
    week_test == 33 ~ 12,
    week_test == 34 ~ 13,
    week_test == 35 ~ 14,
    week_test == 36 ~ 15,
    week_test == 37 ~ 16,
    week_test == 38 ~ 17,
    week_test == 39 ~ 18,
    week_test == 40 ~ 19,
    week_test == 41 ~ 20,
    week_test == 42 ~ 21,
    week_test == 43 ~ 22,
    week_test == 44 ~ 23,
    week_test == 45 ~ 24,
    week_test == 46 ~ 25,
    week_test == 47 ~ 26,
    week_test == 48 ~ 27,
    week_test == 49 ~ 28,
    week_test == 50 ~ 29,
    week_test == 51 ~ 30,
    week_test == 52 ~ 31,
    week_test == 1 ~ 32,
    week_test == 2 ~ 33,
    week_test == 3 ~ 34,
    week_test == 4 ~ 35,
    week_test == 5 ~ 36,
    week_test == 6 ~ 37,
    week_test == 7 ~ 38,
    week_test == 8 ~ 39,
    week_test == 9 ~ 40))


library(mgcv)




###univariate analyses adjutsed for age and sex

##household size
table (contest_vacc1$uitslag_01, contest_vacc1$totalhousehold_cat_new)
gam_model_household <- gam(uitslag_01 ~ totalhousehold_cat_new + agegroup + sex + s(week_test, k= 20) + dayssincevaccination, method= "REML", family = "binomial", data = contest_vacc1)
summary (gam_model_household)

#OR en CIs for householdcat 2-3
beta_hhcat23 <- coef(gam_model_household)["totalhousehold_cat_new2-3"]
Vb_hhcat23 <- vcov(gam_model_household)
se_hhcat23 <- sqrt(diag(Vb_hhcat23) ["totalhousehold_cat_new2-3"])

hh2_3 <- exp (beta_hhcat23) %>% round (digits = 2)


lwr_hhcat23 = (beta_hhcat23 - 1.96*se_hhcat23)
upp_hhcat23 = (beta_hhcat23 + 1.96*se_hhcat23)
lower_hh_2_3 <- exp (lwr_hhcat23) %>% round (digits = 2)
upper_hh_2_3 <- exp (upp_hhcat23) %>% round (digits = 2)

#OR en CIs for householdcat 4+
beta_hhcat4 <- coef(gam_model_household)["totalhousehold_cat_new4+"]
Vb_hhcat4 <- vcov(gam_model_household)
se_hhcat4 <- sqrt(diag(Vb_hhcat4) ["totalhousehold_cat_new4+"])

hh4 <- exp (beta_hhcat4) %>% round (digits = 2)

lwr_hhcat4 = (beta_hhcat4 - 1.96*se_hhcat4)
upp_hhcat4 = (beta_hhcat4 + 1.96*se_hhcat4)
lower_hh4 <- exp (lwr_hhcat4) %>% round (digits = 2)
upper_hh4 <- exp (upp_hhcat4) %>% round (digits = 2)

#living with children 0-12
table (contest_vacc1$uitslag_01, contest_vacc1$livingwithchildren_0_12)
gam_model_child_0_12 <- gam(uitslag_01 ~ livingwithchildren_0_12 + agegroup + sex+ s(week_test, k= 20)  + dayssincevaccination, method= "REML", family = "binomial", data = contest_vacc1)
summary (gam_model_child_0_12)

#OR en CIs for living with children 0-12
beta_child012 <- coef(gam_model_child_0_12)["livingwithchildren_0_12Living with children 0-12"]
Vb_child012 <- vcov(gam_model_child_0_12)
se_child012 <- sqrt(diag(Vb_child012) ["livingwithchildren_0_12Living with children 0-12"])

child0_12 <- exp (beta_child012) %>% round (digits = 2)

lwr_child012 = (beta_child012 - 1.96*se_child012)
upp_child012 = (beta_child012 + 1.96*se_child012)
lower_child0_12 <- exp (lwr_child012) %>% round (digits = 2)
upper_child0_12 <- exp (upp_child012) %>% round (digits = 2)

#living with children 13-18
table (contest_vacc1$uitslag_01, contest_vacc1$livingwithchildren_13_18)
gam_model_child_13_18 <- gam(uitslag_01 ~ livingwithchildren_13_18 + agegroup + sex + s(week_test, k= 20)  + dayssincevaccination, method= "REML", family = "binomial", data = contest_vacc1)
summary (gam_model_child_13_18)

#OR en CIs for living with children 13-18
beta_child1318 <- coef(gam_model_child_13_18)["livingwithchildren_13_18Living with children 13-18"]
Vb_child1318 <- vcov(gam_model_child_13_18)
se_child1318 <- sqrt(diag(Vb_child1318) ["livingwithchildren_13_18Living with children 13-18"])

child13_18 <- exp (beta_child1318) %>% round (digits = 2)

lwr_child1318 = (beta_child1318 - 1.96*se_child1318)
upp_child1318 = (beta_child1318 + 1.96*se_child1318)
lower_child13_18 <- exp (lwr_child1318) %>% round (digits = 2)
upper_child13_18 <- exp (upp_child1318) %>% round (digits = 2)

#healthcare workers (hcw)
table (contest_vacc1$uitslag_01, contest_vacc1$hcw_cat)
gam_model_hcw <- gam(uitslag_01 ~ hcw_cat + agegroup + sex + s(week_test, k= 20)  + dayssincevaccination, method= "REML", family = "binomial", data = contest_vacc1)
summary (gam_model_hcw)

#OR en CIs for HCW
beta_hcw <- coef(gam_model_hcw)["hcw_catYes"]
Vb_hcw <- vcov(gam_model_hcw)
se_hcw <- sqrt(diag(Vb_hcw) ["hcw_catYes"])

hcw <- exp (beta_hcw) %>% round (digits = 2)

lwr_hcw = (beta_hcw - 1.96*se_hcw)
upp_hcw = (beta_hcw + 1.96*se_hcw)
lower_hcw <- exp (lwr_hcw) %>% round (digits = 2)
upper_hcw <- exp (upp_hcw) %>% round (digits = 2)


#went to work/study
table (contest_vacc1$uitslag_01, contest_vacc1$went_to_work)
gam_model_work <- gam(uitslag_01 ~ went_to_work + agegroup + sex + s(week_test, k= 20)  + dayssincevaccination, method= "REML", family = "binomial", data = contest_vacc1)
summary (gam_model_work)

#OR en CIs for days went to work 1-5
beta_work15 <- coef(gam_model_work)["went_to_work1-5 days"]
Vb_work15 <- vcov(gam_model_work)
se_work15 <- sqrt(diag(Vb_work15) ["went_to_work1-5 days"])

work1_5 <- exp (beta_work15) %>% round (digits = 2)

lwr_work15 = (beta_work15 - 1.96*se_work15)
upp_work15 = (beta_work15 + 1.96*se_work15)
lower_work1_5 <- exp (lwr_work15) %>% round (digits = 2)
upper_work1_5 <- exp (upp_work15) %>% round (digits = 2)

#OR en CIs for days went to work 6-10
beta_work610 <- coef(gam_model_work)["went_to_work6-10 days"]
Vb_work610 <- vcov(gam_model_work)
se_work610 <- sqrt(diag(Vb_work610) ["went_to_work6-10 days"])

work6_10 <- exp (beta_work610) %>% round (digits = 2)

lwr_work610 = (beta_work610 - 1.96*se_work610)
upp_work610 = (beta_work610 + 1.96*se_work610)
lower_work6_10 <- exp (lwr_work610) %>% round (digits = 2)
upper_work6_10 <- exp (upp_work610) %>% round (digits = 2)


#OR en CIs for days went to work 10+
beta_work10 <- coef(gam_model_work)["went_to_work>10 days"]
Vb_work10 <- vcov(gam_model_work)
se_work10 <- sqrt(diag(Vb_work10) ["went_to_work>10 days"])

work10 <- exp (beta_work10) %>% round (digits = 2)

lwr_work10 = (beta_work10 - 1.96*se_work10)
upp_work10 = (beta_work10 + 1.96*se_work10)
lower_work10 <- exp (lwr_work10) %>% round (digits = 2)
upper_work10 <- exp (upp_work10) %>% round (digits = 2)

#OR en CIs for days went to work not applicable
beta_workNA <- coef(gam_model_work)["went_to_workNot applicable"]
Vb_workNA <- vcov(gam_model_work)
se_workNA <- sqrt(diag(Vb_workNA) ["went_to_workNot applicable"])

workNA <- exp (beta_workNA) %>% round (digits = 2)

lwr_workNA = (beta_workNA - 1.96*se_workNA)
upp_workNA = (beta_workNA + 1.96*se_workNA)
lower_workNA <- exp (lwr_workNA) %>% round (digits = 2)
upper_workNA <- exp (upp_workNA) %>% round (digits = 2)


##contacts at work or study
table (contest_vacc1$uitslag_01, contest_vacc1$work_study_contacts)
gam_model_contacts <- gam(uitslag_01 ~ work_study_contacts + agegroup + sex + s(week_test, k= 20)  + dayssincevaccination, method= "REML", family = "binomial", data = contest_vacc1)
summary (gam_model_contacts)
#OR en CIs for 0-4 contacts work/study
beta_con04 <- coef(gam_model_contacts)["work_study_contacts1-4 contacts"]
Vb_con04 <- vcov(gam_model_contacts)
se_con04 <- sqrt(diag(Vb_con04) ["work_study_contacts1-4 contacts"])

contacts0_4 <- exp (beta_con04) %>% round (digits = 2)

lwr_con04 = (beta_con04 - 1.96*se_con04)
upp_con04 = (beta_con04 + 1.96*se_con04)
lower_contacts0_4 <- exp (lwr_con04) %>% round (digits = 2)
upper_contacts0_4 <- exp (upp_con04) %>% round (digits = 2)

#OR en CIs for 5-9 contacts work/study
beta_con59 <- coef(gam_model_contacts)["work_study_contacts 5-9 contacts"]
Vb_con59 <- vcov(gam_model_contacts)
se_con59 <- sqrt(diag(Vb_con59) ["work_study_contacts 5-9 contacts"])

contacts5_9 <- exp (beta_con59) %>% round (digits = 2)

lwr_con59 = (beta_con59 - 1.96*se_con59)
upp_con59 = (beta_con59 + 1.96*se_con59)
lower_contacts5_9 <- exp (lwr_con59) %>% round (digits = 2)
upper_contacts5_9 <- exp (upp_con59) %>% round (digits = 2)


#OR en CIs for 10-19 contacts work/study
beta_con1019 <- coef(gam_model_contacts)["work_study_contacts10-19 contacts"]
Vb_con1019 <- vcov(gam_model_contacts)
se_con1019 <- sqrt(diag(Vb_con1019) ["work_study_contacts10-19 contacts"])

contacts10_19 <- exp (beta_con1019) %>% round (digits = 2)

lwr_con1019 = (beta_con1019 - 1.96*se_con1019)
upp_con1019 = (beta_con1019 + 1.96*se_con1019)
lower_contacts10_19 <- exp (lwr_con1019) %>% round (digits = 2)
upper_contacts10_19 <- exp (upp_con1019) %>% round (digits = 2)

#OR en CIs for 20+ contacts work/study
beta_con20 <- coef(gam_model_contacts)["work_study_contacts20 or more contacts"]
Vb_con20 <- vcov(gam_model_contacts)
se_con20 <- sqrt(diag(Vb_con20) ["work_study_contacts20 or more contacts"])

contacts20 <- exp (beta_con20) %>% round (digits = 2)

lwr_con20 = (beta_con20 - 1.96*se_con20)
upp_con20 = (beta_con20 + 1.96*se_con20)
lower_contacts20 <- exp (lwr_con20) %>% round (digits = 2)
upper_contacts20 <- exp (upp_con20) %>% round (digits = 2)


#visited busy indoor locations
table (contest_vacc1$uitslag_01, contest_vacc1$busylocations_inside)
gam_model_inside <- gam(uitslag_01 ~ busylocations_inside + agegroup + sex + s(week_test, k= 20)  + dayssincevaccination, method= "REML", family = "binomial", data = contest_vacc1)
summary (gam_model_inside)

#OR en CIs for busy locations inside
beta_inside <- coef(gam_model_inside)["busylocations_insideYes"]
Vb_inside <- vcov(gam_model_inside)
se_inside <- sqrt(diag(Vb_inside) ["busylocations_insideYes"])

inside <- exp (beta_inside) %>% round (digits = 2)

lwr_inside = (beta_inside - 1.96*se_inside)
upp_inside = (beta_inside + 1.96*se_inside)
lower_inside <- exp (lwr_inside) %>% round (digits = 2)
upper_inside <- exp (upp_inside) %>% round (digits = 2)

#visited busy outdoor locations
table (contest_vacc1$uitslag_01, contest_vacc1$busylocations_outside)
gam_model_outside <- gam(uitslag_01 ~ busylocations_outside + agegroup + sex + s(week_test, k= 20)  + dayssincevaccination, method= "REML", family = "binomial", data = contest_vacc1)
summary (gam_model_outside)

#OR en CIs for busy locations outside
beta_outside <- coef(gam_model_outside)["busylocations_outsideYes"]
Vb_outside <- vcov(gam_model_outside)
se_outside <- sqrt(diag(Vb_outside) ["busylocations_outsideYes"])

outside <- exp (beta_outside) %>% round (digits = 2)

lwr_outside = (beta_outside - 1.96*se_outside)
upp_outside = (beta_outside + 1.96*se_outside)
lower_outside <- exp (lwr_outside) %>% round (digits = 2)
upper_outside <- exp (upp_outside) %>% round (digits = 2)

#facemask everywhere outside the house 
table (contest_vacc1$uitslag_01, contest_vacc1$facemask_who_v1)
gam_model_mask1 <- gam(uitslag_01 ~ facemask_who_v1 + agegroup + sex + s(week_test, k= 20) + dayssincevaccination, method= "REML", family = "binomial", data = contest_vacc1)
summary (gam_model_mask1)

#OR en CIs for sometimes/rarely/never facemask wearing outside the house
beta_masksomerarenever <- coef(gam_model_mask1)["facemask_who_v1Sometimes/Rarely/Never"]
Vb_masksomerarenever <- vcov(gam_model_mask1)
se_masksomerarenever <- sqrt(diag(Vb_masksomerarenever) ["facemask_who_v1Sometimes/Rarely/Never"])

somerarenever_outside <- exp (beta_masksomerarenever) %>% round (digits = 2)

lwr_masksomerarenever = (beta_masksomerarenever - 1.96*se_masksomerarenever)
upp_masksomerarenever = (beta_masksomerarenever + 1.96*se_masksomerarenever)
lower_somerarenever_outside <- exp (lwr_masksomerarenever) %>% round (digits = 2)
upper_somerarenever_outside <- exp (upp_masksomerarenever) %>% round (digits = 2)


#facemask everywhere inside public spaces
table (contest_vacc1$uitslag_01, contest_vacc1$facemask_who_v2)
gam_model_mask2 <- gam(uitslag_01 ~ facemask_who_v2 + agegroup + sex + s(week_test, k= 20) + dayssincevaccination, method= "REML", family = "binomial", data = contest_vacc1)
summary (gam_model_mask2)


#OR en CIs for sometimes/rarely/never facemask wearing everwhere inside/public spaces
beta_facesomerarenever2 <- coef(gam_model_mask2)["facemask_who_v2Sometimes/Rarely/Never"]
Vb_facesomerarenever2 <- vcov(gam_model_mask2)
se_facesomerarenever2 <- sqrt(diag(Vb_facesomerarenever2) ["facemask_who_v2Sometimes/Rarely/Never"])

somerarenever_inside <- exp (beta_facesomerarenever2) %>% round (digits = 2)

lwr_facesomerarenever2 = (beta_facesomerarenever2 - 1.96*se_facesomerarenever2)
upp_facesomerarenever2 = (beta_facesomerarenever2 + 1.96*se_facesomerarenever2)
lower_somerarenever_inside <- exp (lwr_facesomerarenever2) %>% round (digits = 2)
upper_somerarenever_inside <- exp (upp_facesomerarenever2) %>% round (digits = 2)









######Symptomatic cases & asymptomatic controls########


##loading data for symptomatic cases vs asymptomatic controls
a_contest <- "Data/Cleaned/Contest_data_cleaned_20220406_1030.rds" %>%
  readRDS()

######filtering for population for analyses########
#filtering on time period 1-6-2021 t/m 31-12-2021
a_contest <- a_contest %>%
  filter(dateentry >= as.Date("2021-06-01")) %>%
  filter(dateentry <= as.Date("2022-02-28"))


a_contest_nieuw <- a_contest



#filtering on asymptomatic people


table (a_contest_nieuw$symptomatic)

sum(is.na(a_contest_nieuw$symptomatic))


a_contest_nieuw1<- a_contest_nieuw%>%
  filter  (symptomatic %in% c("0"))

#filtering on fully vaccinated and unvaccinated
table (a_contest_nieuw1$vaccinatiestatus)
sum(is.na(a_contest_nieuw1$vaccinatiestatus))

a_contest_nieuw3 <- a_contest_nieuw1 %>%
  filter  (vaccinatiestatus %in% c("Fully vaccinated", "Unvaccinated") | is.na(vaccinatiestatus))




#filtering on a maximum of two doses
table (a_contest_nieuw3$coronavaccinatie_hoeveel_combined)
sum(is.na(a_contest_nieuw3$coronavaccinatie_hoeveel_combined))

a_contest_nieuw4 <- a_contest_nieuw3%>%
  filter  (coronavaccinatie_hoeveel_combined %in% c ("1 dose", "2 doses") | is.na(coronavaccinatie_hoeveel_combined))



#filtering on people who did not contract SARS-CoV-2 from a positive tested household member
table (a_contest_nieuw4$contactpersooncorona_waar)
sum(is.na(a_contest_nieuw4$contactpersooncorona_waar))


a_contest_nieuw5 <- a_contest_nieuw4 %>%
  filter (contactpersooncorona_waar %in% c ("0", "1", "3", "4", "5", "6", "7"))

#filtering on people who did not have a positive self-administered selftest
table (a_contest_nieuw5$reden_testen)
sum(is.na(a_contest_nieuw5$reden_testen))


a_contest_nieuw6 <- a_contest_nieuw5%>%
  filter (reden_testen %in% c("1", "1;2", "1;3", "1;5", "1;7", "2", "2;3", "2;3;4", "2;4", "2;4;5", "2;4;7", "2;5", "2;5;7", "2;7", "3", "3;4", "3;5", "4", "4;5", "4;7", "5", "5;7", "7")| is.na(reden_testen))

#crosstable vaccinated and unvaccinated and test result
table (a_contest_nieuw6$coronavaccinatie_x, a_contest_nieuw6$uitslag)


#checking on positive self-administered selftest in open question reason for testing
a_contest_nieuw_select <- select (a_contest_nieuw6, reden_testen_and)

#view(a_contest_nieuw_select)



#filtering on positive self-administered selftest in open question reason of testing
a_contest_nieuw7 <- a_contest_nieuw6[-c(29, 114, 141, 282, 312, 364, 413, 426, 1413), ]

#checking
a_contest_nieuw_select2 <- select (a_contest_nieuw7, reden_testen_and)

#view(a_contest_nieuw_select2)




#crosstable gevaccineerd en ongevaccineerd en testuitslag
table (a_contest_nieuw7$coronavaccinatie_x, a_contest_nieuw7$uitslag)


#Filtering on vaccinated people

a_contest_vacc <- a_contest_nieuw7 %>%
  filter(a_contest_nieuw7$coronavaccinatie %in% c("3"))

table (a_contest_vacc$uitslag)


#table (a_contest_vacc_asymp_neg$uitslag)

#######making dataset for symptomatic cases and asymptomatic controls######

#merging of the symptomatic and asymptomatic dataset
merged_case_control <- rbind(contest_vacc, a_contest_vacc, fill=TRUE)

#making variable on test result and being symptomatic or asymptomatic
table (merged_case_control$uitslag, merged_case_control$symptomatic)

merged_case_control_1 <- merged_case_control %>%
  mutate(symp_uitslag = case_when(
    symptomatic == 0 & uitslag == "positief" ~ "asymptomatic en positive",
    symptomatic == 0 & uitslag == "negatief" ~ "asymptomatic en negative",
    symptomatic == 1 & uitslag == "positief" ~ "symptomatic and positive",
    symptomatic == 1 & uitslag == "negatief" ~ "symptomatic and negative"))

table (merged_case_control_1$symptomatic, merged_case_control_1$uitslag)
table (merged_case_control_1$symp_uitslag)


#filtering to have a dataset with symptomatic cases and asymptomatic controls
merged_case_control_analyse <- merged_case_control_1 %>%
  filter(merged_case_control_1$symp_uitslag %in% c("asymptomatic en negative", "symptomatic and positive"))

#checking
table (merged_case_control_analyse$symp_uitslag)



####Data management and data cleaning#######
#gender
merged_case_control_analyse <- merged_case_control_analyse %>% mutate(
  sex = case_when(
    geslacht_x == "Man"  ~ 1,
    geslacht_x == "Vrouw" ~ 0 # removing Other since that is too small a number
  ) %>% as_factor())

merged_case_control_analyse$sex <- factor(merged_case_control_analyse$sex,
                                          levels = c(0,1),
                                          labels = c("Female", "Male"))

table(merged_case_control_analyse$sex)


# educational level
# dropping Other since too small a number for glm later on

merged_case_control_analyse <- merged_case_control_analyse %>%
  mutate(educationlevel_new = case_when(
    educationlevel_new == "Low"  ~ 1,
    educationlevel_new == "Middle" ~ 2,
    educationlevel_new == "High"  ~ 3
  ) %>% factor(levels = c(1,2,3), labels = c( "Low", "Middle", "High")))

table(merged_case_control_analyse$educationlevel_new, useNA = "always")

# agegroup
class(merged_case_control_analyse$agegroup)
table(merged_case_control_analyse$agegroup, useNA = "always")

merged_case_control_analyse <- merged_case_control_analyse %>%  mutate(agegroup = case_when(
  agegroup == "18-29"  ~ 1,
  agegroup == "30-44"  ~ 2,
  agegroup == "45-59"  ~ 3,
  agegroup == "60-69"  ~ 4,
  agegroup == "70+"    ~ 5,
))


mean(merged_case_control_analyse$age)
median(merged_case_control_analyse$age)

merged_case_control_analyse $agegroup <- factor(merged_case_control_analyse$agegroup,
                                                levels = c(1,2,3,4,5),
                                                labels = c( "18-29", "30-44", "45-59", "60-69", "70+"))

class(merged_case_control_analyse$agegroup)

table(merged_case_control_analyse$age)


#household size
#totalhousehold_cat_new gebruiken


#Living with children (0-12)
merged_case_control_analyse<- merged_case_control_analyse %>%
  mutate(livingwithchildren_0_12 = case_when(
    mensensamen_jonger4 >= "1" | mensensamen_4_12 >= "1" ~ 1,
    huishouden_hoeveel < "1" ~ 0,
    TRUE ~ 0
  ) %>% factor(levels = c(0,1),
               labels = c("Not living with children 0-12", "Living with children 0-12")))

table(merged_case_control_analyse$livingwithchildren_0_12)


#Living with children (13-18)
merged_case_control_analyse <- merged_case_control_analyse %>%
  mutate(livingwithchildren_13_18 = case_when(
    mensensamen_13_18 >= "1" ~ 1,
    huishouden_hoeveel < "1" ~ 0,
    TRUE ~ 0
  ) %>% factor(levels = c(0,1),
               labels = c("Not living with children 13-18", "Living with children 13-18")))

table(merged_case_control_analyse$livingwithchildren_13_18)



#work environment
#went to work in the previous 14 days

merged_case_control_analyse <- merged_case_control_analyse %>%
  mutate(went_to_work = case_when (
    werk_gegaan_x == "Nooit" ~ "0",
    werk_gegaan_x == "1-2 dagen" ~ "1",
    werk_gegaan_x == "3-5 dagen" ~ "1",
    werk_gegaan_x == "6-8 dagen" ~ "2",
    werk_gegaan_x == "9-10 dagen" ~ "2",
    werk_gegaan_x == "Meer dan 10 dagen" ~ "3",
    werk_gegaan_x == "Niet van toepassing" ~ "4",
    TRUE ~ "4"))



merged_case_control_analyse$went_to_work <- factor(merged_case_control_analyse$went_to_work,
                                                   levels = c(0,1,2,3,4),
                                                   labels = c("Did not go to work", "1-5 days", "6-10 days", ">10 days", "Not applicable"))



#Work re-categorisation
merged_case_control_analyse <- merged_case_control_analyse %>%
  mutate(werk_welkesector_cat_old = case_when (
    werk_welkesector_old_x == "Gezondheidszorg met contact met patienten" ~ "HCW contact with patients",
    werk_welkesector_old_x == "Gezondheidszorg zonder contact met pati?nten" ~ "HCW no contact with patients",
    werk_welkesector_old_x == "Sociaal of maatschappelijk werker" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Mantelzorger" ~ "Contact profession",
    werk_welkesector_old_x == "Onderwijs en kinderopvang" ~ "Daycare - Education",
    werk_welkesector_old_x == "Openbaar vervoer" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Politie, marechaussee, brandweer, Dienst Justiti?le Inrichtingen, of BOA" ~ "Contact profession",
    werk_welkesector_old_x == "Transport (luchtvaart, haven, wegtansport, scheepvaart)" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Afvalverwerking" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Vleesverwerkende industrie (zoals een slachthuis)" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Landbouw (o.a. fruit- en groenteteelt, kwekerijen en glastuinbouw)" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Supermarkt medewerker" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Winkelmedewerker (anders dan supermarkt)" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Schoonmaakbranche" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Kapster, schoonheidsspecialist, manicure of pedicure" ~ "Contact profession",
    werk_welkesector_old_x == "Fabrieksmedewerker" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Medewerker in pretpark dierentuin of speeltuin" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Horeca (restaurant, caf?, bar etc.)" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Sportinstructeur (personal) trainer of scheidsrechter" ~ "Other profession (not possible remote)",
    werk_welkesector_old_x == "Rij-instructeur" ~ "Contact profession",
    werk_welkesector_old_x == "Sekswerker" ~ "Contact profession",
    werk_welkesector_old_x == "Kantoorfunctie" ~ "Other profession (likely remote)",
    werk_welkesector_old_x == "Anders" ~ "Other",
    werk_x == "Nee"  ~ "Does not work",
    TRUE ~ "NA"))


#(7 categories)
merged_case_control_analyse$werk_welkesector_cat_old<- factor(merged_case_control_analyse$werk_welkesector_cat_old, levels=c("Other",
                                                                                                                             "HCW contact with patients",
                                                                                                                             "HCW no contact with patients",
                                                                                                                             "Daycare - Education",
                                                                                                                             "Contact profession",
                                                                                                                             "Other profession (not possible remote)",
                                                                                                                             "Other profession (likely remote)",
                                                                                                                             "Does not work"))




#only taking both HCW categories
merged_case_control_analyse <- merged_case_control_analyse %>%  mutate(hcw_other = case_when(
  werksector_no1 == "Gezondheidszorg zonder patiëntencontact of met patientencontact maar op meer dan 1.5m afstand" ~ 1,
  werksector_no2 == "Gezondheidszorg zonder patiëntencontact of met patientencontact maar op meer dan 1.5m afstand" ~ 1,
  werksector_no3 == "Gezondheidszorg zonder patiëntencontact of met patientencontact maar op meer dan 1.5m afstand" ~ 1,
  werk_welkesector_old_x == "Gezondheidszorg zonder contact met pati?nten"  ~ 1,
  TRUE   ~ 0 
))

merged_case_control_analyse$hcw_other <-factor(merged_case_control_analyse$hcw_other,
                                               levels = c(0,1),
                                               labels = c("No", "Yes"))
merged_case_control_analyse <- merged_case_control_analyse %>%  mutate(hcw_cat = case_when(
  hcw =="Yes"        ~ 1,
  hcw_other == "Yes" ~ 1,
  TRUE               ~ 0 
))

merged_case_control_analyse$hcw_cat <- factor(merged_case_control_analyse$hcw_cat,
                                              levels = c(0,1),
                                              labels = c( "No", "Yes"))

# contact work/study

# trying to combine all into one
merged_case_control_analyse <- merged_case_control_analyse %>% mutate(work_study_contacts = case_when(
  # you don't work and are not a student
  werk_gegaan_contact_x == "Nee" & student_gegaan_old_x == "Nee"      ~ 0,    # did not have close contact at work or universitiy
  werk_gegaan_contactmensen_x == "1-4 mensen"             ~ 1 ,           # you went to work and had close contact with 1-4 people
  student_gegaan_hoeveelmen_old_x == "1-4 mensen"         ~ 1,                # you went to study and had close contact with 1-4 people
  werk_gegaan_contactmensen_x == "5-9 mensen"             ~ 2,
  student_gegaan_hoeveelmen_old_x == "5-9 mensen"         ~ 2 ,
  werk_gegaan_contactmensen_x == "10-19 mensen"           ~ 3,
  student_gegaan_hoeveelmen_old_x == "10-19 mensen"       ~ 3 ,
  werk_gegaan_contactmensen_x == "20 of meer mensen"      ~ 4 ,
  student_gegaan_hoeveelmen_old_x == "20 of meer mensen"   ~ 4,
  TRUE                                                       ~ 0
) %>% factor( levels = c( 0, 1,2,3,4),
              labels = c( "zero contacts" , "1-4 contacts", " 5-9 contacts", "10-19 contacts", "20 or more contacts")) )


#locations inside
# busy locations insides
merged_case_control_analyse<- merged_case_control_analyse %>% mutate(busylocations_inside = case_when(
  bijeenkomst_binnen_old_x == "Nee"               ~ 0 , # not been to busy locations inside
  locaties_binnen == 2                            ~ 0 , # not been to busy locations inside
  locaties_binnen == 3                            ~ 1 , # been to busy location inside new variable
  bijeenkomst_binnen_old_x == "Ja" &
    (drukkelocatie_andere1_af_old_x == "Nee" |
       drukkelocatie_andere2_af_old_x == "Nee" |
       drukkelocatie_andere3_af_old_x == "Nee" |
       drukkelocatie_kerk_afs_old_x == "Nee" |
       drukkelocatie_feest_afs_old_x == "Nee" )        ~ 1, # been to a location inside (old variable) and could not keep 1.5 m distance
  TRUE       ~  0 )  %>%
    factor(levels = c(0,1),
           labels = c("No", "Yes")) )

# check
table(merged_case_control_analyse$busylocations_inside, merged_case_control_analyse$bijeenkomst_binnen_old_x, useNA = "always")

table(merged_case_control_analyse$busylocations_inside, merged_case_control_analyse$locaties_binnen, useNA = "always")

table(merged_case_control_analyse$busylocations_inside, useNA = "always")

table(merged_case_control_analyse$locaties_buiten_combined)

#locations outside
# busy locations outside
merged_case_control_analyse <- merged_case_control_analyse %>%
  mutate(busylocations_outside = case_when(
    locaties_buiten_combined == "Nee"  ~ 0,
    locaties_buiten_combined == "Weet ik niet" ~ 0,
    locaties_buiten_combined == "Ja"  ~ 1,
    TRUE ~ 0
  ) %>% factor(levels = c (0,1),
               labels = c( "No", "Yes")) )
# check
table(merged_case_control_analyse$busylocations_outside, merged_case_control_analyse$locaties_buiten_combined, useNA = "always")

#mouth masks
#WHO categories for wearing facemasks everywhere outside the house
merged_case_control_analyse <- merged_case_control_analyse %>%
  mutate(facemask_who_v1 = case_when(
    mond_overalbuitenshuis_x  == "Altijd"  ~ 0,
    mond_overalbuitenshuis_x  == "Meestal" ~ 0,
    mond_overalbuitenshuis_x  == "Regelmatig" ~ 0,
    mond_overalbuitenshuis_x  == "Soms"    ~ 1,
    mond_overalbuitenshuis_x  == "Zelden"  ~ 1,
    mond_overalbuitenshuis_x  == "Nooit"   ~ 1
  ) %>% factor(levels = c(0,1), labels = c ("Always/Mostly", "Sometimes/Rarely/Never")))


merged_case_control_analyse <- merged_case_control_analyse %>%
  mutate(facemask_who_v2 = case_when(
    mond_openbareruimtesbin_x == "Altijd"  ~ 0,
    mond_openbareruimtesbin_x == "Meestal" ~ 0,
    mond_openbareruimtesbin_x == "Regelmatig" ~ 0,
    mond_openbareruimtesbin_x == "Soms"    ~ 1,
    mond_openbareruimtesbin_x == "Zelden"  ~ 1,
    mond_openbareruimtesbin_x == "Nooit"   ~ 1
  ) %>% factor(levels = c(0,1), labels = c ("Always/Mostly",  "Sometimes/Rarely/Never")))

#controleren op hoeveelheid NAs
sum(is.na(merged_case_control_analyse$facemask_who_v1))

sum(is.na(merged_case_control_analyse$facemask_who_v2))

# making a variable for week of test
merged_case_control_analyse$week_test = week(ymd(merged_case_control_analyse$datum_test))
# check
table(merged_case_control_analyse$week_test)

####making extra variables for analyses#####
#mutate result to get 0 for negative and 1 for positive
merged_case_control_analyse <- 
  contest_vacc1 <- merged_case_control_analyse %>%
  mutate(uitslag_01 = case_when(
    uitslag == "negatief" ~ 0,
    uitslag == "positief" ~ 1)
    %>% factor(levels = c(0, 1), labels = c("Negative", "Positive")))

table (merged_case_control_analyse$uitslag_01, merged_case_control_analyse$uitslag)

#categorising of week of testing

merged_case_control_analyse <- merged_case_control_analyse %>%
  mutate.(week_test_01 = case_when.(
    week_test == 22 ~ 1,
    week_test == 23 ~ 2,
    week_test == 24 ~ 3,
    week_test == 25 ~ 4,
    week_test == 26 ~ 5,
    week_test == 27 ~ 6,
    week_test == 28 ~ 7,
    week_test == 29 ~ 8,
    week_test == 30 ~ 9,
    week_test == 31 ~ 10,
    week_test == 32 ~ 11,
    week_test == 33 ~ 12,
    week_test == 34 ~ 13,
    week_test == 35 ~ 14,
    week_test == 36 ~ 15,
    week_test == 37 ~ 16,
    week_test == 38 ~ 17,
    week_test == 39 ~ 18,
    week_test == 40 ~ 19,
    week_test == 41 ~ 20,
    week_test == 42 ~ 21,
    week_test == 43 ~ 22,
    week_test == 44 ~ 23,
    week_test == 45 ~ 24,
    week_test == 46 ~ 25,
    week_test == 47 ~ 26,
    week_test == 48 ~ 27,
    week_test == 49 ~ 28,
    week_test == 50 ~ 29,
    week_test == 51 ~ 30,
    week_test == 52 ~ 31,
    week_test == 1 ~ 32,
    week_test == 2 ~ 33,
    week_test == 3 ~ 34,
    week_test == 4 ~ 35,
    week_test == 5 ~ 36,
    week_test == 6 ~ 37,
    week_test == 7 ~ 38,
    week_test == 8 ~ 39,
    week_test == 9 ~ 40))

#spline package aanzetten
library(mgcv)



###univariate analyses adjutsed for age and sex

##household size
table (merged_case_control_analyse$uitslag_01, merged_case_control_analyse$totalhousehold_cat_new)
gam_model_household <- gam(uitslag_01 ~ totalhousehold_cat_new + agegroup + sex + s(week_test, k= 20)  + dayssincevaccination, method= "REML", family = "binomial", data = merged_case_control_analyse)
summary (gam_model_household)

#OR en CIs for householdcat 2-3
beta_hhcat23_a <- coef(gam_model_household)["totalhousehold_cat_new2-3"]
Vb_hhcat23_a <- vcov(gam_model_household)
se_hhcat23_a <- sqrt(diag(Vb_hhcat23_a) ["totalhousehold_cat_new2-3"])

a_hh2_3 <- exp (beta_hhcat23_a) %>% round (digits = 2)

lwr_hhcat23_a = (beta_hhcat23_a - 1.96*se_hhcat23_a)
upp_hhcat23_a = (beta_hhcat23_a + 1.96*se_hhcat23_a)
a_lower_hh_2_3 <- exp (lwr_hhcat23_a) %>% round (digits = 2)
a_upper_hh_2_3 <- exp (upp_hhcat23_a) %>% round (digits = 2)

#OR en CIs for householdcat 4+
beta_hhcat4_a <- coef(gam_model_household)["totalhousehold_cat_new4+"]
Vb_hhcat4_a <- vcov(gam_model_household)
se_hhcat4_a <- sqrt(diag(Vb_hhcat4_a) ["totalhousehold_cat_new4+"])

a_hh4 <- exp (beta_hhcat4_a) %>% round (digits = 2)

lwr_hhcat4_a = (beta_hhcat4_a - 1.96*se_hhcat4_a)
upp_hhcat4_a = (beta_hhcat4_a + 1.96*se_hhcat4_a)
a_lower_hh4 <- exp (lwr_hhcat4_a) %>% round (digits = 2)
a_upper_hh4 <- exp (upp_hhcat4_a) %>% round (digits = 2)

#living with children 0-12
table (merged_case_control_analyse$uitslag_01, merged_case_control_analyse$livingwithchildren_0_12)
gam_model_child_0_12 <- gam(uitslag_01 ~ livingwithchildren_0_12 + agegroup + sex+ s(week_test, k= 20)  + dayssincevaccination, method= "REML", family = "binomial", data = merged_case_control_analyse)
summary (gam_model_child_0_12)

#OR en CIs for living with children 0-12
beta_child012_a <- coef(gam_model_child_0_12)["livingwithchildren_0_12Living with children 0-12"]
Vb_child012_a <- vcov(gam_model_child_0_12)
se_child012_a <- sqrt(diag(Vb_child012_a) ["livingwithchildren_0_12Living with children 0-12"])

a_child0_12 <- exp (beta_child012_a) %>% round (digits = 2)

lwr_child012_a = (beta_child012_a - 1.96*se_child012_a)
upp_child012_a = (beta_child012_a + 1.96*se_child012_a)
a_lower_child0_12 <- exp (lwr_child012_a) %>% round (digits = 2)
a_upper_child0_12 <- exp (upp_child012_a) %>% round (digits = 2)

#living with children 13-18
table (merged_case_control_analyse$uitslag_01, merged_case_control_analyse$livingwithchildren_13_18)
gam_model_child_13_18 <- gam(uitslag_01 ~ livingwithchildren_13_18 + agegroup + sex + s(week_test, k= 20)  + dayssincevaccination, method= "REML", family = "binomial", data = merged_case_control_analyse)
summary (gam_model_child_13_18)

#OR en CIs for living with children 13-18
beta_child1318_a <- coef(gam_model_child_13_18)["livingwithchildren_13_18Living with children 13-18"]
Vb_child1318_a <- vcov(gam_model_child_13_18)
se_child1318_a <- sqrt(diag(Vb_child1318_a) ["livingwithchildren_13_18Living with children 13-18"])

a_child13_18 <- exp (beta_child1318_a) %>% round (digits = 2)

lwr_child1318_a = (beta_child1318_a - 1.96*se_child1318_a)
upp_child1318_a = (beta_child1318_a + 1.96*se_child1318_a)
a_lower_child13_18 <- exp (lwr_child1318_a) %>% round (digits = 2)
a_upper_child13_18 <- exp (upp_child1318_a) %>% round (digits = 2)

#healthcare workers (hcw)
table (merged_case_control_analyse$uitslag_01, merged_case_control_analyse$hcw_cat)
gam_model_hcw <- gam(uitslag_01 ~ hcw_cat + agegroup + sex + s(week_test, k= 20)  + dayssincevaccination, method= "REML", family = "binomial", data = merged_case_control_analyse)
summary (gam_model_hcw)

#OR en CIs for HCW
beta_hcw_a <- coef(gam_model_hcw)["hcw_catYes"]
Vb_hcw_a <- vcov(gam_model_hcw)
se_hcw_a <- sqrt(diag(Vb_hcw_a) ["hcw_catYes"])

a_hcw <- exp (beta_hcw_a) %>% round (digits = 2)

lwr_hcw_a = (beta_hcw_a - 1.96*se_hcw_a)
upp_hcw_a = (beta_hcw_a + 1.96*se_hcw_a)
a_lower_hcw <- exp (lwr_hcw_a) %>% round (digits = 2)
a_upper_hcw <- exp (upp_hcw_a) %>% round (digits = 2)

#went to work/study
table (merged_case_control_analyse$uitslag_01, merged_case_control_analyse$went_to_work)
gam_model_work <- gam(uitslag_01 ~ went_to_work + agegroup + sex + s(week_test, k= 20)  + dayssincevaccination, method= "REML", family = "binomial", data = merged_case_control_analyse)
summary (gam_model_work)

#OR en CIs for days went to work 1-5
beta_work15_a <- coef(gam_model_work)["went_to_work1-5 days"]
Vb_work15_a <- vcov(gam_model_work)
se_work15_a <- sqrt(diag(Vb_work15_a) ["went_to_work1-5 days"])

a_work1_5 <- exp (beta_work15_a) %>% round (digits = 2)

lwr_work15_a = (beta_work15_a - 1.96*se_work15_a)
upp_work15_a = (beta_work15_a + 1.96*se_work15_a)
a_lower_work1_5 <- exp (lwr_work15_a) %>% round (digits = 2)
a_upper_work1_5 <- exp (upp_work15_a) %>% round (digits = 2)

#OR en CIs for days went to work 6-10
beta_work610_a <- coef(gam_model_work)["went_to_work6-10 days"]
Vb_work610_a <- vcov(gam_model_work)
se_work610_a <- sqrt(diag(Vb_work610_a) ["went_to_work6-10 days"])

a_work6_10 <- exp (beta_work610_a) %>% round (digits = 2)

lwr_work610_a = (beta_work610_a - 1.96*se_work610_a)
upp_work610_a = (beta_work610_a + 1.96*se_work610_a)
a_lower_work6_10 <- exp (lwr_work610_a) %>% round (digits = 2)
a_upper_work6_10 <- exp (upp_work610_a) %>% round (digits = 2)


#OR en CIs for days went to work 10+
beta_work10_a <- coef(gam_model_work)["went_to_work>10 days"]
Vb_work10_a <- vcov(gam_model_work)
se_work10_a <- sqrt(diag(Vb_work10_a) ["went_to_work>10 days"])

a_work10 <- exp (beta_work10_a) %>% round (digits = 2)

lwr_work10_a = (beta_work10_a - 1.96*se_work10_a)
upp_work10_a = (beta_work10_a + 1.96*se_work10_a)
a_lower_work10 <- exp (lwr_work10_a) %>% round (digits = 2)
a_upper_work10 <- exp (upp_work10_a) %>% round (digits = 2)

#OR en CIs for days went to work not applicable
beta_workNA_a <- coef(gam_model_work)["went_to_workNot applicable"]
Vb_workNA_a <- vcov(gam_model_work)
se_workNA_a <- sqrt(diag(Vb_workNA_a) ["went_to_workNot applicable"])

a_workNA <- exp (beta_workNA) %>% round (digits = 2)

lwr_workNA_a = (beta_workNA_a - 1.96*se_workNA_a)
upp_workNA_a = (beta_workNA_a + 1.96*se_workNA_a)
a_lower_workNA <- exp (lwr_workNA_a) %>% round (digits = 2)
a_upper_workNA <- exp (upp_workNA_a) %>% round (digits = 2)



##contacts at work or study
table (merged_case_control_analyse$uitslag_01, merged_case_control_analyse$work_study_contacts)
gam_model_contacts <- gam(uitslag_01 ~ work_study_contacts + agegroup + sex + s(week_test, k= 20)  + dayssincevaccination, method= "REML", family = "binomial", data = merged_case_control_analyse)
summary (gam_model_contacts)
#OR en CIs for 0-4 contacts work/study
beta_con04_a <- coef(gam_model_contacts)["work_study_contacts1-4 contacts"]
Vb_con04_a <- vcov(gam_model_contacts)
se_con04_a <- sqrt(diag(Vb_con04_a) ["work_study_contacts1-4 contacts"])

a_contacts0_4 <- exp (beta_con04_a) %>% round (digits = 2)

lwr_con04_a = (beta_con04_a - 1.96*se_con04_a)
upp_con04_a = (beta_con04_a + 1.96*se_con04_a)
a_lower_contacts0_4 <- exp (lwr_con04_a) %>% round (digits = 2)
a_upper_contacts0_4 <- exp (upp_con04_a) %>% round (digits = 2)


#OR en CIs for 5-9 contacts work/study
beta_con59_a <- coef(gam_model_contacts)["work_study_contacts 5-9 contacts"]
Vb_con59_a <- vcov(gam_model_contacts)
se_con59_a <- sqrt(diag(Vb_con59_a) ["work_study_contacts 5-9 contacts"])

a_contacts5_9 <- exp (beta_con59_a) %>% round (digits = 2)

lwr_con59_a = (beta_con59_a - 1.96*se_con59_a)
upp_con59_a = (beta_con59_a + 1.96*se_con59_a)
a_lower_contacts5_9 <- exp (lwr_con59_a) %>% round (digits = 2)
a_upper_contacts5_9 <- exp (upp_con59_a) %>% round (digits = 2)


#OR en CIs for 10-19 contacts work/study
beta_con1019_a <- coef(gam_model_contacts)["work_study_contacts10-19 contacts"]
Vb_con1019_a <- vcov(gam_model_contacts)
se_con1019_a <- sqrt(diag(Vb_con1019_a) ["work_study_contacts10-19 contacts"])

a_contacts10_19 <- exp (beta_con1019_a) %>% round (digits = 2)

lwr_con1019_a = (beta_con1019_a - 1.96*se_con1019_a)
upp_con1019_a = (beta_con1019_a + 1.96*se_con1019_a)
a_lower_contacts10_19 <- exp (lwr_con1019_a) %>% round (digits = 2)
a_upper_contacts10_19 <- exp (upp_con1019_a) %>% round (digits = 2)

#OR en CIs for 20+ contacts work/study
beta_con20_a <- coef(gam_model_contacts)["work_study_contacts20 or more contacts"]
Vb_con20_a <- vcov(gam_model_contacts)
se_con20_a <- sqrt(diag(Vb_con20_a) ["work_study_contacts20 or more contacts"])

a_contacts20 <- exp (beta_con20_a) %>% round (digits = 2)

lwr_con20_a = (beta_con20_a - 1.96*se_con20_a)
upp_con20_a = (beta_con20_a + 1.96*se_con20_a)
a_lower_contacts20 <- exp (lwr_con20_a) %>% round (digits = 2)
a_upper_contacts20 <- exp (upp_con20_a) %>% round (digits = 2)


#visited busy indoor locations
table (merged_case_control_analyse$uitslag_01, merged_case_control_analyse$busylocations_inside)
gam_model_inside <- gam(uitslag_01 ~ busylocations_inside + agegroup + sex + s(week_test, k= 20)  + dayssincevaccination, method= "REML", family = "binomial", data = merged_case_control_analyse)
summary (gam_model_inside)

#OR en CIs for busy locations inside
beta_inside_a <- coef(gam_model_inside)["busylocations_insideYes"]
Vb_inside_a <- vcov(gam_model_inside)
se_inside_a <- sqrt(diag(Vb_inside_a) ["busylocations_insideYes"])

a_inside <- exp (beta_inside_a) %>% round (digits = 2)

lwr_inside_a = (beta_inside_a - 1.96*se_inside_a)
upp_inside_a = (beta_inside_a + 1.96*se_inside_a)
a_lower_inside <- exp (lwr_inside_a) %>% round (digits = 2)
a_upper_inside <- exp (upp_inside_a) %>% round (digits = 2)

#visited busy outdoor locations
table (merged_case_control_analyse$uitslag_01, merged_case_control_analyse$busylocations_outside)
gam_model_outside <- gam(uitslag_01 ~ busylocations_outside + agegroup + sex + s(week_test, k= 20)  + dayssincevaccination, method= "REML", family = "binomial", data = merged_case_control_analyse)
summary (gam_model_outside)

#OR en CIs for busy locations outside
beta_outside_a <- coef(gam_model_outside)["busylocations_outsideYes"]
Vb_outside_a <- vcov(gam_model_outside)
se_outside_a <- sqrt(diag(Vb_outside_a) ["busylocations_outsideYes"])

a_outside <- exp (beta_outside_a) %>% round (digits = 2)

lwr_outside_a = (beta_outside_a - 1.96*se_outside_a)
upp_outside_a = (beta_outside_a + 1.96*se_outside_a)
a_lower_outside <- exp (lwr_outside_a) %>% round (digits = 2)
a_upper_outside <- exp (upp_outside_a) %>% round (digits = 2)

#facemask everywhere outside the house
table (merged_case_control_analyse$uitslag_01, merged_case_control_analyse$facemask_who_v1)
gam_model_mask1 <- gam(uitslag_01 ~ facemask_who_v1 + agegroup + sex + s(week_test, k= 20) + dayssincevaccination, method= "REML", family = "binomial", data = merged_case_control_analyse)
summary (gam_model_mask1)

#OR en CIs for sometimes/rarely/never facemask wearing outside the house
beta_masksomerarenever_a <- coef(gam_model_mask1)["facemask_who_v1Sometimes/Rarely/Never"]
Vb_masksomerarenever_a <- vcov(gam_model_mask1)
se_masksomerarenever_a <- sqrt(diag(Vb_masksomerarenever_a) ["facemask_who_v1Sometimes/Rarely/Never"])

a_somerarenever_outside <- exp (beta_masksomerarenever_a) %>% round (digits = 2)

lwr_masksomerarenever_a = (beta_masksomerarenever_a - 1.96*se_masksomerarenever_a)
upp_masksomerarenever_a = (beta_masksomerarenever_a + 1.96*se_masksomerarenever_a)
a_lower_somerarenever_outside <- exp (lwr_masksomerarenever_a) %>% round (digits = 2)
a_upper_somerarenever_outside <- exp (upp_masksomerarenever_a) %>% round (digits = 2)

#facemask everywhere inside public spaces
table (merged_case_control_analyse$uitslag_01, merged_case_control_analyse$facemask_who_v2)
gam_model_mask2 <- gam(uitslag_01 ~ facemask_who_v2 + agegroup + sex + s(week_test, k= 20) + dayssincevaccination, method= "REML", family = "binomial", data = merged_case_control_analyse)
summary (gam_model_mask2)



#OR en CIs for sometimes/rarely/never facemask wearing everwhere inside/public spaces
beta_facesomerarenever_a_2 <- coef(gam_model_mask2)["facemask_who_v2Sometimes/Rarely/Never"]
Vb_facesomerarenever_a_2 <- vcov(gam_model_mask2)
se_facesomerarenever_a_2 <- sqrt(diag(Vb_facesomerarenever_a_2) ["facemask_who_v2Sometimes/Rarely/Never"])

a_somerarenever_inside <- exp (beta_facesomerarenever_a_2) %>% round (digits = 2)

lwr_facesomerarenever_a_2 = (beta_facesomerarenever_a_2 - 1.96*se_facesomerarenever_a_2)
upp_facesomerarenever_a_2 = (beta_facesomerarenever_a_2 + 1.96*se_facesomerarenever_a_2)
a_lower_somerarenever_inside <- exp (lwr_facesomerarenever_a_2) %>% round (digits = 2)
a_upper_somerarenever_inside <- exp (upp_facesomerarenever_a_2) %>% round (digits = 2)


####Rearranging all values in two seperate dataframes to merge for one dataframe to make a graph########


###symptmatic cases vs symptomatic controls

data_plot8 <- data.frame (
  OR = c(hh2_3, hh4, child0_12, child13_18, hcw, work1_5, work6_10, work10, workNA, contacts0_4, contacts5_9, contacts10_19, contacts20, inside, outside, somerarenever_outside, somerarenever_inside), 
  CI_low = c(lower_hh_2_3, lower_hh4, lower_child0_12, lower_child13_18, lower_hcw, lower_work1_5, lower_work6_10, lower_work10, lower_workNA, lower_contacts0_4, lower_contacts5_9, lower_contacts10_19, lower_contacts20, lower_inside, lower_outside, lower_somerarenever_outside, lower_somerarenever_inside),
  CI_high = c(upper_hh_2_3, upper_hh4, upper_child0_12, upper_child13_18, upper_hcw, upper_work1_5, upper_work6_10, upper_work10, upper_workNA, upper_contacts0_4, upper_contacts5_9, upper_contacts10_19, upper_contacts20, upper_inside, upper_outside, upper_somerarenever_outside, upper_somerarenever_inside),
  group = c("Symptomatic cases vs symptomatic controls"),
  Risk_factor = c("Household size 2-3", "Household size 4+", "Living with children 0-12 Yes", "Living with children 13-18 Yes", "Healthcare worker Yes", "Went to work 1-5 days", "Went to work 6-10 days", "Went to work 10+ days", "Went to work Does not have to go to work", "Work/study contacts 1-4 persons", "Work/study contacts 5-9 persons", "Work/study contacts 10-19 persons", "Work/study contacts 20+ persons", "Visited busy indoor locations Yes", "Visited busy outdoor locations Yes", "Sometimes/Rarely/Never wore a facemask outside the house", "Sometimes/Rarely/Never wore a facemask inside public spaces"))

data_plot8 <- data_plot8 %>% mutate(Risk_factor = case_when(
  Risk_factor== "Household size 2-3"  ~ 1,
  Risk_factor == "Household size 4+" ~ 2,
  Risk_factor == "Living with children 0-12 Yes"  ~ 3,
  Risk_factor == "Living with children 13-18 Yes"  ~ 4,
  Risk_factor == "Healthcare worker Yes"  ~ 5,
  Risk_factor == "Went to work 1-5 days"  ~ 6,
  Risk_factor == "Went to work 6-10 days"  ~ 7,
  Risk_factor == "Went to work 10+ days"  ~ 8,
  Risk_factor == "Went to work Does not have to go to work"  ~ 9,
  Risk_factor == "Work/study contacts 1-4 persons"  ~ 10,
  Risk_factor == "Work/study contacts 5-9 persons"  ~ 11,
  Risk_factor == "Work/study contacts 10-19 persons"  ~ 12,
  Risk_factor == "Work/study contacts 20+ persons"  ~ 13,
  Risk_factor == "Visited busy indoor locations Yes"  ~ 14,
  Risk_factor == "Visited busy outdoor locations Yes"  ~ 15,
  Risk_factor == "Sometimes/Rarely/Never wore a facemask outside the house"  ~ 16,
  Risk_factor == "Sometimes/Rarely/Never wore a facemask inside public spaces"  ~ 17
) %>% factor(levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), labels = c("Household size 2-3", "Household size 4+", "Living with children 0-12 Yes", "Living with children 13-18 Yes", "Healthcare worker Yes", "Went to work/school 1-5 days", "Went to work/school 6-10 days", "Went to work/school 10+ days", "Went to work/school Does not have to go to work/school", "Work/study contacts 1-4 persons", "Work/study contacts 5-9 persons", "Work/study contacts 10-19 persons", "Work/study contacts 20+ persons", "Visited busy indoor locations Yes", "Visited busy outdoor locations Yes", "Wore a facemask outside the house Sometimes/Rarely/Never", "Wore a facemask inside public spaces Sometimes/Rarely/Never")))


####symptomatic cases vs asymptomatic controls
data_plot9 <- data.frame (
  OR = c(a_hh2_3, a_hh4, a_child0_12, a_child13_18, a_hcw, a_work1_5, a_work6_10, a_work10, a_workNA, a_contacts0_4, a_contacts5_9, a_contacts10_19, a_contacts20, a_inside, a_outside, a_somerarenever_outside, a_somerarenever_inside),
  CI_low = c(a_lower_hh_2_3, a_lower_hh4, a_lower_child0_12, a_lower_child13_18, a_lower_hcw, a_lower_work1_5, a_lower_work6_10, a_lower_work10, a_lower_workNA, a_lower_contacts0_4, a_lower_contacts5_9, a_lower_contacts10_19, a_lower_contacts20, a_lower_inside, a_lower_outside, a_lower_somerarenever_outside, a_lower_somerarenever_inside),
  CI_high = c(a_upper_hh_2_3, a_upper_hh4, a_upper_child0_12, a_upper_child13_18, a_upper_hcw, a_upper_work1_5, a_upper_work6_10, a_upper_work10, a_upper_workNA, a_upper_contacts0_4, a_upper_contacts5_9, a_upper_contacts10_19, a_upper_contacts20, a_upper_inside, a_upper_outside, a_upper_somerarenever_outside, a_upper_somerarenever_inside),
  group = c("Symptomatic cases vs asymptomatic controls"),
  Risk_factor = c("Household size 2-3", "Household size 4+", "Living with children 0-12 Yes", "Living with children 13-18 Yes", "Healthcare worker Yes", "Went to work 1-5 days", "Went to work 6-10 days", "Went to work 10+ days", "Went to work Does not have to go to work", "Work/study contacts 1-4 persons", "Work/study contacts 5-9 persons", "Work/study contacts 10-19 persons", "Work/study contacts 20+ persons", "Visited busy indoor locations Yes", "Visited busy outdoor locations Yes", "Sometimes/Rarely/Never wore a facemask outside the house", "Sometimes/Rarely/Never wore a facemask inside public spaces"))

data_plot9 <- data_plot9 %>% mutate(Risk_factor = case_when(
  Risk_factor== "Household size 2-3"  ~ 1,
  Risk_factor == "Household size 4+" ~ 2,
  Risk_factor == "Living with children 0-12 Yes"  ~ 3,
  Risk_factor == "Living with children 13-18 Yes"  ~ 4,
  Risk_factor == "Healthcare worker Yes"  ~ 5,
  Risk_factor == "Went to work 1-5 days"  ~ 6,
  Risk_factor == "Went to work 6-10 days"  ~ 7,
  Risk_factor == "Went to work 10+ days"  ~ 8,
  Risk_factor == "Went to work Does not have to go to work"  ~ 9,
  Risk_factor == "Work/study contacts 1-4 persons"  ~ 10,
  Risk_factor == "Work/study contacts 5-9 persons"  ~ 11,
  Risk_factor == "Work/study contacts 10-19 persons"  ~ 12,
  Risk_factor == "Work/study contacts 20+ persons"  ~ 13,
  Risk_factor == "Visited busy indoor locations Yes"  ~ 14,
  Risk_factor == "Visited busy outdoor locations Yes"  ~ 15,
  Risk_factor == "Sometimes/Rarely/Never wore a facemask outside the house"  ~ 16,
  Risk_factor == "Sometimes/Rarely/Never wore a facemask inside public spaces"  ~ 17
) %>% factor(levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), labels = c("Household size 2-3", "Household size 4+", "Living with children 0-12 Yes", "Living with children 13-18 Yes", "Healthcare worker Yes", "Went to work/school 1-5 days", "Went to work/school 6-10 days", "Went to work/school 10+ days", "Went to work/school Does not have to go to work/school", "Work/study contacts 1-4 persons", "Work/study contacts 5-9 persons", "Work/study contacts 10-19 persons", "Work/study contacts 20+ persons", "Visited busy indoor locations Yes", "Visited busy outdoor locations Yes", "Wore a facemask outside the house Sometimes/Rarely/Never", "Wore a facemask inside public spaces Sometimes/Rarely/Never")))


###make one dataframe
data_daysvacc <-bind_rows(
  data_plot8,
  data_plot9)

###############################Making graph###################################
fig_data_daysvacc <- data_daysvacc %>%
  # add reference groups
  add_row(Risk_factor = rep(
    c(
      "Household size 1",
      "Living with children 0-12 No",
      "Living with children 13-18 No",
      "Healthcare worker No",
      "Went to work/school Did not go to work/school",
      "Work/study contacts 0 persons",
      "Visited busy indoor locations No",
      "Visited busy outdoor locations No",
      "Wore a facemask outside the house Always/Mostly",
      "Wore a facemask inside public spaces Always/Mostly"
    ),
    2
  ),
  group = c(rep(
    "Symptomatic cases vs symptomatic controls", 
    10
  ), 
  rep("Symptomatic cases vs asymptomatic controls",
      10
  ))) %>% 
  mutate(
    variable = case_when(
      Risk_factor %>% str_detect("^Household size") ~ "Household size",
      Risk_factor %>% str_detect("^Living with children") ~ "Living\nwith children",
      Risk_factor %>% str_detect("^Healthcare worker") ~ "Healthcare worker",
      Risk_factor %>% str_detect("^Went to work/school") ~ "Went to\nwork/school",
      Risk_factor %>% str_detect("^Work/study contacts") ~ "Work/study\ncontacts",
      Risk_factor %>% str_detect("^Visited busy") ~ "Visited busy",
      Risk_factor %>% str_detect("outside the house") ~ "outside\nthe house",
      Risk_factor %>% str_detect("inside public spaces") ~ "inside\npublic spaces",
    ),
    level = Risk_factor %>% str_remove("^Household size |^Living with children |^Healthcare worker |^Went to work/school |^Work/study contacts |^Visited busy |outside the house |inside public spaces") %>%
      factor(levels = rev(
        c(
          "2-3",
          "4+",
          "0-12 Yes",
          "13-18 Yes",
          "Yes",
          "1-5 days",
          "6-10 days",
          "10+ days",
          "Does not have to go to work/school",
          "1-4 persons",
          "5-9 persons",
          "10-19 persons",
          "20+ persons",
          "indoor locations Yes",
          "outdoor locations Yes",
          "Wore a facemask Sometimes/Rarely/Never",
          "Wore a facemask  Sometimes/Rarely/Never",
          "1",
          "0-12 No",
          "13-18 No",
          "No",
          "Did not go to work/school",
          "0 persons",
          "indoor locations No",
          "outdoor locations No",
          "Wore a facemask Always/Mostly",
          "Wore a facemask  Always/Mostly")
      )),
    CI = if_else(
      !is.na(OR),
      paste0(
        round(OR, 2),
        " (",
        round(CI_low, 2),
        " - ",
        round(CI_high, 2),
        ")"
      ),
      "reference"
    )
  ) %>% 
  arrange(Risk_factor) %>%
  ggplot(aes(x = level, y = OR)) +        
  geom_hline(yintercept = 1.0, linetype = 2) +
  geom_point (colour = "deeppink2") +               
  geom_text(
    aes(label = CI,
        x = level, y = 3),
    color = "black",
    position = position_dodge(0.0),
    hjust = 0,
    size = 3.5,
    show.legend = FALSE,
    check_overlap = TRUE
  ) +
  scale_y_continuous(breaks = c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0)) +  
  labs(x = "",
       y = "Odds Ratio (95%-CI)") +
  theme(plot.title = element_text(hjust = 0)) +  
  geom_errorbar(
    aes(ymin = CI_low, ymax = CI_high),
    width = 0.25,
    colour = "deeppink2"
  ) +
  geom_point(position = position_dodge(.5),
             size = 2,
             colour = "deeppink2") +
  coord_flip(clip = "off")    +  
  facet_grid(facets = variable ~ `group`,
             scales = "free_y",
             switch = "y") +
  theme_minimal () +
  theme(
    panel.spacing = unit(50, "pt"),
    plot.margin = margin(1, 2.5, 1, 1, "cm"),
    strip.placement = "outside", 
    text = element_text(size = 12),
    strip.text.y = element_text(size = 10),
    strip.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(0.5, "lines"),
    panel.spacing.x = unit(5, "lines"),
    strip.clip = "off"
  )






