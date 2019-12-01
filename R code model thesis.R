setwd("/Users/mathijsvanboesschoten/Documents/Efteling")

#load data (standardized)
not_standardised <- read.csv(file="general_data_not_standardised.csv", header=TRUE, sep=",")

# load required packages
library(dplyr)
library("semTable")
library(devtools)
library(piecewiseSEM)
library(lavaan)
vignette("piecewiseSEM")
install_github("jslefche/piecewiseSEM@devel", build_vignette = TRUE, force= TRUE)
library(apaTables)
library(psych)
library(semTools)


#Create subsets for weather, crowdedness and grade visit
weer <- not_standardised  %>%
  select(weer.perceptie,avg_windspeed, som_neerslag_mm, avg_bewolking, change_avg_temp,
         dicht_door_weer, duur_neerslag, hoogste_neerslag_in_uur, avg_percent_luchtvochtigheid )

drukte <- not_standardised  %>%
  select(drukte.perceptie, aantal_bezoekers, misbegroting, avg.waiting.time.cat.A, avg.waiting.time.cat.B, avg.waiting.time.cat.C, dicht_door_gepland_onderhoud, number_a_down, totale_openingstijd_park)


bezoek <- not_standardised  %>%
  select( cijfer.bezoek, cijfer.parkshows, cijfer.attracties, prijs_kwaliteit, cijfer.attracties, weer.perceptie, cijfer.sfeer, cijfer.medewerkers, drukte.perceptie, cijfer.horeca, number_a_down, totale_openingstijd_park, 
          avg.waiting.time.cat.A, reuzenfeesten, misbegroting)


#Check for skewness en kurtosis
describe(drukte)
describe(weer)
describe(bezoek)

#remove som neerslag mm, dicht door weer, hoogste neerslag in uur, reuzenfeesten based on skewed values
weer <- not_standardised  %>%
  select(weer.perceptie,avg_windspeed,avg_bewolking, change_avg_temp,
         duur_neerslag, avg_percent_luchtvochtigheid )

bezoek <- not_standardised  %>%
  select( cijfer.bezoek, cijfer.parkshows, cijfer.attracties, prijs_kwaliteit, weer.perceptie, cijfer.sfeer, cijfer.medewerkers, drukte.perceptie, cijfer.horeca)

#create correlation tables
apa.cor.table(weer, filename = "weer corr.doc", table.number = NA,
              show.conf.interval = FALSE, landscape = TRUE)

apa.cor.table(drukte, filename = "drukte corr.doc", table.number = NA,
              show.conf.interval = FALSE, landscape = TRUE)

apa.cor.table(bezoek, filename = "bezoek corr.doc", table.number = NA,
              show.conf.interval = FALSE, landscape = TRUE)

#------------------------First stage logistic regression without extra variables (base model)--------------------------
basic_efteling.reg <- lm(cijfer.bezoek ~ cijfer.attracties + prijs_kwaliteit + weer.perceptie + drukte.perceptie +
                           cijfer.sfeer  + cijfer.medewerkers + cijfer.horeca, data = not_standardised)

summary(basic_efteling.reg,fit.measures=TRUE,rsq=T, standardized=FALSE)

apa.reg.table(basic_efteling.reg, filename = "regression_efteling_test.doc", table.number = 1)


#First stage logistic regression with extra variables (extended model)
basic.reg <- lm(cijfer.bezoek ~  cijfer.parkshows + cijfer.attracties + prijs_kwaliteit + weer.perceptie + drukte.perceptie +
                  cijfer.sfeer  + cijfer.medewerkers + cijfer.horeca + number_a_down + totale_openingstijd_park +
                  avg.waiting.time.cat.A + misbegroting , data = not_standardised)

summary(basic.reg,fit.measures=TRUE,rsq=T, standardized=FALSE)

apa.reg.table(basic.reg, filename = "extra_attributes_test.doc", table.number = 1)


#------------------------------------- Stage 2: build path analysis in Lavaan-------------------------------------------

path_analysis <- '
# indirect regressions
weer.perceptie ~ a1 * avg_windspeed  + a2 * avg_bewolking + a3 * change_avg_temp + a4 * avg_percent_luchtvochtigheid 
drukte.perceptie ~ a5 * misbegroting + a6 * avg.waiting.time.cat.A + a7 * avg.waiting.time.cat.B + a8 * avg.waiting.time.cat.C + a9 * number_a_down + a10 * totale_openingstijd_park 

# main regression
cijfer.bezoek ~   b1 * weer.perceptie + b2 * drukte.perceptie + c1 * avg_windspeed + c2 * avg_bewolking +
  c3 * change_avg_temp + c4 * avg_percent_luchtvochtigheid +
  c5 * misbegroting + c6 * avg.waiting.time.cat.A + c7 * avg.waiting.time.cat.B + c8 * avg.waiting.time.cat.C  + 
  c9 * number_a_down + c10 * totale_openingstijd_park + cijfer.parkshows + cijfer.attracties + prijs_kwaliteit + cijfer.attracties  + cijfer.sfeer  + cijfer.medewerkers  + cijfer.horeca 

#indirect effects
a1b1 := a1 * b1
a2b1 := a2 * b1
a3b1 := a3 * b1
a4b1 := a4 * b1

a5b2 := a5 * b2
a6b2 := a6 * b2
a7b2 := a7 * b2
a8b2 := a8 * b2
a9b2 := a9 * b2
a10b2 := a10 * b2

# total effect
total_weer := c1 + (a1 * b1) + c2 + (a2 * b1) + c3 + (a3 * b1) + c4 + (a4 * b1) 
total_drukte := c5 + (a5 * b2) + c6 + (a6 * b2) + c7 + (a7 * b2) + c8 + (a8 * b2) + c9 + (a9 * b2) + c10 + (a10 * b2) 
'

path_analysis <- sem(model = path_analysis, data =not_standardised)
summary(path_analysis ,fit.measures=TRUE,rsq=T, standardized=FALSE)

# Stage 2: Controlling whether subjective constructs can be replaced by objective measures

path_analysis_control <- '
# main regression
cijfer.bezoek ~ c1 * avg_windspeed + c2 * avg_bewolking +
c3 * change_avg_temp + c4 * avg_percent_luchtvochtigheid +
c5 * misbegroting + c6 * avg.waiting.time.cat.A + c7 * avg.waiting.time.cat.B + c8 * avg.waiting.time.cat.C  + 
c9 * number_a_down + c10 * totale_openingstijd_park + cijfer.parkshows + cijfer.attracties + prijs_kwaliteit + cijfer.attracties  + cijfer.sfeer  + cijfer.medewerkers  + cijfer.horeca 
'

path_analysis_control_model <- sem(model = path_analysis_control, data =not_standardised)
summary(path_analysis_control_model,fit.measures=TRUE,rsq=T, standardized=FALSE)


#--------------------------------------Stage 3: Testing for measurement invariance-------------------------------------

#test for measurement invariance (constrained vs not constrained)
multigroup1 <- sem(model = path_analysis, data =not_standardised, group= "cluster")
multigroup1.constrained <- sem(model = pmodel_control, data = not_standardised, group= "cluster", group.equal = c("intercepts", "regressions"))
anova(multigroup1, multigroup1.constrained)

# build piecewice sem-model
pmodel <- psem(lm(weer.perceptie ~ avg_windspeed + avg_bewolking + change_avg_temp + avg_percent_luchtvochtigheid, not_standardised), 
                lm(drukte.perceptie ~ misbegroting + avg.waiting.time.cat.A + avg.waiting.time.cat.B + avg.waiting.time.cat.C  + number_a_down + totale_openingstijd_park, not_standardised),
                lm(cijfer.bezoek ~  weer.perceptie + drukte.perceptie + avg_windspeed + avg_bewolking +
                    change_avg_temp + avg_percent_luchtvochtigheid +
                    misbegroting + avg.waiting.time.cat.A + avg.waiting.time.cat.B + avg.waiting.time.cat.C  + 
                    number_a_down + totale_openingstijd_park + cijfer.parkshows + cijfer.attracties + prijs_kwaliteit + cijfer.attracties  + cijfer.sfeer  + cijfer.medewerkers  + cijfer.horeca, not_standardised))

#show which variables are significantly different for types of visitors
(pmultigroup <- multigroup(pmodel, group = "cluster"))

#create constrained model which is flexible for the identified significantly different variables
multigroup1.constrained <- sem(model = path_analysis, data =not_standardised, group= "cluster", group.equal = c("intercepts", "regressions"), group.partial = c("drukte.perceptie ~ aantal_bezoekers + number_a_down", "cijfer.bezoek ~ drukte.perceptie + avg.waiting.time.cat.A + avg.waiting.time.cat.B + cijfer.medewerkers"))
summary(multigroup1.constrained,fit.measures=TRUE,rsq=T, standardized=FALSE)

#Controlling whether subjective constructs can be replaced by objective measures
pmodel_control <- psem(lm(cijfer.bezoek ~ change_avg_temp + duur_neerslag + avg_percent_luchtvochtigheid + 
                            avg.waiting.time.cat.A + 
                            number_a_down + cijfer.parkshows + cijfer.attracties + prijs_kwaliteit + cijfer.attracties  + cijfer.sfeer  + cijfer.medewerkers  + cijfer.horeca, not_standardised))
(pmultigroup <- multigroup(pmodel_control, group = "cluster"))


#---------------------------------Complete model all tested subjective constructs--------------------------------------

path_analysis_full <- '
# other regressions
weer.perceptie ~ avg_windspeed + avg_bewolking + change_avg_temp + avg_percent_luchtvochtigheid
cijfer.medewerkers ~ staff_attracties_uren + staff_entertainment_uren + staff_entree_uren + staff_onderhoud_uren + opleiding_park_uren + staff_horeca_uren + opleiding_horeca_uren + staff_merchandise_uren + opleiding_merchandise_uren
drukte.perceptie ~ misbegroting + avg.waiting.time.cat.A + avg.waiting.time.cat.B + avg.waiting.time.cat.C   + number_a_down + totale_openingstijd_park 
cijfer.parkshows ~ Raveleijn_shows + Aquanura_shows + Jokie.en.Jet_shows + Sprookjesboom_shows + Sprookjessprokkelaar_shows
cijfer.horeca ~ Hor_Open_Anderrijk + Hor_Open_Fantasierijk + Hor_Open_Marerijk + Hor_Open_Reizenrijk + Hor_Open_Ruigrijk
prijs_kwaliteit ~ avg_entreeprijs + Consignatie.entrees + ratio_cosignatie_vs_regular_online + reuzenfeesten + totale_openingstijd_park
cijfer.sfeer ~ Entertainment + Show
cijfer.attracties ~ avg.waiting.time.cat.A + avg.waiting.time.cat.B + avg.waiting.time.cat.C 

# main regression
cijfer.bezoek ~ cijfer.parkshows + cijfer.attracties + prijs_kwaliteit + cijfer.attracties + weer.perceptie + 
cijfer.sfeer  + cijfer.medewerkers + drukte.perceptie + cijfer.horeca
'

test <- sem(model = path_analysis_full,data = not_standardised, check.gradient = FALSE)
summary(test,rsq=T)

