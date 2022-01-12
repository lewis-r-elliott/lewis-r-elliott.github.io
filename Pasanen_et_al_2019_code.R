
# to get all output printed:
options(max.print=15000)

########################## Model 1a  #############################
hmr_pamet_cov6_tot_urbanqimdeduc01_coast3_unsat_sl <- '
# all effects to the main outcomes.
healthnum ~ c11*greenery + c121*coast1ref1 + c122*coast2ref1 + c123*coast3ref1 + c13*fwater1 + urban01 + qimd2 
+ qimd3+ qimd4 + qimd5 + 
agenum + sexnum  + incomediv1000_mis0 + income_na + educ1 + educ2 + educ3 + educ4 + educ5 + unemp + econinact + 
limill2 + marital + children + infants + carnum + year + b11*otherpamet  + b12*outpamet + b13*waterpamet

ghq12_rev ~ c21*greenery + c221*coast1ref1 + c222*coast2ref1 + c223*coast3ref1 + c23*fwater1 + urban01 + qimd2 
+ qimd3+ qimd4 + qimd5 + 
agenum + sexnum  + incomediv1000_mis0 + income_na + educ1 + educ2 + educ3 + educ4 + educ5 + unemp + econinact + 
limill2 + marital + children + infants + carnum + year + b21*otherpamet  + b22*outpamet + b23*waterpamet

# mediators - outdoor land, outdoor water, and other PA
otherpamet ~ urban01 + qimd2 
+ qimd3+ qimd4 + qimd5 + 
agenum + sexnum  + incomediv1000_mis0 + income_na + educ1 + educ2 + educ3 + educ4 + educ5 + unemp + econinact + 
limill2 + marital + children + infants + carnum + year

outpamet ~ a21*greenery + a221*coast1ref1 + a222*coast2ref1 + a223*coast3ref1 + a23*fwater1 + urban01 + qimd2 
+ qimd3+ qimd4 + qimd5 + 
agenum + sexnum  + incomediv1000_mis0 + income_na + educ1 + educ2 + educ3 + educ4 + educ5 + unemp + econinact + 
limill2 + marital + children + infants + carnum + year

waterpamet ~ a321*coast1ref1 + a322*coast2ref1 + a323*coast3ref1 + a33*fwater1 + urban01 + qimd2 
+ qimd3+ qimd4 + qimd5 + 
agenum + sexnum  + incomediv1000_mis0 + income_na + educ1 + educ2 + educ3 + educ4 + educ5 + unemp + econinact + 
limill2 + marital + children + infants + carnum + year

# indirect effects from the env. variables to health/mental health via the PA variables
#greenery via outdoor (land) PA on health
a21b12 := a21*b12
# coast1 via outdoor (land) PA on health
a221b12 := a221*b12
# coast2 via outdoor (land) PA on health
a222b12 := a222*b12
# coast3 via outdoor (land) PA on health
a223b12 := a223*b12
# fwater via outdoor (land) PA on health
a23b12 := a23*b12
# coast1 via outdoor (water) PA on health
a321b13 := a321*b13
# coast2 via outdoor (water) PA on health
a322b13 := a322*b13
# coast3 via outdoor (water) PA on health
a323b13 := a323*b13
# fwater via outdoor (water) PA on health
a33b13 := a33*b13
#greenery via outdoor (land) PA on mental health
a21b22 := a21*b22
# coast1 via outdoor (land) PA on mental health
a221b22 := a221*b22
# coast2 via outdoor (land) PA on mental health
a222b22 := a222*b22
# coast3 via outdoor (land) PA on mental health
a223b22 := a223*b22
# fwater via outdoor (land) PA on mental health
a23b22 := a23*b22
# coast1 via outdoor (water) PA on mental health
a321b23 := a321*b23
# coast2 via outdoor (water) PA on mental health
a322b23 := a322*b23
# coast3 via outdoor (water) PA on mental health
a323b23 := a323*b23
# fwater via outdoor (water) PA on mental health
a33b23 := a33*b23


# total indirect effects from the env. variables to health/mental health via the PA variables
# from greenery to health
a1b1 := a21*b12 
# coast1 to health
a21b1 := a221*b12 + a321*b13 
# coast2 to health
a22b1 := a222*b12 + a322*b13 
# coast3 to health
a23b1 := a223*b12 + a323*b13
# fwater to health
a3b1 := a23*b12 + a33*b13
# from greenery to mental health
a1b2 := a21*b22 
# coast1 to mental health
a21b2 := a221*b22 + a321*b23 
# coast2 to mental health
a22b2 := a222*b22 + a322*b23 
# coast3 to mental health
a23b2 := a223*b22 + a323*b23
# fwater to mental health
a3b2 := a23*b22 + a33*b23

# total effects from env. variables to health
# from greenery to health
a1b1c1 := a21*b12 + c11
# coast1 to health
a21b1c1 := a221*b12 + a321*b13 + c121
# coast2 to health
a22b1c1 := a222*b12 + a322*b13 + c122
# coast3 to health
a23b1c1 := a223*b12 + a323*b13 + c123
# fwater to health
a3b1c1 := a23*b12 + a33*b13 + c13

# from greenery to mental health
a1b2c2 := a21*b22 + c21
# coast1 to mental health
a21b2c2 := a221*b22 + a321*b23 + c221
# coast2 to mental health
a22b2c2 := a222*b22 + a322*b23 + c222
# coast3 to mental health
a23b2c2 := a223*b22 + a323*b23 + c223
# fwater to mental health
a3b2c2 := a23*b22 + a33*b23 + c23


# residual correlations for the PA variables and the outcomes
healthnum ~~ ghq12_rev
otherpamet ~~ outpamet
otherpamet ~~ waterpamet 
waterpamet ~~ outpamet

'

fit.hmr_pamet_cov6_tot_urbanqimdeduc01_coast3_unsat_sl <-
  sem(hmr_pamet_cov6_tot_urbanqimdeduc01_coast3_unsat_sl, 
      data = hse_08_12_an, missing="listwise", estimator="MLM") 

# calculate the 'survey design' to adjust the se's.
survey.design <- svydesign(id=~DHSERIAL, strata = ~syear, weights = ~wt_int, nest = TRUE, data = hse_08_12_an)

# refit the model taking into account the survey design.
fit.hmr_pamet_cov6_tot_urbanqimdeduc01_coast3_unsat_survey <- 
  lavaan.survey(lavaan.fit = fit.hmr_pamet_cov6_tot_urbanqimdeduc01_coast3_unsat_sl, 
                survey.design = survey.design)

summary(fit.hmr_pamet_cov6_tot_urbanqimdeduc01_coast3_unsat_survey,
        standardized=FALSE, rsquare=TRUE, fit.measures = TRUE, nd=5)


setwd("F:\\Users\\jkg209\\Documents\\HSE_JG\\Tytti\\Model_results\\Model_1a")
tidy_fit.hmr_pamet_cov6_tot_urbanqimdeduc01_coast3_unsat_survey<-
  tidy(fit.hmr_pamet_cov6_tot_urbanqimdeduc01_coast3_unsat_survey)
write.csv(tidy_fit.hmr_pamet_cov6_tot_urbanqimdeduc01_coast3_unsat_survey, "summary_m1a_standardised_false.csv")
rm(tidy_fit.hmr_pamet_cov6_tot_urbanqimdeduc01_coast3_unsat_survey)

# to get all estimates, increase max printed rows.
parameterEstimates(fit.hmr_pamet_cov6_tot_urbanqimdeduc01_coast3_unsat_survey,
                   ci = TRUE, level = 0.95)

# model assessment. first residuals.
resid(fit.hmr_pamet_cov6_tot_urbanqimdeduc01_coast3_unsat_survey, type="normalized")
# then modification indices.
mi <- modindices(fit.hmr_pamet_cov6_tot_urbanqimdeduc01_coast3_unsat_survey)
mihead(mi[order(mi$mi, decreasing=TRUE), ], 10)
write.csv(mi, "mmodel_1a_modification_indices.csv")

# CIs 
PEs_lower<- parameterEstimates(fit.hmr_pamet_cov6_tot_urbanqimdeduc01_coast3_unsat_survey, ci = TRUE, level = 0.95)$ci.lower
write.csv(PEs_lower, "Model_1a_parameter_ests_lower.csv")
PEs_upper<-parameterEstimates(fit.hmr_pamet_cov6_tot_urbanqimdeduc01_coast3_unsat_survey, ci = TRUE, level = 0.95)$ci.upper
write.csv(PEs_upper, "Model_1a_parameter_ests_upper.csv")

####################################################################################
