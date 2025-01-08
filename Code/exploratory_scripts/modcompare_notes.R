
#testmod1b - contemporaneous exposure, NB
#didreg_mh_nb4 contemporaneous exposure, controlling for state and year, NB

#didreg_mh_nb canonical DiD with all mental health diagnoses, NB
#didreg_mh_nb generalized DiD with all mental health diagnoses, NB. Controls for state/year rather than introducing fixed effects


#pois1a - canonical DiD with all mental health disagnoses, poisson
#pois2a_alt - generalized DiD with all mental health disagnoses, posisson. controls for state/year rather than introducing fixed effects

#pois2a - generalized DiD with all mental health disagnoses, posisson. State/year two-way fixed effects 



c(seq(-10,-2, by = 1), seq(0,10, by = 1))

# Make the -1 period the central period
merge_prepost$time_to_treat2 <- merge_prepost$time_to_treat 
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -1)] <- 1
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -10)] <- 2
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -9)] <- 3
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -8)] <- 4
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -7)] <- 5
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -6)] <- 6
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -5)] <- 7
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -4)] <- 8
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -3)] <- 9
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -2)] <- 10
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 0)] <- 11
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 1)] <- 12
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 2)] <- 13
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 3)] <- 14
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 4)] <- 15
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 5)] <- 16
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 6)] <- 17
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 7)] <- 18
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 8)] <- 19
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 9)] <- 20
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 10)] <- 21

merge_prepost$time_to_treat2 <- as.factor(merge_prepost$time_to_treat2)


mod_event_mh_boom = feols(mental_health_hospitalizations ~ i(time_to_treat, treat) | 
                            residence_county + year, 
                          data = merge_prepost)


mod_event_mh_boom_factor = feols(mental_health_hospitalizations ~ relevel(time_to_treat2, ref = 1) * treat | 
                            residence_county + year, 
                          data = merge_prepost)




mod_event_adj_boom = fenegbin(adjustment_reaction_hospitalizations ~ i(time_to_treat2, treat) | 
                                residence_county + year, 
                              data = merge_prepost)

mod_event_anx_boom = fenegbin(anxiety_disorders_hospitalizations ~ i(time_to_treat2, treat) | 
                                residence_county + year, 
                              data = merge_prepost)


mod_event_att_boom = fenegbin(attention_disorders_hospitalizations ~ i(time_to_treat2, treat) | 
                                residence_county + year, 
                              data = merge_prepost)


mod_event_mood_boom = fenegbin(mood_disorders_hospitalizations ~ i(time_to_treat2, treat) | 
                                 residence_county + year, 
                               data = merge_prepost)

mod_event_pers_boom = fenegbin(personality_disorders_hospitalizations ~ i(time_to_treat2, treat) | 
                                 residence_county + year, 
                               data = merge_prepost)

mod_event_schiz_boom = fenegbin(schizophrenia_psychotic_disorders_hospitalizations ~ i(time_to_treat2, treat) | 
                                  residence_county + year, 
                                data = merge_prepost)

mod_event_alc_boom = fenegbin(alcohol_disorders_hospitalizations ~ i(time_to_treat2, treat) | 
                                residence_county + year, 
                              data = merge_prepost)


mod_event_sub_boom = fenegbin(substance_disorders_hospitalizations ~ i(time_to_treat2, treat) | 
                                residence_county + year, 
                              data = merge_prepost)


mod_event_suic_boom = fenegbin(suicide_self_harm_hospitalizations ~ i(time_to_treat2, treat) | 
                                 residence_county + year, 
                               data = merge_prepost)



mod_event_adj_bust = feols(adjustment_reaction_hospitalizations ~ i(time_to_treat, treat) | 
                             residence_county + year, 
                           data = merge_prepost)

mod_event_anx_bust = feols(anxiety_disorders_hospitalizations ~ i(time_to_treat, treat) | 
                             residence_county + year, 
                           data = merge_prepost)

mod_event_att_bust = feols(attention_disorders_hospitalizations ~ i(time_to_treat, treat) | 
                             residence_county + year, 
                           data = merge_prepost)

mod_event_mood_bust = feols(mood_disorders_hospitalizations ~ i(time_to_treat, treat) | 
                              residence_county + year, 
                            data = merge_prepost)

mod_event_pers_bust = feols(personality_disorders_hospitalizations ~ i(time_to_treat, treat) | 
                              residence_county + year, 
                            data = merge_prepost)

mod_event_schiz_bust = feols(schizophrenia_psychotic_disorders_hospitalizations ~ i(time_to_treat, treat) | 
                               residence_county + year, 
                             data = merge_prepost)

mod_event_alc_bust = feols(alcohol_disorders_hospitalizations ~ i(time_to_treat, treat) | 
                             residence_county + year, 
                           data = merge_prepost)

mod_event_sub_bust = feols(substance_disorders_hospitalizations ~ i(time_to_treat, treat) | 
                             residence_county + year, 
                           data = merge_prepost)

mod_event_suic_bust = feols(suicide_self_harm_hospitalizations ~ i(time_to_treat, treat) | 
                              residence_county + year, 
                            data = merge_prepost)




png(file="oil_gas_development_medicaid/Figures/feols_adj_effect_plots_boom.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_adj_boom)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_adj_effect_plots_bust.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_adj_bust)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_anx_effect_plots_boom.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_anx_boom)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_anx_effect_plots_bust.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_anx_bust)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_att_effect_plots_boom.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_att_boom)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_att_effect_plots_bust.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_att_bust)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_mood_effect_plots_boom.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_mood_boom)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_mood_effect_plots_bust.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_mood_bust)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_pers_effect_plots_boom.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_pers_boom)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_pers_effect_plots_bust.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_pers_bust)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_schiz_effect_plots_boom.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_schiz_boom)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_schiz_effect_plots_bust.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_schiz_bust)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_alc_effect_plots_boom.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_alc_boom)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_alc_effect_plots_bust.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_alc_bust)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_sub_effect_plots_boom.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_sub_boom)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_sub_effect_plots_bust.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_sub_bust)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_suic_effect_plots_boom.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_suic_boom)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_suic_effect_plots_bust.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_suic_bust)
dev.off()



sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods16.txt")


print("Event models, no covariates. County and year fixed effects.")
print("Uses feols()")
print("Treatment is first boom or bust for a county as indicated by 25% continuous carry through indicator.")
print("Some counties have no boom/bust.")
print("time to treatment is is years preceeding/following boom/bust 'treatment' event")


print("###### Treatment == Bust #######")

print("All mental health")
summary(mod_event_mh_bust)

print("Adjustment disorder")
summary(mod_event_adj_bust)

print("Anxiety disorder")
summary(mod_event_anx_bust)

print("Attention disorder")
summary(mod_event_att_bust)

print("Mood disorder")
summary(mod_event_mood_bust)

print("Personality disorder")
summary(mod_event_pers_bust)

print("Schizophrenia")
summary(mod_event_schiz_bust)

print("Alcohol use")
summary(mod_event_alc_bust)

print("Substance use")
summary(mod_event_sub_bust)

print("Suicide/self-harm")
summary(mod_event_suic_bust)


print("###### Treatment == Boom #######")

print("All mental health")
summary(mod_event_mh_boom)

print("Adjustment disorder")
summary(mod_event_adj_boom)

print("Anxiety disorder")
summary(mod_event_anx_boom)

print("Attention disorder")
summary(mod_event_att_boom)

print("Mood disorder")
summary(mod_event_mood_boom)

print("Personality disorder")
summary(mod_event_pers_boom)

print("Schizophrenia")
summary(mod_event_schiz_boom)

print("Alcohol use")
summary(mod_event_alc_boom)

print("Substance use")
summary(mod_event_sub_boom)

print("Suicide/self-harm")
summary(mod_event_suic_boom)

closeAllConnections()











sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods17.txt")


print("Event models, no covariates. County and year fixed effects.")
print("Uses fenegbin()")
print("Treatment is first boom or bust for a county as indicated by 25% continuous carry through indicator.")
print("Some counties have no boom/bust.")
print("time to treatment is is years preceeding/following boom/bust 'treatment' event")



print("###### Treatment == Bust #######")

print("All mental health")
summary(mod_event_mh_bust)

print("Adjustment disorder")
summary(mod_event_adj_bust)

print("Anxiety disorder")
summary(mod_event_anx_bust)

print("Attention disorder")
summary(mod_event_att_bust)

print("Mood disorder")
summary(mod_event_mood_bust)

print("Personality disorder")
summary(mod_event_pers_bust)

print("Schizophrenia")
summary(mod_event_schiz_bust)

print("Alcohol use")
summary(mod_event_alc_bust)

print("Substance use")
summary(mod_event_sub_bust)

print("Suicide/self-harm")
summary(mod_event_suic_bust)


print("###### Treatment == Boom #######")

print("All mental health")
summary(mod_event_mh_boom)

print("Adjustment disorder")
summary(mod_event_adj_boom)

print("Anxiety disorder")
summary(mod_event_anx_boom)

print("Attention disorder")
summary(mod_event_att_boom)

print("Mood disorder")
summary(mod_event_mood_boom)

print("Personality disorder")
summary(mod_event_pers_boom)

print("Schizophrenia")
summary(mod_event_schiz_boom)

print("Alcohol use")
summary(mod_event_alc_boom)

print("Substance use")
summary(mod_event_sub_boom)

print("Suicide/self-harm")
summary(mod_event_suic_boom)

closeAllConnections()



## Create data to plot
mod_event_mh_boom_tab <- mod_event_mh_boom$coeftable
mod_event_mh_boom_tab$time <- c(0,-9,-8,-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7,8,9,10)

mod_event_mh_bust_tab <- mod_event_mh_bust$coeftable
mod_event_mh_bust_tab$time <- c(0,-9,-8,-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7,8,9,10)



png(file="oil_gas_development_medicaid/Figures/fenegbin_mh_effect_plots_boom.png", width=8, height=6, units="in", res=300)
#fixest::iplot(mod_event_mh_boom)
ggplot(mod_event_mh_boom_tab, aes(time, Estimate)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Estimate - (1.96*`Std. Error`), ymax=Estimate + (1.96 *`Std. Error`))) +
  theme_minimal()
dev.off()

png(file="oil_gas_development_medicaid/Figures/fenegbin_mh_effect_plots_bust.png", width=8, height=6, units="in", res=300)
#fixest::iplot(mod_event_mh_bust)
ggplot(mod_event_mh_bust_tab, aes(time, Estimate)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Estimate - (1.96*`Std. Error`), ymax=Estimate + (1.96 *`Std. Error`))) +
  theme_minimal()
dev.off()


