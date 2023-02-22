### code for CLPM and APIM
##Trish Gilholm

##CLPM Mothers: Mental HRQoL
#Model 1 stability effects only
library(lavaan)

Model1_mothers<-'
         #Latent variables
         PTSS1 =~ 1*T1_pds_ss_mc
         PTSS2 =~ 1*T2_pds_ss_mc
         PTSS3 =~ 1*T3_pds_ss_mc
         PTSS4 =~ 1*T4_pds_ss_mc
         
         HRQoL1 =~ 1*T1_MCS12
         HRQoL2 =~ 1*T2_MCS12
         HRQoL3 =~ 1*T3_MCS12
         HRQoL4 =~ 1*T4_MCS12
         #PTSS
         PTSS2 ~ PTSS1
         PTSS3 ~ PTSS2
         PTSS4 ~ PTSS3
         #HRQoL
         HRQoL2 ~ HRQoL1
         HRQoL3 ~ HRQoL2
         HRQoL4 ~ HRQoL3
         #covariances
         PTSS1 ~~ HRQoL1
         PTSS2 ~~ HRQoL2
         PTSS3 ~~ HRQoL3
         PTSS4 ~~ HRQoL4
         
         #variances
         PTSS1 ~~ PTSS1
         PTSS2 ~~ PTSS2
         PTSS3 ~~ PTSS3
         PTSS4 ~~ PTSS4
         
         HRQoL1 ~~ HRQoL1
         HRQoL2 ~~ HRQoL2
         HRQoL3 ~~ HRQoL3
         HRQoL4 ~~ HRQoL4
        '

fit <- sem(Model1_mothers, data = data_sem_scaled, estimator = "MLR", missing = "fiml")
summary(fit, fit.measures= TRUE, standardized=TRUE,rsquare=TRUE)
fitMeasures(fit)

#Model 2 - cross lagged effects PTSS -> HRQoL
Model2_mothers<-'
         #Latent variables
         PTSS1 =~ 1*T1_pds_ss_mc
         PTSS2 =~ 1*T2_pds_ss_mc
         PTSS3 =~ 1*T3_pds_ss_mc
         PTSS4 =~ 1*T4_pds_ss_mc
         
         HRQoL1 =~ 1*T1_MCS12
         HRQoL2 =~ 1*T2_MCS12
         HRQoL3 =~ 1*T3_MCS12
         HRQoL4 =~ 1*T4_MCS12
         #PTSS
         PTSS2 ~ PTSS1
         PTSS3 ~ PTSS2
         PTSS4 ~ PTSS3
         #HRQoL
         HRQoL2 ~ HRQoL1 + b1*PTSS1
         HRQoL3 ~ HRQoL2 + b1*PTSS2
         HRQoL4 ~ HRQoL3 + b1*PTSS3
         #covariances
         PTSS1 ~~ HRQoL1
         PTSS2 ~~ HRQoL2
         PTSS3 ~~ HRQoL3
         PTSS4 ~~ HRQoL4
         
         #variances
         PTSS1 ~~ PTSS1
         PTSS2 ~~ PTSS2
         PTSS3 ~~ PTSS3
         PTSS4 ~~ PTSS4
         
         HRQoL1 ~~ HRQoL1
         HRQoL2 ~~ HRQoL2
         HRQoL3 ~~ HRQoL3
         HRQoL4 ~~ HRQoL4
        '


fit2 <- sem(Model2_mothers, data = data_sem_scaled, estimator = "MLR", missing = "fiml")
summary(fit2, fit.measures= TRUE, standardized=TRUE, rsquare=TRUE)

#compare model 1 and model 2
anova(fit, fit2, method = "satorra.bentler.2010")

#Model 3 - cross lagged effects HRQoL -> PTSS
Model3_mothers<-'
         #Latent variables
         PTSS1 =~ 1*T1_pds_ss_mc
         PTSS2 =~ 1*T2_pds_ss_mc
         PTSS3 =~ 1*T3_pds_ss_mc
         PTSS4 =~ 1*T4_pds_ss_mc
         
         HRQoL1 =~ 1*T1_MCS12
         HRQoL2 =~ 1*T2_MCS12
         HRQoL3 =~ 1*T3_MCS12
         HRQoL4 =~ 1*T4_MCS12
         #PTSS
         PTSS2 ~ PTSS1 + b2*HRQoL1
         PTSS3 ~ PTSS2 + b2*HRQoL2
         PTSS4 ~ PTSS3 + b2*HRQoL3
         #HRQoL
         HRQoL2 ~ HRQoL1 
         HRQoL3 ~ HRQoL2 
         HRQoL4 ~ HRQoL3 
         
         #covariances
         PTSS1 ~~ HRQoL1
         PTSS2 ~~ HRQoL2
         PTSS3 ~~ HRQoL3
         PTSS4 ~~ HRQoL4
         
         #variances
         PTSS1 ~~ PTSS1
         PTSS2 ~~ PTSS2
         PTSS3 ~~ PTSS3
         PTSS4 ~~ PTSS4
         
         HRQoL1 ~~ HRQoL1
         HRQoL2 ~~ HRQoL2
         HRQoL3 ~~ HRQoL3
         HRQoL4 ~~ HRQoL4
        '

fit3 <- sem(Model3_mothers, data = data_sem_scaled, estimator = "MLR", missing = "fiml")
summary(fit3, fit.measures= TRUE, standardized=TRUE, rsquare=TRUE)

#compare model 1 and model 3
anova(fit, fit3, method = "satorra.bentler.2010")

#Model 4 - all cross lagged effects
Model4_mothers<-'
          #Latent variables
         PTSS1 =~ 1*T1_pds_ss_mc
         PTSS2 =~ 1*T2_pds_ss_mc
         PTSS3 =~ 1*T3_pds_ss_mc
         PTSS4 =~ 1*T4_pds_ss_mc
         
         HRQoL1 =~ 1*T1_MCS12
         HRQoL2 =~ 1*T2_MCS12
         HRQoL3 =~ 1*T3_MCS12
         HRQoL4 =~ 1*T4_MCS12
         #PTSS
         PTSS2 ~ PTSS1 + b2*HRQoL1
         PTSS3 ~ PTSS2 + b2*HRQoL2
         PTSS4 ~ PTSS3 + b2*HRQoL3
         #HRQoL
         HRQoL2 ~ HRQoL1 + b1*PTSS1
         HRQoL3 ~ HRQoL2 + b1*PTSS2 
         HRQoL4 ~ HRQoL3 + b1*PTSS3
         #covariances
         PTSS1 ~~ HRQoL1
         PTSS2 ~~ HRQoL2
         PTSS3 ~~ HRQoL3
         PTSS4 ~~ HRQoL4
         
         #variances
         PTSS1 ~~ PTSS1
         PTSS2 ~~ PTSS2
         PTSS3 ~~ PTSS3
         PTSS4 ~~ PTSS4
         
         HRQoL1 ~~ HRQoL1
         HRQoL2 ~~ HRQoL2
         HRQoL3 ~~ HRQoL3
         HRQoL4 ~~ HRQoL4
        '

fit4 <- sem(Model4_mothers, data = data_sem_scaled, estimator = "MLR", missing = "fiml")
summary(fit4, fit.measures= TRUE, standardized=TRUE, rsquare=TRUE)
standardizedsolution(fit4, level=.95)
#compare model 4 and Model 1
anova(fit, fit4, method = "satorra.bentler.2010")
#Compare model 4 and model 2
anova(fit2, fit4, method = "satorra.bentler.2010")
#Compare model 4 and mdodel 3
anova(fit3, fit4, method = "satorra.bentler.2010")

##CLPM children: psychosocial QoL scale
#Model 1 - stability effects only
Model1_child_PSHS<-'
         #latent variables
         PTSS1 =~ 1*T1_tscyc_PTSTOT_mc 
         PTSS2 =~ 1*T2_tscyc_PTSTOT_mc 
         PTSS3 =~ 1*T3_tscyc_PTSTOT_mc 
         PTSS4 =~ 1*T4_tscyc_PTSTOT_mc 
         
         HRQoL1 =~ 1*T1_pedsPSHS_mc
         HRQoL2 =~ 1*T2_pedsPSHS_mc
         HRQoL3 =~ 1*T3_pedsPSHS_mc
         HRQoL4 =~ 1*T4_pedsPSHS_mc
         
         #PTSS
         PTSS2 ~ PTSS1
         PTSS3 ~ PTSS2
         PTSS4 ~ PTSS3
         #HRQoL
         HRQoL2 ~ HRQoL1
         HRQoL3 ~ HRQoL2
         HRQoL4 ~ HRQoL3
         #covariances
         PTSS1 ~~ HRQoL1
         PTSS2 ~~ HRQoL2
         PTSS3 ~~ HRQoL3
         PTSS4 ~~ HRQoL4
         
         #variances
         PTSS1 ~~ PTSS1
         PTSS2 ~~ PTSS2
         PTSS3 ~~ PTSS3
         PTSS4 ~~ PTSS4
         
         HRQoL1 ~~ HRQoL1
         HRQoL2 ~~ HRQoL2
         HRQoL3 ~~ HRQoL3
         HRQoL4 ~~ HRQoL4
        '



fit_PSHS <- sem(Model1_child_PSHS, data = data_sem_scaled, estimator = "MLR", missing = "fiml")
summary(fit_PSHS, fit.measures= TRUE, standardized=TRUE,rsquare=TRUE)
standardizedsolution(fit_PSHS, level=.95)

#Model 2: cross lagged effects from PTSS ->HRQoL
Model2b_child_PSHS<-'
         #latent variables
         PTSS1 =~ 1*T1_tscyc_PTSTOT_mc 
         PTSS2 =~ 1*T2_tscyc_PTSTOT_mc 
         PTSS3 =~ 1*T3_tscyc_PTSTOT_mc 
         PTSS4 =~ 1*T4_tscyc_PTSTOT_mc 
         
         HRQoL1 =~ 1*T1_pedsPSHS_mc
         HRQoL2 =~ 1*T2_pedsPSHS_mc
         HRQoL3 =~ 1*T3_pedsPSHS_mc
         HRQoL4 =~ 1*T4_pedsPSHS_mc
         
         #PTSS
         PTSS2 ~ PTSS1
         PTSS3 ~ PTSS2
         PTSS4 ~ PTSS3
         #HRQoL
         HRQoL2 ~ HRQoL1 + b1*PTSS1
         HRQoL3 ~ HRQoL2 + b1*PTSS2
         HRQoL4 ~ HRQoL3 + b1*PTSS3
         #covariances
         PTSS1 ~~ HRQoL1 
         PTSS2 ~~ HRQoL2 
         PTSS3 ~~ HRQoL3 
         PTSS4 ~~ HRQoL4
         
         #variances
         PTSS1 ~~ PTSS1
         PTSS2 ~~ PTSS2
         PTSS3 ~~ PTSS3
         PTSS4 ~~ PTSS4
         
         HRQoL1 ~~ HRQoL1
         HRQoL2 ~~ HRQoL2
         HRQoL3 ~~ HRQoL3
         HRQoL4 ~~ HRQoL4
        '

fit2_PSHS <- sem(Model2b_child_PSHS, data = data_sem_scaled, estimator = "MLR", missing = "fiml")
summary(fit2_PSHS, fit.measures= TRUE, standardized=TRUE, rsquare=TRUE)

#compare Model 1 and Model 2
anova(fit_PSHS, fit2_PSHS, method = "satorra.bentler.2010")

#<odel 3: corss lagged effects from HRQoL -> PTSS
Model3_child_PSHS<-'
         #latent variables
         PTSS1 =~ 1*T1_tscyc_PTSTOT_mc 
         PTSS2 =~ 1*T2_tscyc_PTSTOT_mc 
         PTSS3 =~ 1*T3_tscyc_PTSTOT_mc 
         PTSS4 =~ 1*T4_tscyc_PTSTOT_mc 
         
         HRQoL1 =~ 1*T1_pedsPSHS_mc
         HRQoL2 =~ 1*T2_pedsPSHS_mc
         HRQoL3 =~ 1*T3_pedsPSHS_mc
         HRQoL4 =~ 1*T4_pedsPSHS_mc
         
         #PTSS
         PTSS2 ~ PTSS1 + b2*HRQoL1
         PTSS3 ~ PTSS2 + b2*HRQoL2
         PTSS4 ~ PTSS3 + b2*HRQoL3
         #HRQoL
         HRQoL2 ~ HRQoL1 
         HRQoL3 ~ HRQoL2 
         HRQoL4 ~ HRQoL3 
         #covariances
         PTSS1 ~~ HRQoL1 
         PTSS2 ~~ HRQoL2 
         PTSS3 ~~ HRQoL3 
         PTSS4 ~~ HRQoL4
         
         #variances
         PTSS1 ~~ PTSS1
         PTSS2 ~~ PTSS2
         PTSS3 ~~ PTSS3
         PTSS4 ~~ PTSS4
         
         HRQoL1 ~~ HRQoL1
         HRQoL2 ~~ HRQoL2
         HRQoL3 ~~ HRQoL3
         HRQoL4 ~~ HRQoL4
        '

fit3_PSHS <- sem(Model3_child_PSHS, data = data_sem_scaled, estimator = "MLR", missing = "fiml")
summary(fit3_PSHS, fit.measures= TRUE, standardized=TRUE, rsquare=TRUE)

#compare Model 1 and Model 3
anova(fit_PSHS, fit3_PSHS, method = "satorra.bentler.2010")

#Model 4: All cross-lagged paths
Model4_child_PSHS<-'
          #latent variables
         PTSS1 =~ 1*T1_tscyc_PTSTOT_mc 
         PTSS2 =~ 1*T2_tscyc_PTSTOT_mc 
         PTSS3 =~ 1*T3_tscyc_PTSTOT_mc 
         PTSS4 =~ 1*T4_tscyc_PTSTOT_mc 
         
         HRQoL1 =~ 1*T1_pedsPSHS_mc
         HRQoL2 =~ 1*T2_pedsPSHS_mc
         HRQoL3 =~ 1*T3_pedsPSHS_mc
         HRQoL4 =~ 1*T4_pedsPSHS_mc
         
         #PTSS
         PTSS2 ~ PTSS1 + b2*HRQoL1
         PTSS3 ~ PTSS2 + b2*HRQoL2
         PTSS4 ~ PTSS3 + b2*HRQoL3
         #HRQoL
         HRQoL2 ~ HRQoL1 + b1*PTSS1
         HRQoL3 ~ HRQoL2 + b1*PTSS2
         HRQoL4 ~ HRQoL3 + b1*PTSS3
         #covariances
         PTSS1 ~~ HRQoL1 
         PTSS2 ~~ HRQoL2 
         PTSS3 ~~ HRQoL3 
         PTSS4 ~~ HRQoL4
         
         #variances
         PTSS1 ~~ PTSS1
         PTSS2 ~~ PTSS2
         PTSS3 ~~ PTSS3
         PTSS4 ~~ PTSS4
         
         HRQoL1 ~~ HRQoL1
         HRQoL2 ~~ HRQoL2
         HRQoL3 ~~ HRQoL3
         HRQoL4 ~~ HRQoL4
        '

fit4_PSHS <- sem(Model4_child_PSHS, data = data_sem_scaled, estimator = "MLR", missing = "fiml")
summary(fit4_PSHS, fit.measures= TRUE, standardized=TRUE, rsquare=TRUE)
standardizedsolution(fit4_PSHS, level=.95)
#compare Model 1 and Model 4
anova(fit_PSHS, fit4_PSHS, method = "satorra.bentler.2010")
#compare Model 2 and Model 4
anova(fit2_PSHS, fit4_PSHS, method = "satorra.bentler.2010")
#compare Model 3 and Model 4
anova(fit3_PSHS, fit4_PSHS, method = "satorra.bentler.2010")

###APIM - Mother mental HRQoL and child psychosocial HRQoL
#Model 1 No partner paths
Model1_combined_PSHS<-'
         #latent variables child
         c_PTSS1 =~ 1*T1_tscyc_PTSTOT_mc 
         c_PTSS2 =~ 1*T2_tscyc_PTSTOT_mc 
         c_PTSS3 =~ 1*T3_tscyc_PTSTOT_mc 
         c_PTSS4 =~ 1*T4_tscyc_PTSTOT_mc 
         
         c_HRQoL1 =~ 1*T1_pedsPSHS_mc
         c_HRQoL2 =~ 1*T2_pedsPSHS_mc
         c_HRQoL3 =~ 1*T3_pedsPSHS_mc
         c_HRQoL4 =~ 1*T4_pedsPSHS_mc
         
         #latent variables mother
         m_PTSS1 =~ 1*T1_pds_ss_mc
         m_PTSS2 =~ 1*T2_pds_ss_mc
         m_PTSS3 =~ 1*T3_pds_ss_mc
         m_PTSS4 =~ 1*T4_pds_ss_mc
         
         m_HRQoL1 =~ 1*T1_MCS12
         m_HRQoL2 =~ 1*T2_MCS12
         m_HRQoL3 =~ 1*T3_MCS12
         m_HRQoL4 =~ 1*T4_MCS12
          #mother effects
           #PTSS
         m_PTSS2 ~ m_PTSS1 + b2*m_HRQoL1
         m_PTSS3 ~ m_PTSS2 + b2*m_HRQoL2
         m_PTSS4 ~ m_PTSS3 + b2*m_HRQoL3
         #HRQoL
         m_HRQoL2 ~ m_HRQoL1 + b1*m_PTSS1
         m_HRQoL3 ~ m_HRQoL2 + b1*m_PTSS2 
         m_HRQoL4 ~ m_HRQoL3 + b1*m_PTSS3
         
         #child effects
         #PTSS
         #PTSS
         c_PTSS2 ~ c_PTSS1 + b3*c_HRQoL1
         c_PTSS3 ~ c_PTSS2 + b3*c_HRQoL2
         c_PTSS4 ~ c_PTSS3 + b3*c_HRQoL3
         #HRQoL
         c_HRQoL2 ~ c_HRQoL1 + b4*c_PTSS1
         c_HRQoL3 ~ c_HRQoL2 + b4*c_PTSS2 
         c_HRQoL4 ~ c_HRQoL3 + b4*c_PTSS3
         
         #correlations
         #mother
         m_PTSS1 ~~ m_HRQoL1
         m_PTSS2 ~~ m_HRQoL2
         m_PTSS3 ~~ m_HRQoL3
         m_PTSS4 ~~ m_HRQoL4
         
         #child
         c_PTSS1 ~~ c_HRQoL1
         c_PTSS2 ~~ c_HRQoL2
         c_PTSS3 ~~ c_HRQoL3
         c_PTSS4 ~~ c_HRQoL4
         
         #mother/child
         m_HRQoL1 ~~ c_PTSS1
         m_HRQoL2 ~~ c_PTSS2
         m_HRQoL3 ~~ c_PTSS3
         m_HRQoL4 ~~ c_PTSS4
         
         c_HRQoL1 ~~ m_PTSS1
         c_HRQoL2 ~~ m_PTSS2
         c_HRQoL3 ~~ m_PTSS3
         c_HRQoL4 ~~ m_PTSS4
         
         m_HRQoL1 ~~ c_HRQoL1
         m_HRQoL2 ~~ c_HRQoL2
         m_HRQoL3 ~~ c_HRQoL3
         m_HRQoL4 ~~ c_HRQoL4
         
         m_PTSS1 ~~ c_PTSS1
         m_PTSS2 ~~ c_PTSS2
         m_PTSS3 ~~ c_PTSS3
         m_PTSS4 ~~ c_PTSS4
         
         #variances
         m_PTSS1 ~~ m_PTSS1
         m_PTSS2 ~~ m_PTSS2
         m_PTSS3 ~~ m_PTSS3
         m_PTSS4 ~~ m_PTSS4
         
         m_HRQoL1 ~~ m_HRQoL1
         m_HRQoL2 ~~ m_HRQoL2
         m_HRQoL3 ~~ m_HRQoL3
         m_HRQoL4 ~~ m_HRQoL4
         
         c_PTSS1 ~~ c_PTSS1
         c_PTSS2 ~~ c_PTSS2
         c_PTSS3 ~~ c_PTSS3
         c_PTSS4 ~~ c_PTSS4
         
         c_HRQoL1 ~~ c_HRQoL1
         c_HRQoL2 ~~ c_HRQoL2
         c_HRQoL3 ~~ c_HRQoL3
         c_HRQoL4 ~~ c_HRQoL4
         
'

fit_full1 <- sem(Model1_combined_PSHS, data = data_sem_scaled, estimator = "MLR", missing = "fiml")
summary(fit_full1, fit.measures= TRUE, standardized=TRUE, rsquare=TRUE)

#Model 2 - partner paths from mothers to children
Model2_combined_PSHS<-'
          #latent variables child
         c_PTSS1 =~ 1*T1_tscyc_PTSTOT_mc 
         c_PTSS2 =~ 1*T2_tscyc_PTSTOT_mc 
         c_PTSS3 =~ 1*T3_tscyc_PTSTOT_mc 
         c_PTSS4 =~ 1*T4_tscyc_PTSTOT_mc 
         
         c_HRQoL1 =~ 1*T1_pedsPSHS_mc
         c_HRQoL2 =~ 1*T2_pedsPSHS_mc
         c_HRQoL3 =~ 1*T3_pedsPSHS_mc
         c_HRQoL4 =~ 1*T4_pedsPSHS_mc
         
         #latent variables mother
         m_PTSS1 =~ 1*T1_pds_ss_mc
         m_PTSS2 =~ 1*T2_pds_ss_mc
         m_PTSS3 =~ 1*T3_pds_ss_mc
         m_PTSS4 =~ 1*T4_pds_ss_mc
         
         m_HRQoL1 =~ 1*T1_MCS12
         m_HRQoL2 =~ 1*T2_MCS12
         m_HRQoL3 =~ 1*T3_MCS12
         m_HRQoL4 =~ 1*T4_MCS12
          #mother effects
           #PTSS
         m_PTSS2 ~ m_PTSS1 + b2*m_HRQoL1
         m_PTSS3 ~ m_PTSS2 + b2*m_HRQoL2
         m_PTSS4 ~ m_PTSS3 + b2*m_HRQoL3
         #HRQoL
         m_HRQoL2 ~ m_HRQoL1 + b1*m_PTSS1
         m_HRQoL3 ~ m_HRQoL2 + b1*m_PTSS2 
         m_HRQoL4 ~ m_HRQoL3 + b1*m_PTSS3
         #child effects
         #PTSS
         c_PTSS2 ~ c_PTSS1 + b3*c_HRQoL1 + b4*m_PTSS1 + b5*m_HRQoL1
         c_PTSS3 ~ c_PTSS2 + b3*c_HRQoL2 + b4*m_PTSS2 + b5*m_HRQoL2
         c_PTSS4 ~ c_PTSS3 + b3*c_HRQoL3 + b4*m_PTSS3 + b5*m_HRQoL3
         #HRQoL
         c_HRQoL2 ~ c_HRQoL1 + b6*c_PTSS1 + b7*m_PTSS1 + b8*m_HRQoL1
         c_HRQoL3 ~ c_HRQoL2 + b6*c_PTSS2 + b7*m_PTSS2 + b8*m_HRQoL2
         c_HRQoL4 ~ c_HRQoL3 + b6*c_PTSS3 + b7*m_PTSS3 + b8*m_HRQoL3
         #correlations
         #mother
         m_PTSS1 ~~ m_HRQoL1
         m_PTSS2 ~~ m_HRQoL2
         m_PTSS3 ~~ m_HRQoL3
         m_PTSS4 ~~ m_HRQoL4
         
         #child
         c_PTSS1 ~~ c_HRQoL1
         c_PTSS2 ~~ c_HRQoL2
         c_PTSS3 ~~ c_HRQoL3
         c_PTSS4 ~~ c_HRQoL4
         
         #mother/child
         m_HRQoL1 ~~ c_PTSS1
         m_HRQoL2 ~~ c_PTSS2
         m_HRQoL3 ~~ c_PTSS3
         m_HRQoL4 ~~ c_PTSS4
         
         c_HRQoL1 ~~ m_PTSS1
         c_HRQoL2 ~~ m_PTSS2
         c_HRQoL3 ~~ m_PTSS3
         c_HRQoL4 ~~ m_PTSS4
         
         m_HRQoL1 ~~ c_HRQoL1
         m_HRQoL2 ~~ c_HRQoL2
         m_HRQoL3 ~~ c_HRQoL3
         m_HRQoL4 ~~ c_HRQoL4
         
         m_PTSS1 ~~ c_PTSS1
         m_PTSS2 ~~ c_PTSS2
         m_PTSS3 ~~ c_PTSS3
         m_PTSS4 ~~ c_PTSS4
         
         #variances
         m_PTSS1 ~~ m_PTSS1
         m_PTSS2 ~~ m_PTSS2
         m_PTSS3 ~~ m_PTSS3
         m_PTSS4 ~~ m_PTSS4
         
         m_HRQoL1 ~~ m_HRQoL1
         m_HRQoL2 ~~ m_HRQoL2
         m_HRQoL3 ~~ m_HRQoL3
         m_HRQoL4 ~~ m_HRQoL4
         
         c_PTSS1 ~~ c_PTSS1
         c_PTSS2 ~~ c_PTSS2
         c_PTSS3 ~~ c_PTSS3
         c_PTSS4 ~~ c_PTSS4
         
         c_HRQoL1 ~~ c_HRQoL1
         c_HRQoL2 ~~ c_HRQoL2
         c_HRQoL3 ~~ c_HRQoL3
         c_HRQoL4 ~~ c_HRQoL4
         
'

fit_full2 <- sem(Model2_combined_PSHS, data = data_sem_scaled, estimator = "MLR", missing = "fiml")
summary(fit_full2, fit.measures= TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit_full2, standardized = TRUE)
standardizedsolution(fit_full2, level=.95)
#compare model 1 and model 2
anova(fit_full1, fit_full2, method = "satorra.bentler.2010")

#Model 3 - partner paths from children to mothers only
Model3_combined_PSHS<-'
          #latent variables child
         c_PTSS1 =~ 1*T1_tscyc_PTSTOT_mc 
         c_PTSS2 =~ 1*T2_tscyc_PTSTOT_mc 
         c_PTSS3 =~ 1*T3_tscyc_PTSTOT_mc 
         c_PTSS4 =~ 1*T4_tscyc_PTSTOT_mc 
         
         c_HRQoL1 =~ 1*T1_pedsPSHS_mc
         c_HRQoL2 =~ 1*T2_pedsPSHS_mc
         c_HRQoL3 =~ 1*T3_pedsPSHS_mc
         c_HRQoL4 =~ 1*T4_pedsPSHS_mc
         
         #latent variables mother
         m_PTSS1 =~ 1*T1_pds_ss_mc
         m_PTSS2 =~ 1*T2_pds_ss_mc
         m_PTSS3 =~ 1*T3_pds_ss_mc
         m_PTSS4 =~ 1*T4_pds_ss_mc
         
         m_HRQoL1 =~ 1*T1_MCS12
         m_HRQoL2 =~ 1*T2_MCS12
         m_HRQoL3 =~ 1*T3_MCS12
         m_HRQoL4 =~ 1*T4_MCS12
          #mother effects
           #PTSS
         m_PTSS2 ~ m_PTSS1 + b2*m_HRQoL1 + b4*c_PTSS1 + b5*c_HRQoL1
         m_PTSS3 ~ m_PTSS2 + b2*m_HRQoL2 + b4*c_PTSS2 + b5*c_HRQoL2
         m_PTSS4 ~ m_PTSS3 + b2*m_HRQoL3 + b4*c_PTSS3 + b5*c_HRQoL3
         #HRQoL
         m_HRQoL2 ~ m_HRQoL1 + b1*m_PTSS1 + b7*c_PTSS1 + b8*c_HRQoL1
         m_HRQoL3 ~ m_HRQoL2 + b1*m_PTSS2 + b7*c_PTSS2 + b8*c_HRQoL2
         m_HRQoL4 ~ m_HRQoL3 + b1*m_PTSS3 + b7*c_PTSS3 + b8*c_HRQoL3
         #child effects
         #PTSS
         c_PTSS2 ~ c_PTSS1 + b3*c_HRQoL1  
         c_PTSS3 ~ c_PTSS2 + b3*c_HRQoL2 
         c_PTSS4 ~ c_PTSS3 + b3*c_HRQoL3 
         #HRQoL
         c_HRQoL2 ~ c_HRQoL1 + b6*c_PTSS1 
         c_HRQoL3 ~ c_HRQoL2 + b6*c_PTSS2 
         c_HRQoL4 ~ c_HRQoL3 + b6*c_PTSS3  
         #correlations
         #mother
         m_PTSS1 ~~ m_HRQoL1
         m_PTSS2 ~~ m_HRQoL2
         m_PTSS3 ~~ m_HRQoL3
         m_PTSS4 ~~ m_HRQoL4
         
         #child
         c_PTSS1 ~~ c_HRQoL1
         c_PTSS2 ~~ c_HRQoL2
         c_PTSS3 ~~ c_HRQoL3
         c_PTSS4 ~~ c_HRQoL4
         
         #mother/child
         m_HRQoL1 ~~ c_PTSS1
         m_HRQoL2 ~~ c_PTSS2
         m_HRQoL3 ~~ c_PTSS3
         m_HRQoL4 ~~ c_PTSS4
         
         c_HRQoL1 ~~ m_PTSS1
         c_HRQoL2 ~~ m_PTSS2
         c_HRQoL3 ~~ m_PTSS3
         c_HRQoL4 ~~ m_PTSS4
         
         m_HRQoL1 ~~ c_HRQoL1
         m_HRQoL2 ~~ c_HRQoL2
         m_HRQoL3 ~~ c_HRQoL3
         m_HRQoL4 ~~ c_HRQoL4
         
         m_PTSS1 ~~ c_PTSS1
         m_PTSS2 ~~ c_PTSS2
         m_PTSS3 ~~ c_PTSS3
         m_PTSS4 ~~ c_PTSS4
         
         #variances
         m_PTSS1 ~~ m_PTSS1
         m_PTSS2 ~~ m_PTSS2
         m_PTSS3 ~~ m_PTSS3
         m_PTSS4 ~~ m_PTSS4
         
         m_HRQoL1 ~~ m_HRQoL1
         m_HRQoL2 ~~ m_HRQoL2
         m_HRQoL3 ~~ m_HRQoL3
         m_HRQoL4 ~~ m_HRQoL4
         
         c_PTSS1 ~~ c_PTSS1
         c_PTSS2 ~~ c_PTSS2
         c_PTSS3 ~~ c_PTSS3
         c_PTSS4 ~~ c_PTSS4
         
         c_HRQoL1 ~~ c_HRQoL1
         c_HRQoL2 ~~ c_HRQoL2
         c_HRQoL3 ~~ c_HRQoL3
         c_HRQoL4 ~~ c_HRQoL4
         
'
         


fit_full3 <- sem(Model3_combined_PSHS, data = data_sem_scaled, estimator = "MLR", missing = "fiml")
summary(fit_full3, fit.measures= TRUE, standardized=TRUE, rsquare=TRUE)

#compare model 1 and model 3
anova(fit_full1, fit_full3, method = "satorra.bentler.2010")

#Model 4 - all actor and partner paths
Model4_combined_PSHS<-'
          #latent variables child
         c_PTSS1 =~ 1*T1_tscyc_PTSTOT_mc 
         c_PTSS2 =~ 1*T2_tscyc_PTSTOT_mc 
         c_PTSS3 =~ 1*T3_tscyc_PTSTOT_mc 
         c_PTSS4 =~ 1*T4_tscyc_PTSTOT_mc 
         
         c_HRQoL1 =~ 1*T1_pedsPSHS_mc
         c_HRQoL2 =~ 1*T2_pedsPSHS_mc
         c_HRQoL3 =~ 1*T3_pedsPSHS_mc
         c_HRQoL4 =~ 1*T4_pedsPSHS_mc
         
         #latent variables mother
         m_PTSS1 =~ 1*T1_pds_ss_mc
         m_PTSS2 =~ 1*T2_pds_ss_mc
         m_PTSS3 =~ 1*T3_pds_ss_mc
         m_PTSS4 =~ 1*T4_pds_ss_mc
         
         m_HRQoL1 =~ 1*T1_MCS12
         m_HRQoL2 =~ 1*T2_MCS12
         m_HRQoL3 =~ 1*T3_MCS12
         m_HRQoL4 =~ 1*T4_MCS12
          #mother effects
           #PTSS
         m_PTSS2 ~ m_PTSS1 + b1*m_HRQoL1 + b2*c_PTSS1 + b3*c_HRQoL1
         m_PTSS3 ~ m_PTSS2 + b1*m_HRQoL2 + b2*c_PTSS2 + b3*c_HRQoL2
         m_PTSS4 ~ m_PTSS3 + b1*m_HRQoL3 + b2*c_PTSS3 + b3*c_HRQoL3
         #HRQoL
         m_HRQoL2 ~ m_HRQoL1 + b4*m_PTSS1 + b5*c_PTSS1 + b6*c_HRQoL1
         m_HRQoL3 ~ m_HRQoL2 + b4*m_PTSS2 + b5*c_PTSS2 + b6*c_HRQoL2
         m_HRQoL4 ~ m_HRQoL3 + b4*m_PTSS3 + b5*c_PTSS3 + b6*c_HRQoL3
         #child effects
         #PTSS
         c_PTSS2 ~ c_PTSS1 + b7*c_HRQoL1  + b8*m_PTSS1 + b9*m_HRQoL1
         c_PTSS3 ~ c_PTSS2 + b7*c_HRQoL2  + b8*m_PTSS2 + b9*m_HRQoL2
         c_PTSS4 ~ c_PTSS3 + b7*c_HRQoL3  + b8*m_PTSS3 + b9*m_HRQoL3
         #HRQoL
         c_HRQoL2 ~ c_HRQoL1 + b10*c_PTSS1 + b11*m_PTSS1 + b12*m_HRQoL1
         c_HRQoL3 ~ c_HRQoL2 + b10*c_PTSS2 + b11*m_PTSS2 + b12*m_HRQoL2
         c_HRQoL4 ~ c_HRQoL3 + b10*c_PTSS3 + b11*m_PTSS3 + b12*m_HRQoL3 
         #correlations
         #mother
         m_PTSS1 ~~ m_HRQoL1
         m_PTSS2 ~~ m_HRQoL2
         m_PTSS3 ~~ m_HRQoL3
         m_PTSS4 ~~ m_HRQoL4
         
         #child
         c_PTSS1 ~~ c_HRQoL1
         c_PTSS2 ~~ c_HRQoL2
         c_PTSS3 ~~ c_HRQoL3
         c_PTSS4 ~~ c_HRQoL4
         
         #mother/child
         m_HRQoL1 ~~ c_PTSS1
         m_HRQoL2 ~~ c_PTSS2
         m_HRQoL3 ~~ c_PTSS3
         m_HRQoL4 ~~ c_PTSS4
         
         c_HRQoL1 ~~ m_PTSS1
         c_HRQoL2 ~~ m_PTSS2
         c_HRQoL3 ~~ m_PTSS3
         c_HRQoL4 ~~ m_PTSS4
         
         m_HRQoL1 ~~ c_HRQoL1
         m_HRQoL2 ~~ c_HRQoL2
         m_HRQoL3 ~~ c_HRQoL3
         m_HRQoL4 ~~ c_HRQoL4
         
         m_PTSS1 ~~ c_PTSS1
         m_PTSS2 ~~ c_PTSS2
         m_PTSS3 ~~ c_PTSS3
         m_PTSS4 ~~ c_PTSS4
         
         #variances
         m_PTSS1 ~~ m_PTSS1
         m_PTSS2 ~~ m_PTSS2
         m_PTSS3 ~~ m_PTSS3
         m_PTSS4 ~~ m_PTSS4
         
         m_HRQoL1 ~~ m_HRQoL1
         m_HRQoL2 ~~ m_HRQoL2
         m_HRQoL3 ~~ m_HRQoL3
         m_HRQoL4 ~~ m_HRQoL4
         
         c_PTSS1 ~~ c_PTSS1
         c_PTSS2 ~~ c_PTSS2
         c_PTSS3 ~~ c_PTSS3
         c_PTSS4 ~~ c_PTSS4
         
         c_HRQoL1 ~~ c_HRQoL1
         c_HRQoL2 ~~ c_HRQoL2
         c_HRQoL3 ~~ c_HRQoL3
         c_HRQoL4 ~~ c_HRQoL4
         
'

fit_full4 <- sem(Model4_combined_PSHS, data = data_sem_scaled, estimator = "MLR", missing = "fiml")
summary(fit_full4, fit.measures= TRUE, standardized=TRUE, rsquare=TRUE)

#compare model 1 and model 4
anova(fit_full1, fit_full4, method = "satorra.bentler.2010")
#compare model 2 and model 4
anova(fit_full2, fit_full4, method = "satorra.bentler.2010")
#compare model 3 and model 4
anova(fit_full3, fit_full4, method = "satorra.bentler.2010")


###Final model APIM (Model 2) with added covariates: Mental HRQoL for mothers, psychosocial HRQoL for children
Model2_combined_PSHS_cov<-'
           #latent variables child
         c_PTSS1 =~ 1*T1_tscyc_PTSTOT_mc 
         c_PTSS2 =~ 1*T2_tscyc_PTSTOT_mc 
         c_PTSS3 =~ 1*T3_tscyc_PTSTOT_mc 
         c_PTSS4 =~ 1*T4_tscyc_PTSTOT_mc 
         
         c_HRQoL1 =~ 1*T1_pedsPSHS_mc
         c_HRQoL2 =~ 1*T2_pedsPSHS_mc
         c_HRQoL3 =~ 1*T3_pedsPSHS_mc
         c_HRQoL4 =~ 1*T4_pedsPSHS_mc
         
         #latent variables mother
         m_PTSS1 =~ 1*T1_pds_ss_mc
         m_PTSS2 =~ 1*T2_pds_ss_mc
         m_PTSS3 =~ 1*T3_pds_ss_mc
         m_PTSS4 =~ 1*T4_pds_ss_mc
         
         m_HRQoL1 =~ 1*T1_MCS12
         m_HRQoL2 =~ 1*T2_MCS12
         m_HRQoL3 =~ 1*T3_MCS12
         m_HRQoL4 =~ 1*T4_MCS12
         
         
          #mother effects
          #T1 controls
         m_PTSS1 ~ age_adm+PICULOS
         m_HRQoL1 ~ age_adm+PICULOS
           #PTSS
         m_PTSS2 ~ m_PTSS1 + b2*m_HRQoL1
         m_PTSS3 ~ m_PTSS2 + b2*m_HRQoL2
         m_PTSS4 ~ m_PTSS3 + b2*m_HRQoL3
         #HRQoL
         m_HRQoL2 ~ m_HRQoL1 + b1*m_PTSS1
         m_HRQoL3 ~ m_HRQoL2 + b1*m_PTSS2 
         m_HRQoL4 ~ m_HRQoL3 + b1*m_PTSS3
         #child effects
         #T1 controls
         c_PTSS1 ~ age_adm+PICULOS
         c_HRQoL1 ~ age_adm+PICULOS
         #PTSS
         c_PTSS2 ~ c_PTSS1 + b3*c_HRQoL1 + b4*m_PTSS1 + b5*m_HRQoL1
         c_PTSS3 ~ c_PTSS2 + b3*c_HRQoL2 + b4*m_PTSS2 + b5*m_HRQoL2
         c_PTSS4 ~ c_PTSS3 + b3*c_HRQoL3 + b4*m_PTSS3 + b5*m_HRQoL3
         #HRQoL
         c_HRQoL2 ~ c_HRQoL1 + b6*c_PTSS1 + b7*m_PTSS1 + b8*m_HRQoL1
         c_HRQoL3 ~ c_HRQoL2 + b6*c_PTSS2 + b7*m_PTSS2 + b8*m_HRQoL2
         c_HRQoL4 ~ c_HRQoL3 + b6*c_PTSS3 + b7*m_PTSS3 + b8*m_HRQoL3
         #correlations
         #mother
         m_PTSS1 ~~ m_HRQoL1
         m_PTSS2 ~~ m_HRQoL2
         m_PTSS3 ~~ m_HRQoL3
         m_PTSS4 ~~ m_HRQoL4
         
         #child
         c_PTSS1 ~~ c_HRQoL1
         c_PTSS2 ~~ c_HRQoL2
         c_PTSS3 ~~ c_HRQoL3
         c_PTSS4 ~~ c_HRQoL4
         
         #mother/child
         m_HRQoL1 ~~ c_PTSS1
         m_HRQoL2 ~~ c_PTSS2
         m_HRQoL3 ~~ c_PTSS3
         m_HRQoL4 ~~ c_PTSS4
         
         c_HRQoL1 ~~ m_PTSS1
         c_HRQoL2 ~~ m_PTSS2
         c_HRQoL3 ~~ m_PTSS3
         c_HRQoL4 ~~ m_PTSS4
         
         m_HRQoL1 ~~ c_HRQoL1
         m_HRQoL2 ~~ c_HRQoL2
         m_HRQoL3 ~~ c_HRQoL3
         m_HRQoL4 ~~ c_HRQoL4
         
         m_PTSS1 ~~ c_PTSS1
         m_PTSS2 ~~ c_PTSS2
         m_PTSS3 ~~ c_PTSS3
         m_PTSS4 ~~ c_PTSS4
         
         #variances
         m_PTSS1 ~~ m_PTSS1
         m_PTSS2 ~~ m_PTSS2
         m_PTSS3 ~~ m_PTSS3
         m_PTSS4 ~~ m_PTSS4
         
         m_HRQoL1 ~~ m_HRQoL1
         m_HRQoL2 ~~ m_HRQoL2
         m_HRQoL3 ~~ m_HRQoL3
         m_HRQoL4 ~~ m_HRQoL4
         
         c_PTSS1 ~~ c_PTSS1
         c_PTSS2 ~~ c_PTSS2
         c_PTSS3 ~~ c_PTSS3
         c_PTSS4 ~~ c_PTSS4
         
         c_HRQoL1 ~~ c_HRQoL1
         c_HRQoL2 ~~ c_HRQoL2
         c_HRQoL3 ~~ c_HRQoL3
         c_HRQoL4 ~~ c_HRQoL4
         
'


fit_full2_PSHS_cov <- sem(Model2_combined_PSHS_cov, data =sub_all_data, estimator = "MLR", missing = "fiml")
summary(fit_full2_PSHS_cov, fit.measures= TRUE, standardized=TRUE, rsquare=TRUE)
standardizedsolution(fit_full2_PSHS_cov, level=.95)
