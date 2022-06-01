# This is the script in which we carry out the main part of the data analysis in our paper in Journal of Speech, Language, and Hearing Research (JSLHR-S-21-00812)
# Stenbäck, Marsja, Hällgren, Lyxell, & Larsby



library(tidyverse)

# Let's clean the environment:
rm(list=ls())

# SNR as DV
df_l <- readRDS("Data_SNR.Rds") %>% 
  mutate(HayInhibition = (HaylingRT1_error + HaylingRT2_error)/2,
         ReadingSpan_scaled = scale(ReadingSpan)[,1],
         PTA4_scaled = scale(PTA4)[,1],
         HayRT2_error_scaled = scale(HaylingRT2_error))

# Effort as DV
df_e <- readRDS("Data_Effort.Rds") %>% 
  mutate(HayInhibition = (HaylingRT1_error + HaylingRT2_error)/2,
         ReadingSpan_scaled = scale(ReadingSpan)[,1],
         PTA4_scaled = scale(PTA4)[,1],
         HayRT2_error_scaled = scale(HaylingRT2_error))
library(lmerTest); library(lme4);library(emmeans)

# The full maximal model not converging
# m_maximal <- lmer(SNR ~ 1
#                   # Q1:
#                   + MaskType*ReadingSpan_scaled*Age +
#                     + MaskType*HayRT2_error_scaled*Age +
#                     + MaskType*HayRT2_error_scaled*PTA4_scaled
#                   + MaskType*ReadingSpan_scaled*PTA4_scaled
#                   # Random Effects (by-participant)
#                   + (HayRT2_error_scaled|Participant)
#                   + (ReadingSpan_scaled|Participant)
#                   + (MaskType|Participant) 
#                   ,REML = F,
#                   data = df_l)

# Singular fit
# m_maximal <- lmer(SNR ~ 1
#                   # Q1:
#                   + MaskType*ReadingSpan_scaled*Age +
#                     + MaskType*HayRT2_error_scaled*Age +
#                     + MaskType*HayRT2_error_scaled*PTA4_scaled
#                   + MaskType*ReadingSpan_scaled*PTA4_scaled
#                   # Random Effects (by-participant)
#                   + (HayRT2_error_scaled|Participant)
#                   + (MaskType|Participant)
#                   ,REML = F,
#                   data = df_l)

# Singular fit
# m_maximal <- lmer(SNR ~ 1
#                   # Q1:
#                   + MaskType*ReadingSpan_scaled*Age +
#                     + MaskType*HayRT2_error_scaled*Age +
#                     # Covariate:
#                     + MaskType*HayRT2_error_scaled*PTA4_scaled
#                   + MaskType*ReadingSpan_scaled*PTA4_scaled
#                   # Random Effects (by-participant)
#                   + (HayRT2_error_scaled|Participant)
#                   + (ReadingSpan_scaled|Participant)
#                   ,REML = F,
#                   data = df_l)

# Maximal model presented in paper
m_maximal <- lmer(SNR ~ 1
                  + MaskType*ReadingSpan_scaled*Age +
                    + MaskType*HayRT2_error_scaled*Age +
                    + MaskType*HayRT2_error_scaled*PTA4_scaled
                  + MaskType*ReadingSpan_scaled*PTA4_scaled
                  # Random Effects (by-participant)
                  + (HayRT2_error_scaled|Participant)
                  ,REML = F,
                  data = df_l)



# Testing the inclusion of both fixed and random effects:
stepw.snr <- step(m_maximal)

# Get the final model
final_model.snr <- get_model(stepw.snr)

sjPlot::tab_model(final_model.snr, show.stat = TRUE, show.se = TRUE,
                  title = "Table X. Fixed effects estimates for SNR  as dependent variable",
                  show.r2 = TRUE, p.style = c('numeric_stars'),
                  string.est = "Estimate",
                  string.se = "SE",
                  string.stat = "t",
                  string.intercept = "Intercept",
                  dv.labels = c("Effort"),
                  file = "LMM_model_table_Q1_rev.doc")

# Saving for Creating the table in Suppl. 1
saveRDS(stepw.snr, "stepw_deletion_SNR_revision.RDs")


# Data visualisation:

# Interactions: https://rstudio-pubs-static.s3.amazonaws.com/187243_e55ff9b4d78a4eb5974fc7d4a36fc0ce.html

effs <- effects::allEffects(final_model.snr)
effs.df <- as.data.frame(effs)

g <- ggplot(effs.df$`MaskType:HayRT2_error_scaled`,
            aes(x=HayRT2_error_scaled,y=fit,
                colour=MaskType, shape=MaskType,
                ymin=lower,ymax=upper), size = 1) + 
  geom_pointrange(position=position_dodge(width=.1)) + 
  geom_line(position=position_dodge(width=.1), size = 1) + 
  scale_colour_grey() +
  papaja::theme_apa()  +
  
  xlab("Inhibitory Control (Scaled)") + ylab("SNR (dB) HL") + 
  ggtitle("Effect of Inhibitory Control and Mask type on SNR")   + labs(colour = "Mask type",
                                                                  shape = "Mask type") 
plot(g)
ggsave("Figure2-MtypeByInhibitoryC.tiff", dpi = 300)

## Effort:

# Maximal model
m_maximal.effort <- lmer(Effort ~ 1 + 
                             # FE interactions:
                             MaskType*Material*PTA4_scaled*Age +
                             MaskType:ReadingSpan_scaled:PTA4_scaled:Age + 
                             MaskType*HayRT2_error_scaled*PTA4_scaled*Age +
                             Material*ReadingSpan_scaled*PTA4_scaled*Age + 
                             Material*HayRT2_error_scaled*PTA4_scaled*Age + 
                             # Random intercept by S:
                             #(1|Participant),
                             # R slopes
                             # Cannot converge (MaskType|Participant) +
                             
                             # Cannot converge (Material|Participant) +
                             # (ReadingSpan_scaled|Participant) +
                             (1|Participant),
                           REML = F,
                           data = df_e)
# Testing the inclusion of both fixed and random effects:
stepw.eff <- lmerTest::step(m_maximal.effort)

# Get the final model
final_model.eff <- get_model(stepw.eff)

sjPlot::tab_model(final_model.eff, show.stat = TRUE, show.se = TRUE,
                  title = "Table X. Fixed effects estimates for Effort  as dependent variable",
                  show.r2 = TRUE, p.style = c('numeric_stars'),
                  string.est = "Estimate",
                  string.se = "SE",
                  string.stat = "t",
                  string.intercept = "Intercept",
                  dv.labels = c("Effort"),
                  file = "LMM_model_table_Q2_rev.doc")

# Save to greate table in Suppl. 2
saveRDS(stepw.eff, "stepw_deletion_EFF_revision.RDs")