library(tidyverse)
rm(list=ls())


df_aud <- haven::read_spss('Audiogram Elderly.sav')
df_aud$ID <- seq(1, length(df_aud$Group))


# Factor level names so that we can easily change this later
# Normal Hearing:
nh = "Normal Hearing"
# Hearing Loss:
hl = "Hearing Impaired"

df_aud <- df_aud %>% 
  # Change type to factor
  mutate(Group = as_factor(Group)) %>%
  # Recode the factor levels
  mutate(Group = recode_factor(Group, "2" = nh,
                               "1" = hl)
  ) 

# Re-organize data
# Here we get the ear frequencies as rows, instead of columns, and decibel as 
# a separate column:

if("Group" %in% colnames(df_aud)){
  long_data <- gather(df_aud, key = "ear-freq", value = "dB", -Group, -ID)
} else {
  long_data <- gather(df_aud, key = "ear-freq", value = "dB")
}

# Creating a new dataframe extracting ear and frequencies from the values in the 
# long_data we created above. That is we split the characters such as we get
# columns with ear and frequencies
d <- long_data %>% 
  extract("ear-freq", into = c("Ear", "Freq"), "(Hö|Vä)(\\d+)",
          convert= TRUE) %>%
  mutate(Ear = recode_factor(Ear, "Hö" = "right",
                              "Vä" = "left"),
         dB = as.numeric(dB))


d <- na.omit(d)

# ISO thresholds:
# 125Hz-1000Hz = 20dB HL; 2000 Hz = 25dB HL, 4000Hz = 35dB HL, 6000Hz = 40db HL och 8000Hz är 45 
isonh <- as_tibble(cbind(c(125, 250, 500, 1000, 4000, 6000, 8000),
                         c(rep(20, 4), 35, 40, 45)))
# Ca
d <- d %>%
  group_by(ID, Group, Ear, Freq) %>%
  pivot_wider(names_from = Ear, values_from = dB) %>%
  mutate(Left = mean(left, na.rm = TRUE), Right = mean(right, na.rm = TRUE)) %>%
  gather(key = "Ear", value = "dB", Left, Right)

# Creating labels:
d$freqlabs <- d$Freq/1000

# Calculate CIs
cis <- d %>% 
  mutate(Ear = recode(Ear, "Left" = "Left Ear",
                      "Right" = "Right Ear")) %>%
  group_by(Ear, Freq, Group) %>%
  dplyr::summarise(mean.dB = mean(dB, na.rm = TRUE),
            sd.dB = sd(dB, na.rm = TRUE),
            n.dB = n(), .groups = "rowwise") %>%
  mutate(se.dB = sd.dB / sqrt(n.dB),
         lower.ci.dB = mean.dB - qt(1 - (0.05 / 2), n.dB - 1) * se.dB,
         upper.ci.dB = mean.dB + qt(1 - (0.05 / 2), n.dB - 1) * se.dB,
         freqlabs = Freq/1000) 


# Dodge for minimizing overlap
pd <- position_dodge(width = 0.05)
pd <- position_jitter(width = 0.01, height = 0.01)

# Audiogram grouped with CIs:
audgg <- ggplot(data = cis, aes(x = Freq)) +
  geom_line(data = isonh, aes(y = V2, x = V1, linetype = "ISO 7029:2000"), size = 1) + 
  geom_line(aes(y = mean.dB, linetype = Ear), size = 1, position = pd) + 
  geom_point(aes(y = mean.dB), size = 2, position = pd) + 
  geom_errorbar(aes(y = 0, ymin = lower.ci.dB, ymax = upper.ci.dB),
                width = 0.025, size = 0.8, position = pd) +
  
  geom_hline(yintercept = 0, size = 1.2) +
  scale_y_reverse(breaks=seq(0, 80, 5), limits=c(NA, 0)) + 
  scale_x_continuous(breaks=unique(d$Freq)) +
  scale_x_log10(breaks = cis$Freq, labels = cis$Freq) + 
  scale_linetype_manual(values = c(3, 1, 2)) +  
  jtools::theme_apa() + theme(axis.text.x = element_text(angle = 45)) +

  
  labs(x = "Frequency (Hz)", y = "Hearing Thresholds (dB HL)", linetype = "Ear", shape = "Ear")

audgg + facet_wrap(~Group)
ggsave("Figure-Audiogram_1stRevision.tiff", dpi = 300)
