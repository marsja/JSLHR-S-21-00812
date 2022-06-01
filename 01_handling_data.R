# This is the script in which we pre-process the data for our paper in Journal of Speech, Language, and Hearing Research (JSLHR-S-21-00812)
# Stenbäck, Marsja, Hällgren, Lyxell, & Larsby
# Essentially, we are restructuring (transforming) the data from long to  wide
# But we are also adding group and creating the mask type variable, and so on




library(tidyverse)

# Import Data from SPSS file (not available online): 
df_ms4 <- haven::read_sav('Mastersheetstudy4.sav') %>%
  # We don't need all variables in the sheet
  select(-c(HayDN5:HayISTS0))

# Data were not grouped:
# ENH = Older adults with normal hearing
# HI = Older adults with hearing impairment
df_ms4$Group <- c(rep("ENH", 24), 
                  rep("HI", 24)) %>% as_factor() 


# Get number of males and females
df_ms4 %>% group_by(Group, Sex) %>%
    summarise(Gender = n())

# Get number of males and females
df_ms4 %>% select(Group, Age) %>%
  psych::describeBy("Group")

# HI group need unique IDs
df_ms4 <- df_ms4 %>%
  mutate(Participant = as_factor(if_else(Group == "HI", Participant + 100, Participant)))

# Just getting the average PTA4
df_ms4 %>%
  group_by(Group) %>%
  summarise(PTA4 = mean(PTA4, na.rm = T))

# Renaming variables to make transformation of data easier (wide to long)
df_ms4 <- df_ms4 %>%
  rename(
    Hag_SSN = HagSSN,
    Hag_Mod = HagMod,
    Hag_ISTS = HagISTS,
    Hag_FT = HagFT,
    HINT_SSN = HINTSSN,
    HINT_Mod = HINTMod,
    HINT_ISTS = HINTISTS,
    HINT_FT = HINTFT,
    BorgHag_SSN = BorgHagSSN,
    BorgHag_FT = BorgHagDN,
    BorgHag_ISTS = BorgHagISTS,
    BorgHag_Mod = BorgHagMod,
    BorgHint_SSN = BorgHintSSN,
    BorgHint_FT = BorgHintDN,
    BorgHint_ISTS = BorgHintISTS,
    BorgHint_Mod = BorgHintMod
    
  )


# Transforming data to long format (needed for linear mixed effects modeling)
# SNR is the dependent variable here:
df_ms4.long <- df_ms4 %>%
  # Select the variables of interest
  dplyr::select(c(Participant, Age, Group, 
                  ReadingSpan, HaylingRT1_error , 
                  HaylingRT2_error, Inihibition, 
                  PTA4, Hag_SSN:HINT_FT,
                  BorgHag_SSN:BorgHint_Mod)) %>%
  # Transform data to long format
  tidyr::pivot_longer(
    cols = Hag_SSN:HINT_FT,
    # The new columns will be Material (Hagerman or HINT) and Noise
    names_to = c("Material", "Noise"),
    names_sep = "_",
    values_to = "SNR"
  ) %>%
  # Create the new variable (column) MaskType
  mutate(MaskType = if_else(Noise == "FT", "Informational", 
                            if_else(Noise == "ISTS", "Informational", "Energetic"))) %>%
  # Making categorical variables/factors of all containing characters
  mutate_if(is.character, as.factor) %>% 
  # Saving the data for analysis
  saveRDS("Data_SNR.Rds")


# Transforming data to long format (needed for linear mixed effects modeling)
# Effort
df_ms4 %>%
  dplyr::select(c(Participant, Age, Group, ReadingSpan,
                  HaylingRT1_error, HaylingRT2_error,
                  PTA4, Hag_SSN:HINT_FT,
                  BorgHag_SSN:BorgHint_Mod)) %>%
  tidyr::pivot_longer(
    cols = BorgHag_SSN:BorgHint_Mod ,
    names_to = c("Material", "Noise"),
    names_sep = "_",
    values_to = "Effort"
  ) %>%
  mutate(Material = stringr::str_replace(Material, "Borg", "")) %>%
  select(-c(Hag_SSN:HINT_FT)) %>% 
  mutate(MaskType = if_else(Noise == "FT", "Informational", 
                                                            if_else(Noise == "ISTS", "Informational", "Energetic"))) %>%
  mutate_if(is.character, as.factor) %>%
  saveRDS("Data_Effort.Rds")