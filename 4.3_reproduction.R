# #===============================================================================
#
# Section 4.3: Reproductive effort on sap yield and sugar content at Harvard 
#              Forest
#
# script to explore relationship between reproductive effort (i.e. flower and 
# samara production) with sap yield and sap sugar content (i.e., succrose 
# concentration) based on data by Harvard Forest (Rapp et al., 2021; HF285). 
#-------------------------------------------------------------------------------

# waiting to hear from Josh before doing this ----------------------------------


# get data for HF --------------------------------------------------------------
HF_data_s <- read_csv("./data/HF/HFmaple.sap.2012_2022.csv", 
                      col_types = cols()) %>% 
  mutate(date = lubridate::as_date(date, format = "%m/%d/%Y"))
HF_data_t <- read_csv("./data/HF/HFmaple.tapping.2012_2022.csv", 
                      col_types = cols()) %>% 
  mutate(date = lubridate::as_date(date, format = "%m/%d/%Y"))
HF_data_f <- read_csv("./data/HF/HFmaple.flowering.qual.2011_2022.csv", 
                      col_types = cols())
HF_data_r <- read_csv("./data/HF/HFmaple.seedcounts.2011_2021.csv", 
                      col_types = cols()) %>% 
  mutate(date = lubridate::as_date(date, format = "%m/%d/%Y"))

# conclusions ------------------------------------------------------------------
# 
#===============================================================================