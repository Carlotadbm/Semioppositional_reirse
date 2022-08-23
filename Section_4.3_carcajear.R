#Carlota de Benito Moreno
#From (semi-)oppositional to non-oppositional middles: the case of Spanish reír(se)
#Section 4.1 (RAE corpora)
#February 14th 2021, 1.0
#August 30th 2021, 2.0

#load libraries
library(tidyverse)

#load data
carcajear_CDH <- read_delim("carcajear_CDH.csv", delim = ";")
carcajear_CREA <- read_delim("carcajear_CREA.csv", delim = ";")
carcajear_CORPES <- read_delim("carcajear_CORPES.csv", delim = ";")

#Data description
##CDH
###Number of total examples
carcajear_CDH %>% 
  filter(Válido == "si") %>% 
  nrow()
###Number and proportion of MM cases by date
carcajear_CDH %>% 
  mutate(century = ifelse(str_detect(FECHA, "^15"), "16",
                          ifelse(str_detect(FECHA, "^16"), "17", 20))) %>% 
  filter(Válido == "si") %>% 
  count(century, FECHA, Reflexivo) %>% 
  group_by(century) %>% 
  mutate(tota = sum(n))
  
###Number and proportion of MM cases by type of object
carcajear_CDH %>% 
  filter(Válido == "si") %>% 
  count(Reflexivo, Complemento_de) 

##CREA
###range of dates
carcajear_CREA %>% 
  distinct(FECHA) #1975-2000

###Number of total examples
carcajear_CREA %>% 
  filter(Válido == "si") %>% 
  nrow()

###Number and proportion of MM cases
carcajear_CREA %>% 
  filter(Válido == "si") %>% 
  count(Reflexivo) %>% 
  mutate(refl_perc = round(n/sum(n)*100, 0), 
         total = sum(n))

###Number and proportion of MM cases by type of object
carcajear_CREA %>% 
  filter(Válido == "si") %>% 
  mutate(Complemento_de = ifelse(Complemento_de == "si", "de", Complemento_de)) %>% 
  group_by(Complemento_de) %>% 
  count(Reflexivo) %>% 
  mutate(refl_perc = round(n/sum(n)*100,0),  
         total = sum(n))


##CORPES
###range of dates
carcajear_CORPES %>% 
  distinct(FECHA) #1975-2000

###Number of total examples
carcajear_CORPES %>% 
  nrow()

###Number and proportion of MM cases
carcajear_CORPES %>% 
  count(Reflexivo) %>% 
  mutate(refl_perc = round(n/sum(n)*100, 0))

###Number and proportion of MM cases by type of object
carcajear_CORPES %>% 
  group_by(Complemento_de) %>% 
  count(Reflexivo) %>% 
  mutate(refl_perc = round(n/sum(n)*100, 0), 
         total = sum(n))

###Number and proportion of MM cases by periprhasis
carcajear_CORPES %>% 
  group_by(Perífrasis) %>% 
  count(Reflexivo) %>% 
  mutate(refl_perc = round(n/sum(n)*100, 0), 
         total = sum(n))


