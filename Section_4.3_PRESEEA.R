#Carlota de Benito Moreno
#From (semi-)oppositional to non-oppositional middles: the case of Spanish reír(se)
#Section 4.3 (data gfrom PRESEEA)
#February 11th 2021, 1.0
#August 30th 2021, 2.0


#load libraries
library(tidyverse)

#load files
preseea <- read_delim("Preseea_reirse.csv", delim = ";")

##Number and proportion of MM cases depending on the object
preseea %>% 
  filter(Valido == "si") %>% 
  filter(Reflexivo %in% c("si", "no")) %>% #excluding ambiguous examples
  filter(Perifrasis %in% c("no_reflexiva", "reflexiva", NA)) %>% #excluding semicausativa (perception/permission control verbs)
  mutate(Complemento_de =  ifelse(Complemento_de == "si", "de", "other")) %>% 
  count(Complemento_de, Reflexivo) %>% 
  group_by(Complemento_de) %>% 
  mutate(perc_refl = n/sum(n)*100)

