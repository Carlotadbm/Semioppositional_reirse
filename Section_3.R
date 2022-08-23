#Carlota de Benito Moreno
#From (semi-)oppositional to non-oppositional middles: the case of Spanish reír(se)
#Section 3
#August 26th 2021

#Section 3

#load libraries
library(tidyverse)

####
#Table 1

#load tables
reir <- read_delim("reir_CDH.csv", delim = ";")
alegrar <- read_delim("alegrar.csv", delim = ";")
regocijar <- read_delim("regocijar.csv", delim = ";")
burlar <- read_delim("burlar.csv", delim = ";")
mofar <- read_delim("mofar.csv", delim = ";")
escarnecer <- read_delim("escarnecer.csv", delim = ";")

#Calculate total number of occurrences
reir %>% 
  count()
alegrar %>% 
  count()
regocijar %>% 
  count()
burlar %>% 
  count()
mofar %>% 
  count()
escarnecer %>% 
  count()

#Calculate number of occurrences filtering by CORDEMÁFORO
reir %>% 
  filter(!Cordemaforo %in% c("A", "R")) %>% 
  count()
alegrar %>% 
  filter(!CORDEMÁFORO %in% c("A", "R", "A_manual", "R_manual"))  %>% 
  count()
burlar %>% 
  filter(!CORDEMÁFORO %in% c("A", "R", "R_manual"))  %>% 
  count()
escarnecer %>% 
  filter(!CORDEMÁFORO %in% c("A", "R", "R_manual"))  %>% 
  count()

#Calculate total number of documents
reir %>% 
  distinct(BIBLIOGRAFIA) %>% 
  count()
alegrar %>% 
  distinct(BIBLIOGRAFÍA) %>% 
  count()
regocijar %>% 
  distinct(BIBLIOGRAFÍA) %>% 
  count()
burlar %>% 
  distinct(BIBLIOGRAFÍA) %>% 
  count()
mofar %>% 
  distinct(BIBLIOGRAFÍA) %>% 
  count()
escarnecer %>% 
  distinct(BIBLIOGRAFÍA) %>% 
  count()

#Calculate number of documents filtering by CORDEMÁFORO
reir %>% 
  filter(!Cordemaforo %in% c("A", "R")) %>% 
  distinct(BIBLIOGRAFIA) %>% 
  count()
alegrar %>% 
  filter(!CORDEMÁFORO %in% c("A", "R", "A_manual", "R_manual")) %>% 
  distinct(BIBLIOGRAFÍA) %>% 
  count()
burlar %>% 
  filter(!CORDEMÁFORO %in% c("A", "R", "R_manual")) %>% 
  distinct(BIBLIOGRAFÍA) %>% 
  count()
escarnecer %>% 
  filter(!CORDEMÁFORO %in% c("A", "R", "R_manual")) %>% 
  distinct(BIBLIOGRAFÍA) %>% 
  count()

####
#Table 2

#load tables
cdh <- read_delim("reir_1975.csv", delim = ";")
crea <- read_delim("reir_CREA.csv", delim = ";")
corpes <- read_delim("reir_CORPES.csv", delim = ";")

#calculate total occurrences
cdh %>% 
  filter(FECHA != 1975) %>% 
  count()
crea %>% 
  count()
corpes %>% 
  count()

#calculate total occurrences
cdh %>% 
  filter(FECHA != 1975) %>% 
  distinct(BIBLIOGRAFÍA) %>% 
  count()
crea %>% 
  distinct(BIBLIOGRAFÍA) %>% 
  count()
corpes %>% 
  distinct(BIBLIOGRAFÍA) %>% 
  count()


