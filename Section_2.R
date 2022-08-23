#Carlota de Benito Moreno
#From (semi-)oppositional to non-oppositional middles: the case of Spanish reír(se)
#Section 2
#August 26th 2021


#Section 2

#load libraries
library(tidyverse)

####
#Figure 1 (adapted from de Benito Moreno 2021)

#load table
full_table <- read_delim("https://raw.githubusercontent.com/Carlotadbm/Middle_voice_IberoRomance/main/VM_fusion_blanco_revisado_3.csv", delim="\t") #COSER linguistic data

#clean table
intransitive_reciprocal <- full_table %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No MM", "MM")) %>%
  filter(Tipo_sintactico %in% c("Intransitivo", "Intransitivo_Anticausativo")) %>%
  filter(Tipo_semantico == "Reciproco_natural") %>%
  mutate(Verbo = ifelse(str_detect(Verbo, "rennir"), "reñir", Verbo)) %>%
  count(Verbo, Pron_reflexivo) %>%
  group_by(Verbo) %>%
  mutate(total=sum(n), prop=(n/total*100)) %>% 
  filter(!Verbo %in% c("colaborar", "coincidir"))

##relevel Verbs 
intransitive_reciprocal$Verbo <- factor(intransitive_reciprocal$Verbo, 
                                        levels = c("pelear", "luchar", "reñir", "discutir"))

intransitive_reciprocal$Pron_reflexivo <- factor(intransitive_reciprocal$Pron_reflexivo, 
                                        levels = c("No MM", "MM"))

##create plot
ggplot(intransitive_reciprocal, aes(x=Verbo,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Naturally reciprocal intransitive verbs", x="Verb", y="Presence of the MM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



####
#Figure 2 (adapted from de Benito Moreno 2021)

##clean table
oppositional_and_semi <- full_table %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No MM", "MM")) %>% 
  mutate(Tipo_sintactico = ifelse(Tipo_sintactico %in% c("Intransitivo", "Intransitivo_Anticausativo"), "Semi-oppositional", 
                                  ifelse(Tipo_sintactico %in% c("Anticausativo", "Antipasiva_CR", "Inverso", "Anticausativo_discontinuo", 
                                                         "Deobjetivo", "Antipasiva_SO", "Absoluto", "Ambiguo"), "Oppositional", Tipo_sintactico))) %>% 
  filter(str_detect(Tipo_sintactico, "[Oo]ppositional")) %>% 
  mutate(Tipo_semantico = ifelse(is.na(Tipo_semantico), "Other", Tipo_semantico)) %>% 
  mutate(Tipo_semantico = ifelse(Tipo_semantico %in% c("Discursivo", "Pseudopasiva", "Pseudocopulativo", "Other"), "Other", "Kemmer's situation types")) 

##summarise data
oppositional_and_semi_summary <- oppositional_and_semi %>% 
  group_by(Tipo_sintactico, Tipo_semantico) %>% 
  count(Verbo) %>%
  select(-n) %>%
  count(Tipo_semantico) %>%
  arrange(desc(n)) %>%
  ungroup(Tipo_semantico) %>%
  mutate(total = sum(n), percentage = round(n*100/total, 1)) 

##create plot
oppositional_and_semi_summary %>% 
  ggplot(aes(x=Tipo_semantico, y=percentage)) + 
  geom_col(aes(), position = "stack", fill = "dark grey") + 
  labs(title="Distribution of middle-marked verbs by situation type", x="Situation type", y="Percentage of verbs") +
  geom_text(aes(label = n), position = position_stack(vjust = 1)) + 
  theme_classic() + 
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~ Tipo_sintactico, scales = "free_y")

