install.packages("spdep")
# Set working directory if needed
#setwd("") 
getwd()

# Load packages 
packages <- c("vegan","multcomp","lme4","languageR","bbmle","ggplot2","dplyr","gridExtra",
              "glmmTMB","DHARMa","readxl","openxlsx","ppsr","tidyr","stats",
              "ggcorrplot","corrplot","ggthemes","ggeffects","MuMIn","car",
              "interactions","performance","mgcv","glmmLasso","spdep","ncf")

lapply(packages,require,character.only = TRUE)
warnings()

# Calling data frames: 
traits <- read.csv("treecotraits-edit3.csv",sep=";",head=T,as.is=T) %>% #disp mode data
            mutate(sp_id = sub(" ", ".", sp_id),
disp_mode = case_when(
  disp_mode %in% c("anemochoric", "autochoric", "anemochoric|autochoric") ~ "AneAuto",
  disp_mode %in% c("zoochoric", "autochoric|zoochoric") ~ "Zoo",
  TRUE ~ disp_mode),
eco_group = case_when(
  eco_group %in% c(1, 1.0, 1.5, 2, 2.0) ~ "early",
  eco_group %in% c(2.5, 3, 3.0, 3.5, 4, 4.0) ~ "late",
  TRUE ~ NA_character_))
             
traits %>%
  summarise(
    n_eco_group = sum(!is.na(eco_group)),
    n_disp_mode = sum(!is.na(disp_mode)),
    n_seed_size_g = sum(!is.na(seed_size_g)),
    n_habit = sum(!is.na(habit)))



landscape <- read_excel("LandscapeFinalEdited-Sep2025-Fulbert2.xlsx") # landscape data

# Merging excel tabs per trap [each tab is a study site]
sheet = excel_sheets("ZFinal-Dez23-2024.xlsx") # number of seeds per trap for each study site
data_frame = lapply(setNames(sheet, sheet), 
                    function(x) read_excel("ZFinal-Dez23-2024.xlsx", sheet=x))

data_frame = bind_rows(data_frame, .id="sheet") # merging tabs

write.xlsx(data_frame,"PerTrap-Merged-52.xlsx",
            rowNames=F,quote=F)
traps <- read.xlsx("PerTrap-Merged-52.xlsx") # reading traps

trapslonger<-pivot_longer(traps,cols=6:2633)
traps2 <- trapslonger %>% drop_na(7) %>% rename("sp_id"="name")

write.xlsx(traps2,"Longer2.xlsx",
            rowNames=F,quote=F)
traps3 <- read.xlsx("Longer2.xlsx") # reading traps longer

trapsmerged <- left_join(traps3,traits,by=c("sp_id"="sp_id"))
str(trapsmerged)

setdiff(trapsmerged$sp_id,traits$sp_id)

# Spp names - distinct (to check)
#spp_all <- trapsmerged %>% # disp mode proportions incl undet spp.
 # filter(value != 0) %>% 
  #distinct(data_owner,sp_id) 
#write.xlsx(spp_all,"Spp_All.xlsx",
 #           rowNames=F,quote=F)

#Spp_All<-read.xlsx("Spp_All.xlsx",sheet=2)
#setdiff(Spp_All$sp_id,traits$sp_id)

# Proportions of identified and unidentified spp.
#id_prop <- trapsmerged %>%
 # filter(value != 0) %>%
  #distinct(data_owner, sp_id, .keep_all = TRUE) %>%
  #group_by(data_owner) %>%
  #summarise(
   # total_species = n(),
    #w_family = sum(!is.na(family)),
    #wout_family = sum(is.na(family)),
    #.groups = 'drop') %>%
  #mutate(
   #percent_w_family = (w_family / total_species) * 100,
    #percent_wout_family = (wout_family / total_species) * 100)

id_prop <- trapsmerged %>% # two ways of doing it? # this one looks better
  filter(value != 0) %>%
  distinct(data_owner, sp_id, .keep_all = TRUE) %>%
  mutate(status = ifelse(is.na(family), "Undet", "ID")) %>%
  group_by(data_owner, status) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(data_owner) %>%
  mutate(total = sum(count),
         proportion = count / total * 100)

id_prop$status <- factor(id_prop$status, levels = c("Undet", "ID")) # order of the bars

prop_graph<-ggplot(id_prop, aes(x = data_owner, y = proportion, fill = status)) + # id spp. graphic!
  geom_bar(stat = "identity") +
  labs(title = "Proportion of identified species per study patch",
       x = "data owner",
       y = "proportion (%)",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "right",
      axis.text.x = element_text(angle = 50, vjust = 1.0, hjust = 1.0)) +
  scale_fill_manual(values = c("ID" = "blue4", "Undet" = "gray"))+
                      geom_hline(yintercept = 50, linetype = "dashed", color = "red") 

jpeg("PropSppID-Dez23-2024.jpeg", width = 20, height = 14,units = "cm", pointsize = 9,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(prop_graph)
dev.off() 



# Dispersal mode proportions
#trap_area_per_owner <- trapsmerged %>% # total trap area per site, just in case
 # group_by(data_owner) %>%
  #reframe(total_trap_area = trap_area_m2 * n_distinct(trap_id)) %>%
  #group_by(data_owner) %>%
  #summarise(total_trap_area = mean(total_trap_area, na.rm = TRUE))

disp_mode_prop <- trapsmerged %>%    # disp mode proportions incl undet spp.
  filter(value != 0, !is.na(family)) %>% 
  group_by(data_owner, disp_mode) %>%
  summarise(total_spp = n_distinct(sp_id)) %>%
  mutate(
    prop_spp = total_spp / sum(total_spp), # * 100,      # proportions of spp. calculations
          disp_mode = if_else(is.na(disp_mode), "Undet", disp_mode))


disp_mode_dens <- trapsmerged %>%    # seed density calculations - means per trap
  filter(value != 0, !is.na(family)) %>%
  group_by(data_owner, disp_mode, trap_id) %>%   # grouping by trap_id
  summarise(
    total_seeds = sum(value),
    avg_months = mean(months, na.rm = TRUE),
    trap_area = first(trap_area_m2)) %>%     # each trap area
  mutate(seed_dens_per_trap = total_seeds / trap_area / avg_months) %>%  # seed dens per trap
  group_by(data_owner, disp_mode) %>%
  summarise(seed_dens = mean(seed_dens_per_trap, na.rm = TRUE)) %>% # means of seed_dens 
  mutate(disp_mode = if_else(is.na(disp_mode), "Undet", disp_mode))

disp_mode_final <- left_join(disp_mode_dens, disp_mode_prop, by = c("data_owner", "disp_mode"))


disp_table <- left_join(disp_mode_final,landscape) %>% 
  subset(!data_owner %in% c("MaraschinDaSilvaF","MoraesLFD","MerattiOliveiraMA",
                                        "AlmeidaJrPA","RochaJ","RotherD2",
                                        "GamaM","PiottoD","FreitasCG5",
                                        "Pimentel&Zamith","MartiniA")) # excl studies w/ <50% ID spp

disp_table$locality_name<- as.factor(disp_table$locality_name)
disp_table$patch_size_log<- log(disp_table$patch_size_ha)
disp_table$seed_dens_log<- log(disp_table$seed_dens)
#disp_table$prop_spp<- as.integer(disp_table$prop_spp)

#write.xlsx(disp_table,"DispTable.xlsx",
 #           rowNames=F,quote=F)
#disp_table_read <- read.xlsx("DispTable.xlsx") #just to check on it

hist(disp_table$prop_spp)
hist(disp_table$seed_dens)
hist(log(disp_table$seed_dens))

# Barplot for dispersal mode proportions
disp_graph<-ggplot(disp_table, aes(x = data_owner, y = prop_spp, fill = disp_mode)) + 
  geom_bar(stat = "identity") +
  labs(title = "Dispersal mode per study patch",
       x = "data owner",
       y = "proportion of spp.",
       fill = "disp mode") +
  theme_minimal() +
  theme(legend.position = "right",
      axis.text.x = element_text(angle = 50, vjust = 1.0, hjust = 1.0)) +
  scale_fill_manual(values = c("Zoo" = "purple4", "AneAuto" = "darkgoldenrod3", "Undet" = "gray"),
                    labels = c("Zoo" = "Zoo", "AneAuto" = "Non-Zoo")) 

jpeg("PropDispMode-Dez23-2024.jpeg", width = 20, height = 14,units = "cm", pointsize = 9,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(disp_graph)
dev.off()





# Eco group proportions
eco_group_prop <- trapsmerged %>%
  filter(value != 0, !is.na(family)) %>%
  group_by(data_owner, eco_group) %>%
  summarise(total_spp = n_distinct(sp_id)) %>%
  mutate(
    prop_spp = total_spp / sum(total_spp),
    eco_group = if_else(is.na(eco_group), "Undet", eco_group))

eco_group_dens <- trapsmerged %>%
  filter(value != 0, !is.na(family)) %>%
  group_by(data_owner, eco_group, trap_id) %>%
  summarise(
    total_seeds = sum(value),
    avg_months = mean(months, na.rm = TRUE),
    trap_area = first(trap_area_m2),
    .groups = "drop") %>%
  mutate(seed_dens_per_trap = total_seeds / trap_area / avg_months) %>%
  group_by(data_owner, eco_group) %>%
  summarise(seed_dens = mean(seed_dens_per_trap, na.rm = TRUE)) %>%
  mutate(eco_group = if_else(is.na(eco_group), "Undet", eco_group))

eco_group_final <- left_join(eco_group_dens, eco_group_prop, by = c("data_owner", "eco_group"))


disp_table2 <- left_join(eco_group_final,landscape) %>% 
  subset(!data_owner %in% c("MaraschinDaSilvaF","MoraesLFD","MerattiOliveiraMA",
                            "AlmeidaJrPA","RochaJ","RotherD2",
                            "GamaM","PiottoD","FreitasCG5",
                            "Pimentel&Zamith","MartiniA")) # excl studies w/ <50% ID spp

disp_table2$locality_name<- as.factor(disp_table2$locality_name)
disp_table2$patch_log<- log(disp_table2$patch_size_ha)
disp_table2$seed_dens_log<- log(disp_table2$seed_dens)


# Barplot for ecological group proportions
disp_graph_group<-ggplot(disp_table2, aes(x = data_owner, y = prop_spp, fill = eco_group)) + 
  geom_bar(stat = "identity") +
  labs(title = "Ecological group per study patch",
       x = "data owner",
       y = "proportion of spp.",
       fill = "eco group") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 50, vjust = 1.0, hjust = 1.0)) +
  scale_fill_manual(values = c("late" = "green4", "early" = "red4", "Undet" = "gray"),
                    labels = c("late" = "Late", "early" = "Early")) 

jpeg("PropEcolGroup-Oct-2025.jpeg", width = 20, height = 14,units = "cm", pointsize = 9,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(disp_graph_group)
dev.off()





# Habit proportions
unique(traits$habit)

habit_prop <- trapsmerged %>%
  filter(value != 0, 
         !is.na(family),
         !is.na(habit)) %>%
  group_by(data_owner, habit) %>%
  summarise(total_spp = n_distinct(sp_id)) %>%
  mutate(
    prop_spp = total_spp / sum(total_spp))
    #habit = if_else(is.na(habit), "Undet", habit))

habit_dens <- trapsmerged %>%
  filter(value != 0, 
         !is.na(family),
         !is.na(habit)) %>%
  group_by(data_owner, habit, trap_id) %>%
  summarise(
    total_seeds = sum(value),
    avg_months = mean(months, na.rm = TRUE),
    trap_area = first(trap_area_m2),
    .groups = "drop") %>%
  mutate(seed_dens_per_trap = total_seeds / trap_area / avg_months) %>%
  group_by(data_owner, habit) %>%
  summarise(seed_dens = mean(seed_dens_per_trap, na.rm = TRUE)) 
  #mutate(habit = if_else(is.na(habit), "Undet", habit))

habit_final <- left_join(habit_dens, habit_prop, by = c("data_owner", "habit"))


disp_table3 <- left_join(habit_final,landscape) %>% 
  subset(!data_owner %in% c("MaraschinDaSilvaF","MoraesLFD","MerattiOliveiraMA",
                            "AlmeidaJrPA","RochaJ","RotherD2",
                            "GamaM","PiottoD","FreitasCG5",
                            "Pimentel&Zamith","MartiniA")) # excl studies w/ <50% ID spp

disp_table3$locality_name<- as.factor(disp_table3$locality_name)
disp_table3$patch_log<- log(disp_table3$patch_size_ha)
disp_table3$seed_dens_log<- log(disp_table3$seed_dens)


# Barplot for Habit proportions
#disp_table3$habit <- factor(disp_table3$habit, 
 #                           levels = c("Undet", "others", "shrub", "tree"))

#disp_table3_no_undet <- disp_table3 %>%
 # filter(habit != "Undet")

graph_habit<-ggplot(disp_table3, aes(x = data_owner, y = prop_spp, fill = habit)) + 
  geom_bar(stat = "identity") +
  labs(title = "Growth form per study patch",
       x = "data owner",
       y = "proportion of spp.",
       fill = "Growth form") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 50, vjust = 1.0, hjust = 1.0)) +
  scale_fill_manual(values = c("tree" = "brown", "shrub" = "orange", "others" = "gray"),
                    labels = c("tree" = "Tree", "shrub" = "Shrub", "others" = "Others")) 

jpeg("PropGrowthForm-Feb-2026.jpeg", width = 20, height = 14,units = "cm", pointsize = 9,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(graph_habit)
dev.off()





##### Buffers for *disp mode proportion* as response variable
# Forest cover buffers
m1_disp_prop_cover_5km = glmmTMB(prop_spp~forest_cover_5km,
                            family = ordbeta(link = "logit"),
                            data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
                            summary(m1_disp_prop_cover_5km)

m2_disp_prop_cover_2km = glmmTMB(prop_spp~forest_cover_2km,
                            family = ordbeta(link = "logit"),
                            data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
                            summary(m2_disp_prop_cover_2km)

m3_disp_prop_cover_1km = glmmTMB(prop_spp~forest_cover_1km,
                            family = ordbeta(link = "logit"),
                            data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
                            summary(m3_disp_prop_cover_1km)

m4_disp_prop_cover_500m = glmmTMB(prop_spp~forest_cover_500m,
                            family = ordbeta(link = "logit"),
                            data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
                            summary(m4_disp_prop_cover_500m)

AIC.m_disp_prop_cover <- AICtab(m1_disp_prop_cover_5km,
                           m2_disp_prop_cover_2km,
                           m3_disp_prop_cover_1km,
                           m4_disp_prop_cover_500m,
                           delta=T,weights=T)

AIC.m_disp_prop_cover <- data.frame(AIC.m_disp_prop_cover)
AIC.m_disp_prop_cover



# Number of patches buffers
m1_disp_prop_npatch_5km = glmmTMB(prop_spp~n_patch_5km,
                            family = ordbeta(link = "logit"),
                            data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
                            summary(m1_disp_prop_npatch_5km)

m2_disp_prop_npatch_2km = glmmTMB(prop_spp~n_patch_2km,
                            family = ordbeta(link = "logit"),
                            data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
                            summary(m2_disp_prop_npatch_2km)

m3_disp_prop_npatch_1km = glmmTMB(prop_spp~n_patch_1km,
                            family = ordbeta(link = "logit"),
                            data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
                            summary(m3_disp_prop_npatch_1km)

m4_disp_prop_npatch_500m = glmmTMB(prop_spp~n_patch_500m,
                            family = ordbeta(link = "logit"),
                            data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
                            summary(m4_disp_prop_npatch_500m)

AIC.m_disp_prop_npatch <- AICtab(m1_disp_prop_npatch_5km,
                                m2_disp_prop_npatch_2km,
                                m3_disp_prop_npatch_1km,
                                m4_disp_prop_npatch_500m,
                                delta=T,weights=T)

AIC.m_disp_prop_npatch <- data.frame(AIC.m_disp_prop_npatch)
AIC.m_disp_prop_npatch





# Model selection
# Full model
m1_disp_prop_full = glmmTMB(prop_spp~scale(patch_size_log)+
                         scale(forest_cover_5km)+
                         scale(n_patch_5km)+
                         scale(prec.total)+
                         location_lat+
                         scale(patch_size_log)*scale(n_patch_5km) +
                         scale(forest_cover_5km)*scale(n_patch_5km) +      
                         scale(patch_size_log)*scale(prec.total) +
                         scale(forest_cover_5km)*scale(prec.total) +
                         scale(n_patch_5km)*scale(prec.total),
                       #(1|locality_name), 
                       family = ordbeta(link = "logit"),
                       data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
#control = glmmTMBControl(optimizer = "nlminb", optArgs = list(eval.max = 1e4, iter.max = 1e4)))
#control = glmmTMBControl(optimizer = optim, #nlminb
#optArgs = list(method = "BFGS")))  
# 'arg' can be “Nelder-Mead”, “BFGS”, “CG”, “L-BFGS-B”, “SANN”, “Brent”
summary(m1_disp_prop_full)
simulationOutput <- simulateResiduals(fittedModel = m1_disp_prop_full, plot = TRUE)
#step_model <- step(m1_disp_prop_5km_a.full, direction = "both")
#summary(step_model)
vif_results <- check_collinearity(m1_disp_prop_full)
print(vif_results)
plot(ggeffects::ggpredict(m1_disp_prop_full, terms = c("location_lat")),show_data=T,jitter=T)
plot(ggeffects::ggpredict(m1_disp_prop_full, terms = c("patch_size_log")),show_data=T)
plot(ggeffects::ggpredict(m1_disp_prop_full, terms = c("forest_cover_5km")),show_data=T)
plot(ggeffects::ggpredict(m1_disp_prop_full, terms = c("n_patch_5km")), show_data=T,jitter=T)
plot(ggeffects::ggpredict(m1_disp_prop_full, terms = c("prec.total")),show_data=T)

#model.selection <- dredge(m1_disp_prop_full, rank = "AICc")
#model.selection
#best.models <- get.models(model.selection, subset = delta < 2)
#best.models
#simulationOutput <- simulateResiduals(best.models[[1]], plot = TRUE)
#avg.model <- model.avg(best.models)
#summary(avg.model)


m1_disp_prop_full2 = glmmTMB(prop_spp~scale(patch_size_log)+
                          scale(forest_cover_5km)+
                          scale(n_patch_5km)+
                          scale(prec.total)+
                          location_lat,
                        family = ordbeta(link = "logit"),
                        data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")

# Latitude (control variable)
m2_disp_prop = glmmTMB(prop_spp ~ location_lat, family = ordbeta(link = "logit"),
                  data = disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")

# Each predictor
m3_disp_prop = glmmTMB(prop_spp ~ scale(patch_size_log) + location_lat, family = ordbeta(link="logit"), 
                  data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
m4_disp_prop = glmmTMB(prop_spp ~ scale(forest_cover_5km) + location_lat, family = ordbeta(link="logit"), 
                  data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
m5_disp_prop = glmmTMB(prop_spp ~ scale(n_patch_5km) + location_lat, family = ordbeta(link="logit"), 
                  data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
m6_disp_prop = glmmTMB(prop_spp ~ scale(prec.total) + location_lat, family = ordbeta(link="logit"), 
                  data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")

# Plausible interactions
m7_disp_prop = glmmTMB(prop_spp ~ scale(patch_size_log)*scale(n_patch_5km) + location_lat, family = ordbeta(link="logit"), 
                  data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
m8_disp_prop = glmmTMB(prop_spp ~ scale(forest_cover_5km)*scale(n_patch_5km) + location_lat, family = ordbeta(link="logit"), 
                  data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
m9_disp_prop = glmmTMB(prop_spp ~ scale(patch_size_log)*scale(prec.total) + location_lat, family = ordbeta(link="logit"), 
                  data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
m10_disp_prop = glmmTMB(prop_spp ~ scale(forest_cover_5km)*scale(prec.total) + location_lat, family = ordbeta(link="logit"), 
                   data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
m11_disp_prop = glmmTMB(prop_spp ~ scale(n_patch_5km)*scale(prec.total) + location_lat, family = ordbeta(link="logit"), 
                   data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail") 

# Additive models
#m12_disp_prop = glmmTMB(prop_spp ~ scale(patch_size_log)+scale(n_patch_5km) + location_lat, family = ordbeta(link="logit"), 
 #                  data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
#m13_disp_prop = glmmTMB(prop_spp ~ scale(forest_cover_5km)+scale(n_patch_5km) + location_lat, family = ordbeta(link="logit"), 
 #                  data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
#m14_disp_prop = glmmTMB(prop_spp ~ scale(patch_size_log)+scale(prec.total) + location_lat, family = ordbeta(link="logit"), 
 #                  data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
#m15_disp_prop = glmmTMB(prop_spp ~ scale(forest_cover_5km)+scale(prec.total) + location_lat, family = ordbeta(link="logit"), 
 #                  data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail") 
#m16_disp_prop = glmmTMB(prop_spp ~ scale(n_patch_5km)+scale(prec.total) + location_lat, family = ordbeta(link="logit"), 
 #                data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail") 

# Null model
m_disp_prop_null = glmmTMB(prop_spp ~ 1,
                      family = ordbeta(link = "logit"), data = disp_table[disp_table$disp_mode=="Zoo",],
                      na.action = "na.fail")

# AIC
AIC.m_disp_prop <- AICtab(#m1_disp_prop_full, 
  #m1_disp_prop_full2, 
  m2_disp_prop, m3_disp_prop, m4_disp_prop, m5_disp_prop, m6_disp_prop, m7_disp_prop,
  m8_disp_prop, m9_disp_prop, m10_disp_prop, m11_disp_prop, #m12_disp_prop,
  #m13_disp_prop, m14_disp_prop, m15_disp_prop, m16_disp_prop, 
  m_disp_prop_null, delta = TRUE, weights = TRUE)

AIC.m_disp_prop <- data.frame(AIC.m_disp_prop)
AIC.m_disp_prop
AIC.m_disp_prop_df <- data.frame(model = rownames(AIC.m_disp_prop),
                                 AIC.m_disp_prop,row.names = NULL)

write.xlsx(AIC.m_disp_prop_df, file = "AIC_m_disp_prop.xlsx", rowNames = FALSE)


# Best models:

#summary(m15_disp_prop)
#simulationOutput <- simulateResiduals(fittedModel = m15_disp_prop, plot = TRUE)
#vif_results <- check_collinearity(m15_disp_prop)
#print(vif_results)
#plot(ggeffects::ggpredict(m15_disp_prop, terms = c("location_lat")),show_data=T,jitter=T)
#plot(ggeffects::ggpredict(m15_disp_prop, terms = c("prec.total")),show_data=T)
#plot(ggeffects::ggpredict(m15_disp_prop, terms = c("forest_cover_5km","prec.total")),show_data=T)
#plot(ggeffects::ggpredict(m15_disp_prop, terms = c("forest_cover_5km")),show_data=T)

#modelOutput.disp_prop_m15<-capture.output(summary(m15_disp_prop)) #writing the best model outputs
#write.csv(modelOutput.disp_prop_m15,"modelOutput-disp_prop-m15.csv",row.names=F,quote=F)


summary(m6_disp_prop)
simulationOutput <- simulateResiduals(fittedModel = m6_disp_prop, plot = TRUE)
r2(m6_disp_prop)
vif_results <- check_collinearity(m6_disp_prop)
print(vif_results)
plot(ggeffects::ggpredict(m6_disp_prop, terms = c("location_lat")),show_data=T,jitter=T)
plot(ggeffects::ggpredict(m6_disp_prop, terms = c("prec.total")),show_data=T)

modelOutput.disp_prop_m6<-capture.output(summary(m6_disp_prop)) #writing the best model outputs
write.csv(modelOutput.disp_prop_m6,"modelOutput-disp_prop-m6.csv",row.names=F,quote=F)


summary(m10_disp_prop)
simulationOutput <- simulateResiduals(fittedModel = m10_disp_prop, plot = TRUE)
r2(m10_disp_prop)
vif_results <- check_collinearity(m10_disp_prop)
print(vif_results)
plot(ggeffects::ggpredict(m10_disp_prop, terms = c("location_lat")),show_data=T,jitter=T)
plot(ggeffects::ggpredict(m10_disp_prop, terms = c("prec.total")),show_data=T)
plot(ggeffects::ggpredict(m10_disp_prop, terms = c("forest_cover_5km","prec.total")),show_data=T)
plot(ggeffects::ggpredict(m10_disp_prop, terms = c("forest_cover_5km")),show_data=T)

modelOutput.disp_prop_m10<-capture.output(summary(m10_disp_prop)) #writing the best model outputs
write.csv(modelOutput.disp_prop_m10,"modelOutput-disp_prop-m10.csv",row.names=F,quote=F)



# Moran's I correlogram of model residuals
disp_table_zoo <- disp_table[disp_table$disp_mode=="Zoo",]
m6_disp_prop_residuals <- residuals(m6_disp_prop, type = "pearson")
disp_table_zoo$prop_residuals <- m6_disp_prop_residuals

cor_moran_m6_disp_prop <- correlog(
  x = disp_table_zoo$location_long,
  y = disp_table_zoo$location_lat,
  z = disp_table_zoo$prop_residuals,
  increment = 1,   
  resamp = 999)     

plot(cor_moran_m6_disp_prop, ylim = c(-1, 1))
abline(h = 0, lty = 2, col = "red") 


m10_disp_prop_residuals <- residuals(m10_disp_prop, type = "pearson")
disp_table_zoo$prop_residuals <- m10_disp_prop_residuals

cor_moran_m10_disp_prop <- correlog(
  x = disp_table_zoo$location_long,
  y = disp_table_zoo$location_lat,
  z = disp_table_zoo$prop_residuals,
  increment = 1,   
  resamp = 999)     

plot(cor_moran_m10_disp_prop, ylim = c(-1, 1))
abline(h = 0, lty = 2, col = "red") 


# Plotting the figure
jpeg("moran_plots_disp_prop.jpg", width = 1200, height = 1800, res = 150)
par(mfrow = c(2, 1))  # 2 rows, 1 column

plot(cor_moran_m6_disp_prop, ylim = c(-1, 1), main = "Correlogram - prop_zoo ~ prec.total")
abline(h = 0, lty = 2, col = "red")

plot(cor_moran_m10_disp_prop, ylim = c(-1, 1), main = "Correlogram - prop_zoo ~ forest_cover * prec.total")
abline(h = 0, lty = 2, col = "red")

dev.off()



# Model predictions 
pred.test1<-ggpredict(m6_disp_prop, terms=c("prec.total")) #model predictions
pred.test2<-ggpredict(m10_disp_prop, terms=c("forest_cover_5km"))
pred.test3<-ggpredict(m10_disp_prop, terms=c("forest_cover_5km","prec.total"))
pred.test4<-ggpredict(m6_disp_prop, terms=c("location_lat"))


{a.predict1<-plot(pred.test1,show_data=T,show_ci=T,colors="blue",dot_size=1.5)+  
    labs(title="",x="precipitation (mm)",y="proportion of Zoo species", col="")+
    #geom_point(size = 2) 
    ylim(0.0,1)+
    theme_classic()+
    theme(legend.position = "none", axis.title.y = element_text(size = 10))}

{b.predict2<-plot(pred.test2,show_data=T,show_ci=T,colors="blue",dot_size=1.5)+  
    labs(title="",x="forest cover (%)",y="proportion of Zoo species", col="")+
    ylim(0.0,1)+
    theme_classic()+
    theme(legend.position = "none", axis.title.y = element_text(size = 10))}

{c.predict3<-plot(pred.test3,show_data=T,show_ci=T,dot_size=1.5)+
    #scale_linetype_manual(labels = c("-1 SD", "Mean", "+1 SD"))+  
   labs(title="",x="forest cover (%)",y="proportion of Zoo species", col="precipitation (mm)")+
  ylim(0.0,1)+
  theme_classic()+
  theme(legend.position = c(0.8,0.2),legend.text = element_text(size = 7),
        legend.title = element_text(size = 10), axis.title.y = element_text(size = 10))}

{d.predict4<-plot(pred.test4,show_data=T,show_ci=T,colors="blue",ci_style="dash",dot_size=1.5)+  
    labs(title="",x="latitude",y="proportion of Zoo species", col="")+
    ylim(0.0,1)+
    scale_x_reverse()+ 
    theme_classic()+
    theme(legend.position = "none", axis.title.y = element_text(size = 10))}


jpeg("ModelPredictions-ZooProportions-Oct2025.jpeg", width = 17, height = 14,
     units = "cm", pointsize = 5,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(a.predict1, b.predict2,  
             c.predict3, d.predict4,  
             nrow = 2, ncol = 2)
dev.off() 





##### Buffers for *Zoo seed density* as response variable
# Forest cover buffers
m1_Zoodisp_dens_cover_5km = glmmTMB(seed_dens_log~forest_cover_5km,
                                family = gaussian(link = "identity"),
                                data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
                                summary(m1_Zoodisp_dens_cover_5km)

m2_Zoodisp_dens_cover_2km = glmmTMB(seed_dens_log~forest_cover_2km,
                                family = gaussian(link = "identity"),
                                data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
                                summary(m2_Zoodisp_dens_cover_2km)

m3_Zoodisp_dens_cover_1km = glmmTMB(seed_dens_log~forest_cover_1km,
                                family = gaussian(link = "identity"),
                                data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
                                summary(m3_Zoodisp_dens_cover_1km)

m4_Zoodisp_dens_cover_500m = glmmTMB(seed_dens_log~forest_cover_500m,
                                family = gaussian(link = "identity"),
                                data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
                                summary(m4_Zoodisp_dens_cover_500m)

AIC.m_Zoodisp_dens_cover <- AICtab(m1_Zoodisp_dens_cover_5km,
                                m2_Zoodisp_dens_cover_2km,
                                m3_Zoodisp_dens_cover_1km,
                                m4_Zoodisp_dens_cover_500m,
                                delta=T,weights=T)

AIC.m_Zoodisp_dens_cover <- data.frame(AIC.m_Zoodisp_dens_cover)
AIC.m_Zoodisp_dens_cover



# Number of patches buffers
m1_Zoodisp_dens_npatch_5km = glmmTMB(seed_dens_log~n_patch_5km,
                                  family = gaussian(link = "identity"),
                                  data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
                                  summary(m1_Zoodisp_dens_npatch_5km)

m2_Zoodisp_dens_npatch_2km = glmmTMB(seed_dens_log~n_patch_2km,
                                  family = gaussian(link = "identity"),
                                  data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
                                  summary(m2_Zoodisp_dens_npatch_2km)

m3_Zoodisp_dens_npatch_1km = glmmTMB(seed_dens_log~n_patch_1km,
                                  family = gaussian(link = "identity"),
                                  data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
                                  summary(m3_Zoodisp_dens_npatch_1km)

m4_Zoodisp_dens_npatch_500m = glmmTMB(seed_dens_log~n_patch_500m,
                                  family = gaussian(link = "identity"),
                                  data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
                                  summary(m4_Zoodisp_dens_npatch_500m)

AIC.m_Zoodisp_dens_npatch <- AICtab(m1_Zoodisp_dens_npatch_5km,
                                 m2_Zoodisp_dens_npatch_2km,
                                 m3_Zoodisp_dens_npatch_1km,
                                 m4_Zoodisp_dens_npatch_500m,
                                 delta=T,weights=T)

AIC.m_Zoodisp_dens_npatch <- data.frame(AIC.m_Zoodisp_dens_npatch)
AIC.m_Zoodisp_dens_npatch





# Model selection
# Full model
m1_Zoodisp_dens_full = glmmTMB(seed_dens_log~scale(patch_size_log)+
                              scale(forest_cover_500m)+
                              scale(n_patch_500m)+
                              scale(prec.total)+
                              location_lat+
                              scale(patch_size_log)*scale(n_patch_500m) +
                              scale(forest_cover_500m)*scale(n_patch_500m) +      
                              scale(patch_size_log)*scale(prec.total) +
                              scale(forest_cover_500m)*scale(prec.total) +
                              scale(n_patch_500m)*scale(prec.total),
                            #(1|locality_name), 
                            family = gaussian(link = "identity"),
                            data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
#control = glmmTMBControl(optimizer = "nlminb", optArgs = list(eval.max = 1e4, iter.max = 1e4)))
#control = glmmTMBControl(optimizer = optim, #nlminb
#optArgs = list(method = "BFGS")))  
# 'arg' can be “Nelder-Mead”, “BFGS”, “CG”, “L-BFGS-B”, “SANN”, “Brent”
summary(m1_Zoodisp_dens_full)
simulationOutput <- simulateResiduals(fittedModel = m1_Zoodisp_dens_full, plot = TRUE)
#step_model <- step(m1_Zoodisp_dens_500m_a.full, direction = "both")
#summary(step_model)
vif_results <- check_collinearity(m1_Zoodisp_dens_full)
print(vif_results)
plot(ggeffects::ggpredict(m1_Zoodisp_dens_full, terms = c("location_lat")),show_data=T,jitter=T)
plot(ggeffects::ggpredict(m1_Zoodisp_dens_full, terms = c("patch_size_log")),show_data=T)
plot(ggeffects::ggpredict(m1_Zoodisp_dens_full, terms = c("forest_cover_500m")),show_data=T)
plot(ggeffects::ggpredict(m1_Zoodisp_dens_full, terms = c("n_patch_500m")), show_data=T,jitter=T)
plot(ggeffects::ggpredict(m1_Zoodisp_dens_full, terms = c("prec.total")),show_data=T)

#model.selection <- dredge(m1_Zoodisp_dens_full, rank = "AICc")
#model.selection
#best.models <- get.models(model.selection, subset = delta < 2)
#best.models
#simulationOutput <- simulateResiduals(best.models[[1]], plot = TRUE)
#avg.model <- model.avg(best.models)
#summary(avg.model)


m1_Zoodisp_dens_full2 = glmmTMB(seed_dens_log~scale(patch_size_log)+
                               scale(forest_cover_500m)+
                               scale(n_patch_500m)+
                               scale(prec.total)+
                               location_lat,
                             family = gaussian(link = "identity"),
                             data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")

# Latitude (control variable)
m2_Zoodisp_dens = glmmTMB(seed_dens_log ~ location_lat, family = gaussian(link = "identity"),
                       data = disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")

# Each predictor
m3_Zoodisp_dens = glmmTMB(seed_dens_log ~ scale(patch_size_log) + location_lat, family = gaussian(link = "identity"), 
                       data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
m4_Zoodisp_dens = glmmTMB(seed_dens_log ~ scale(forest_cover_500m) + location_lat, family = gaussian(link = "identity"), 
                       data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
m5_Zoodisp_dens = glmmTMB(seed_dens_log ~ scale(n_patch_500m) + location_lat, family = gaussian(link = "identity"), 
                       data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
m6_Zoodisp_dens = glmmTMB(seed_dens_log ~ scale(prec.total) + location_lat, family = gaussian(link = "identity"), 
                       data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")

# Plausible interactions
m7_Zoodisp_dens = glmmTMB(seed_dens_log ~ scale(patch_size_log)*scale(n_patch_500m) + location_lat, family = gaussian(link = "identity"), 
                       data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
m8_Zoodisp_dens = glmmTMB(seed_dens_log ~ scale(forest_cover_500m)*scale(n_patch_500m) + location_lat, family = gaussian(link = "identity"), 
                       data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
m9_Zoodisp_dens = glmmTMB(seed_dens_log ~ scale(patch_size_log)*scale(prec.total) + location_lat, family = gaussian(link = "identity"), 
                       data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
m10_Zoodisp_dens = glmmTMB(seed_dens_log ~ scale(forest_cover_500m)*scale(prec.total) + location_lat, family = gaussian(link = "identity"), 
                        data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
m11_Zoodisp_dens = glmmTMB(seed_dens_log ~ scale(n_patch_500m)*scale(prec.total) + location_lat, family = gaussian(link = "identity"), 
                        data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail") 

# Additive models
#m12_Zoodisp_dens = glmmTMB(seed_dens_log ~ scale(patch_size_log)+scale(n_patch_500m) + location_lat, family = gaussian(link = "identity"), 
 #                       data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
#m13_Zoodisp_dens = glmmTMB(seed_dens_log ~ scale(forest_cover_500m)+scale(n_patch_500m) + location_lat, family = gaussian(link = "identity"), 
 #                       data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
#m14_Zoodisp_dens = glmmTMB(seed_dens_log ~ scale(patch_size_log)+scale(prec.total) + location_lat, family = gaussian(link = "identity"), 
 #                       data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail")
#m15_Zoodisp_dens = glmmTMB(seed_dens_log ~ scale(forest_cover_500m)+scale(prec.total) + location_lat, family = gaussian(link = "identity"), 
 #                       data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail") 
#m16_Zoodisp_dens = glmmTMB(seed_dens_log ~ scale(n_patch_500m)+scale(prec.total) + location_lat, family = gaussian(link = "identity"), 
 #                       data=disp_table[disp_table$disp_mode=="Zoo",], na.action="na.fail") 

# Null model
m_Zoodisp_dens_null = glmmTMB(seed_dens_log ~ 1,
                           family = gaussian(link = "identity"), 
                           data = disp_table[disp_table$disp_mode=="Zoo",],
                           na.action = "na.fail")

# AIC
AIC.m_Zoodisp_dens <- AICtab(#m1_Zoodisp_dens_full, 
                          #m1_Zoodisp_dens_full2, 
                          m2_Zoodisp_dens, m3_Zoodisp_dens, m4_Zoodisp_dens, m5_Zoodisp_dens, m6_Zoodisp_dens, m7_Zoodisp_dens,
                          m8_Zoodisp_dens, m9_Zoodisp_dens, m10_Zoodisp_dens, m11_Zoodisp_dens, #m12_Zoodisp_dens,
                          #m13_Zoodisp_dens, m14_Zoodisp_dens, m15_Zoodisp_dens, m16_Zoodisp_dens, 
                          m_Zoodisp_dens_null, delta = TRUE, weights = TRUE)

AIC.m_Zoodisp_dens <- data.frame(AIC.m_Zoodisp_dens)
AIC.m_Zoodisp_dens
AIC.m_Zoodisp_dens_df <- data.frame(model = rownames(AIC.m_Zoodisp_dens),
                                    AIC.m_Zoodisp_dens,row.names = NULL)

write.xlsx(AIC.m_Zoodisp_dens_df, file = "AIC_m_Zoodisp_dens.xlsx", rowNames = FALSE)


# Best models:

#summary(m7_Zoodisp_dens)
#simulationOutput <- simulateResiduals(fittedModel = m7_Zoodisp_dens, plot = TRUE)
#vif_results <- check_collinearity(m7_Zoodisp_dens)
#print(vif_results)
#plot(ggeffects::ggpredict(m7_Zoodisp_dens, terms = c("location_lat")),show_data=T,jitter=T)
#plot(ggeffects::ggpredict(m7_Zoodisp_dens, terms = c("patch_size_log","n_patch_500m")),show_data=T)

#modelOutput.Zoodisp_dens_m7<-capture.output(summary(m7_Zoodisp_dens)) #writing the best model outputs
#write.csv(modelOutput.Zoodisp_dens_m7,"modelOutput-Zoodisp_dens-m7.csv",row.names=F,quote=F)


summary(m8_Zoodisp_dens)
simulationOutput <- simulateResiduals(fittedModel = m8_Zoodisp_dens, plot = TRUE)
r2(m8_Zoodisp_dens)
vif_results <- check_collinearity(m8_Zoodisp_dens)
print(vif_results)
plot(ggeffects::ggpredict(m8_Zoodisp_dens, terms = c("location_lat")),show_data=T,jitter=T)
plot(ggeffects::ggpredict(m8_Zoodisp_dens, terms = c("forest_cover_500m")),show_data=T)
plot(ggeffects::ggpredict(m8_Zoodisp_dens, terms = c("n_patch_500m")),show_data=T)
plot(ggeffects::ggpredict(m8_Zoodisp_dens, terms = c("forest_cover_500m","n_patch_500m")),show_data=T)

modelOutput.Zoodisp_dens_m8<-capture.output(summary(m8_Zoodisp_dens)) #writing the best model outputs
write.csv(modelOutput.Zoodisp_dens_m8,"modelOutput-Zoodisp_dens-m8.csv",row.names=F,quote=F)



# Moran's I correlogram of model residuals
#disp_table_zoo <- disp_table[disp_table$disp_mode=="Zoo",]
m8_Zoodisp_dens_residuals <- residuals(m8_Zoodisp_dens, type = "pearson")
disp_table_zoo$prop_residuals <- m8_Zoodisp_dens_residuals

cor_moran_m8_Zoodisp_dens <- correlog(
  x = disp_table_zoo$location_long,
  y = disp_table_zoo$location_lat,
  z = disp_table_zoo$prop_residuals,
  increment = 1,   
  resamp = 999)     

plot(cor_moran_m8_Zoodisp_dens, ylim = c(-1, 1))
abline(h = 0, lty = 2, col = "red") 



# Model predictions 
pred.test1<-ggpredict(m8_Zoodisp_dens, terms=c("n_patch_500m")) #model predictions
pred.test2<-ggpredict(m8_Zoodisp_dens, terms=c("forest_cover_500m"))
pred.test3<-ggpredict(m8_Zoodisp_dens, terms=c("forest_cover_500m","n_patch_500m[threenum]"))
pred.test4<-ggpredict(m8_Zoodisp_dens, terms=c("location_lat"))


{a.predict1<-plot(pred.test1,show_data=T,show_ci=T,colors="blue",dot_size=1.5)+  
    labs(title="Zoochorous species",x="number of patches",y="seed density (log)", col="")+
    #geom_point(size = 2) 
    scale_y_continuous(limits = c(-2.6, 9), breaks = c(-2.5, 0, 2.5, 5.0, 7.0))+
    theme_classic()+
    theme(legend.position = c(0.2,0.8),legend.text = element_text(size = 8))}

{b.predict2<-plot(pred.test2,show_data=T,show_ci=T,colors="blue",dot_size=1.5)+  
    labs(title="",x="forest cover (%)",y="seed density (log)", col="")+
    scale_y_continuous(limits = c(-2.6, 9), breaks = c(-2.5, 0, 2.5, 5.0, 7.0))+
    theme_classic()+
    theme(legend.position = "none")}

{c.predict3<-plot(pred.test3,show_data=T,show_ci=T,dot_size=1.5)+  
    labs(title="",x="forest cover (%)",y="seed density (log)", col="number of patches")+
    scale_y_continuous(limits = c(-2.6, 9), breaks = c(-2.5, 0, 2.5, 5.0, 7.0))+
    theme_classic()+
    theme(legend.position = c(0.3,0.9),legend.text = element_text(size = 7),
          legend.title = element_text(size = 10))}

{d.predict4<-plot(pred.test4,show_data=T,show_ci=T,colors="blue",ci_style="dash",dot_size=1.5)+  
    labs(title="",x="latitude",y="seed density (log)", col="")+
    scale_y_continuous(limits = c(-2.6, 9), breaks = c(-2.5, 0, 2.5, 5.0, 7.0))+
    scale_x_reverse()+
    theme_classic()+
    theme(legend.position = "none")}


#jpeg("ModelPredictions-ZooDens-Oct2025.jpeg", width = 17, height = 14,
 #    units = "cm", pointsize = 5,
  #   bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
#grid.arrange(a.predict1, b.predict2,  
 #            c.predict3, d.predict4,  
  #           nrow = 2, ncol = 2)
#dev.off() 





##### Buffers for *AneAuto seed density* as response variable
# Forest cover buffers
m1_AneAutodisp_dens_cover_5km = glmmTMB(seed_dens_log~forest_cover_5km,
                                    family = gaussian(link = "identity"),
                                    data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
                                    summary(m1_AneAutodisp_dens_cover_5km)

m2_AneAutodisp_dens_cover_2km = glmmTMB(seed_dens_log~forest_cover_2km,
                                    family = gaussian(link = "identity"),
                                    data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
                                    summary(m2_AneAutodisp_dens_cover_2km)

m3_AneAutodisp_dens_cover_1km = glmmTMB(seed_dens_log~forest_cover_1km,
                                    family = gaussian(link = "identity"),
                                    data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
                                    summary(m3_AneAutodisp_dens_cover_1km)

m4_AneAutodisp_dens_cover_500m = glmmTMB(seed_dens_log~forest_cover_500m,
                                    family = gaussian(link = "identity"),
                                    data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
                                    summary(m4_AneAutodisp_dens_cover_500m)

AIC.m_AneAutodisp_dens_cover <- AICtab(m1_AneAutodisp_dens_cover_5km,
                                   m2_AneAutodisp_dens_cover_2km,
                                   m3_AneAutodisp_dens_cover_1km,
                                   m4_AneAutodisp_dens_cover_500m,
                                   delta=T,weights=T)

AIC.m_AneAutodisp_dens_cover <- data.frame(AIC.m_AneAutodisp_dens_cover)
AIC.m_AneAutodisp_dens_cover



# Number of patches buffers
m1_AneAutodisp_dens_npatch_5km = glmmTMB(seed_dens_log~n_patch_5km,
                                    family = gaussian(link = "identity"),
                                    data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
                                    summary(m1_AneAutodisp_dens_npatch_5km)

m2_AneAutodisp_dens_npatch_2km = glmmTMB(seed_dens_log~n_patch_2km,
                                    family = gaussian(link = "identity"),
                                    data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
                                    summary(m2_AneAutodisp_dens_npatch_2km)

m3_AneAutodisp_dens_npatch_1km = glmmTMB(seed_dens_log~n_patch_1km,
                                    family = gaussian(link = "identity"),
                                    data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
                                    summary(m3_AneAutodisp_dens_npatch_1km)

m4_AneAutodisp_dens_npatch_500m = glmmTMB(seed_dens_log~n_patch_500m,
                                      family = gaussian(link = "identity"),
                                      data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
                                      summary(m4_AneAutodisp_dens_npatch_500m)

AIC.m_AneAutodisp_dens_npatch <- AICtab(m1_AneAutodisp_dens_npatch_5km,
                                    m2_AneAutodisp_dens_npatch_2km,
                                    m3_AneAutodisp_dens_npatch_1km,
                                    m4_AneAutodisp_dens_npatch_500m,
                                    delta=T,weights=T)

AIC.m_AneAutodisp_dens_npatch <- data.frame(AIC.m_AneAutodisp_dens_npatch)
AIC.m_AneAutodisp_dens_npatch





# Model selection
# Full model
m1_AneAutodisp_dens_full = glmmTMB(seed_dens_log~scale(patch_size_log)+
                                 scale(forest_cover_500m)+
                                 scale(n_patch_5km)+
                                 scale(prec.total)+
                                 location_lat+
                                 scale(patch_size_log)*scale(n_patch_5km) +
                                 scale(forest_cover_500m)*scale(n_patch_5km) +      
                                 scale(patch_size_log)*scale(prec.total) +
                                 scale(forest_cover_500m)*scale(prec.total) +
                                 scale(n_patch_5km)*scale(prec.total),
                               #(1|locality_name), 
                               family = gaussian(link = "identity"),
                               data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
#control = glmmTMBControl(optimizer = "nlminb", optArgs = list(eval.max = 1e4, iter.max = 1e4)))
#control = glmmTMBControl(optimizer = optim, #nlminb
#optArgs = list(method = "BFGS")))  
# 'arg' can be “Nelder-Mead”, “BFGS”, “CG”, “L-BFGS-B”, “SANN”, “Brent”
summary(m1_AneAutodisp_dens_full)
simulationOutput <- simulateResiduals(fittedModel = m1_AneAutodisp_dens_full, plot = TRUE)
#step_model <- step(m1_AneAutodisp_dens_500m_a.full, direction = "both")
#summary(step_model)
vif_results <- check_collinearity(m1_AneAutodisp_dens_full)
print(vif_results)
plot(ggeffects::ggpredict(m1_AneAutodisp_dens_full, terms = c("location_lat")),show_data=T,jitter=T)
plot(ggeffects::ggpredict(m1_AneAutodisp_dens_full, terms = c("patch_size_log")),show_data=T)
plot(ggeffects::ggpredict(m1_AneAutodisp_dens_full, terms = c("forest_cover_500m")),show_data=T)
plot(ggeffects::ggpredict(m1_AneAutodisp_dens_full, terms = c("n_patch_5km")), show_data=T,jitter=T)
plot(ggeffects::ggpredict(m1_AneAutodisp_dens_full, terms = c("prec.total")),show_data=T)

#model.selection <- dredge(m1_AneAutodisp_dens_full, rank = "AICc")
#model.selection
#best.models <- get.models(model.selection, subset = delta < 2)
#best.models
#simulationOutput <- simulateResiduals(best.models[[1]], plot = TRUE)
#avg.model <- model.avg(best.models)
#summary(avg.model)


m1_AneAutodisp_dens_full2 = glmmTMB(seed_dens_log~scale(patch_size_log)+
                                  scale(forest_cover_500m)+
                                  scale(n_patch_5km)+
                                  scale(prec.total)+
                                  location_lat,
                                family = gaussian(link = "identity"),
                                data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")

# Latitude (control variable)
m2_AneAutodisp_dens = glmmTMB(seed_dens_log ~ location_lat, family = gaussian(link = "identity"),
                          data = disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")

# Each predictor
m3_AneAutodisp_dens = glmmTMB(seed_dens_log ~ scale(patch_size_log) + location_lat, family = gaussian(link = "identity"), 
                          data=disp_table[disp_table$disp_mode=="AneAuto",], na.action="na.fail")
m4_AneAutodisp_dens = glmmTMB(seed_dens_log ~ scale(forest_cover_500m) + location_lat, family = gaussian(link = "identity"), 
                          data=disp_table[disp_table$disp_mode=="AneAuto",], na.action="na.fail")
m5_AneAutodisp_dens = glmmTMB(seed_dens_log ~ scale(n_patch_5km) + location_lat, family = gaussian(link = "identity"), 
                          data=disp_table[disp_table$disp_mode=="AneAuto",], na.action="na.fail")
m6_AneAutodisp_dens = glmmTMB(seed_dens_log ~ scale(prec.total) + location_lat, family = gaussian(link = "identity"), 
                          data=disp_table[disp_table$disp_mode=="AneAuto",], na.action="na.fail")

# Plausible interactions
m7_AneAutodisp_dens = glmmTMB(seed_dens_log ~ scale(patch_size_log)*scale(n_patch_5km) + location_lat, family = gaussian(link = "identity"), 
                          data=disp_table[disp_table$disp_mode=="AneAuto",], na.action="na.fail")
m8_AneAutodisp_dens = glmmTMB(seed_dens_log ~ scale(forest_cover_500m)*scale(n_patch_5km) + location_lat, family = gaussian(link = "identity"), 
                          data=disp_table[disp_table$disp_mode=="AneAuto",], na.action="na.fail")
m9_AneAutodisp_dens = glmmTMB(seed_dens_log ~ scale(patch_size_log)*scale(prec.total) + location_lat, family = gaussian(link = "identity"), 
                          data=disp_table[disp_table$disp_mode=="AneAuto",], na.action="na.fail")
m10_AneAutodisp_dens = glmmTMB(seed_dens_log ~ scale(forest_cover_500m)*scale(prec.total) + location_lat, family = gaussian(link = "identity"), 
                           data=disp_table[disp_table$disp_mode=="AneAuto",], na.action="na.fail")
m11_AneAutodisp_dens = glmmTMB(seed_dens_log ~ scale(n_patch_5km)*scale(prec.total) + location_lat, family = gaussian(link = "identity"), 
                           data=disp_table[disp_table$disp_mode=="AneAuto",], na.action="na.fail") 

# Additive models
#m12_AneAutodisp_dens = glmmTMB(seed_dens_log ~ scale(patch_size_log)+scale(n_patch_5km) + location_lat, family = gaussian(link = "identity"), 
 #                          data=disp_table[disp_table$disp_mode=="AneAuto",], na.action="na.fail")
#m13_AneAutodisp_dens = glmmTMB(seed_dens_log ~ scale(forest_cover_500m)+scale(n_patch_5km) + location_lat, family = gaussian(link = "identity"), 
 #                          data=disp_table[disp_table$disp_mode=="AneAuto",], na.action="na.fail")
#m14_AneAutodisp_dens = glmmTMB(seed_dens_log ~ scale(patch_size_log)+scale(prec.total) + location_lat, family = gaussian(link = "identity"), 
 #                          data=disp_table[disp_table$disp_mode=="AneAuto",], na.action="na.fail")
#m15_AneAutodisp_dens = glmmTMB(seed_dens_log ~ scale(forest_cover_500m)+scale(prec.total) + location_lat, family = gaussian(link = "identity"), 
 #                          data=disp_table[disp_table$disp_mode=="AneAuto",], na.action="na.fail") 
#m16_AneAutodisp_dens = glmmTMB(seed_dens_log ~ scale(n_patch_5km)+scale(prec.total) + location_lat, family = gaussian(link = "identity"), 
 #                          data=disp_table[disp_table$disp_mode=="AneAuto",], na.action="na.fail") 

# Null model
m_AneAutodisp_dens_null = glmmTMB(seed_dens_log ~ 1,
                              family = gaussian(link = "identity"), 
                              data = disp_table[disp_table$disp_mode=="AneAuto",],
                              na.action = "na.fail")

# AIC
AIC.m_AneAutodisp_dens <- AICtab(#m1_AneAutodisp_dens_full, 
                             #m1_AneAutodisp_dens_full2, 
                             m2_AneAutodisp_dens, m3_AneAutodisp_dens, m4_AneAutodisp_dens, m5_AneAutodisp_dens, m6_AneAutodisp_dens, m7_AneAutodisp_dens,
                             m8_AneAutodisp_dens, m9_AneAutodisp_dens, m10_AneAutodisp_dens, m11_AneAutodisp_dens, #m12_AneAutodisp_dens,
                             #m13_AneAutodisp_dens, m14_AneAutodisp_dens, m15_AneAutodisp_dens, m16_AneAutodisp_dens, 
                             m_AneAutodisp_dens_null, delta = TRUE, weights = TRUE)

AIC.m_AneAutodisp_dens <- data.frame(AIC.m_AneAutodisp_dens)
AIC.m_AneAutodisp_dens
AIC.m_AneAutodisp_dens_df <- data.frame(model = rownames(AIC.m_AneAutodisp_dens),
                                    AIC.m_AneAutodisp_dens,row.names = NULL)

write.xlsx(AIC.m_AneAutodisp_dens_df, file = "AIC_m_AneAutodisp_dens.xlsx", rowNames = FALSE)


# Best models:

summary(m5_AneAutodisp_dens)
simulationOutput <- simulateResiduals(fittedModel = m5_AneAutodisp_dens, plot = TRUE)
r2(m5_AneAutodisp_dens)
vif_results <- check_collinearity(m5_AneAutodisp_dens)
print(vif_results)
plot(ggeffects::ggpredict(m5_AneAutodisp_dens, terms = c("n_patch_5km")),show_data=T,jitter=T)

modelOutput.AneAutodisp_dens_m5<-capture.output(summary(m5_AneAutodisp_dens)) #writing the best model outputs
write.csv(modelOutput.AneAutodisp_dens_m5,"modelOutput-AneAutodisp_dens-m5.csv",row.names=F,quote=F)


summary(m4_AneAutodisp_dens)
simulationOutput <- simulateResiduals(fittedModel = m4_AneAutodisp_dens, plot = TRUE)
r2(m4_AneAutodisp_dens)
vif_results <- check_collinearity(m4_AneAutodisp_dens)
print(vif_results)
plot(ggeffects::ggpredict(m4_AneAutodisp_dens, terms = c("forest_cover_500m")),show_data=T,jitter=T)

modelOutput.AneAutodisp_dens_m4<-capture.output(summary(m4_AneAutodisp_dens)) #writing the best model outputs
write.csv(modelOutput.AneAutodisp_dens_m4,"modelOutput-AneAutodisp_dens-m4.csv",row.names=F,quote=F)


summary(m2_AneAutodisp_dens)
simulationOutput <- simulateResiduals(fittedModel = m2_AneAutodisp_dens, plot = TRUE)
r2(m2_AneAutodisp_dens)
vif_results <- check_collinearity(m2_AneAutodisp_dens)
print(vif_results)
plot(ggeffects::ggpredict(m2_AneAutodisp_dens, terms = c("location_lat")),show_data=T,jitter=T)

modelOutput.AneAutodisp_dens_m2<-capture.output(summary(m2_AneAutodisp_dens)) #writing the best model outputs
write.csv(modelOutput.AneAutodisp_dens_m2,"modelOutput-AneAutodisp_dens-m2.csv",row.names=F,quote=F)


summary(m8_AneAutodisp_dens)
r2(m8_AneAutodisp_dens)
simulationOutput <- simulateResiduals(fittedModel = m8_AneAutodisp_dens, plot = TRUE)
vif_results <- check_collinearity(m8_AneAutodisp_dens)
print(vif_results)
plot(ggeffects::ggpredict(m8_AneAutodisp_dens, terms = c("location_lat")),show_data=T,jitter=T)
plot(ggeffects::ggpredict(m8_AneAutodisp_dens, terms = c("forest_cover_500m")),show_data=T)
plot(ggeffects::ggpredict(m8_AneAutodisp_dens, terms = c("n_patch_5km")),show_data=T)
plot(ggeffects::ggpredict(m8_AneAutodisp_dens, terms = c("forest_cover_500m","n_patch_5km")),show_data=T)

modelOutput.AneAutodisp_dens_m8<-capture.output(summary(m8_AneAutodisp_dens)) #writing the best model outputs
write.csv(modelOutput.AneAutodisp_dens_m8,"modelOutput-AneAutodisp_dens-m8.csv",row.names=F,quote=F)



# Moran's I correlogram of model residuals
disp_table_AneAuto <- disp_table[disp_table$disp_mode=="AneAuto",]
m8_AneAutodisp_dens_residuals <- residuals(m8_AneAutodisp_dens, type = "pearson")
disp_table_AneAuto$prop_residuals <- m8_AneAutodisp_dens_residuals

cor_moran_m8_AneAutodisp_dens <- correlog(
  x = disp_table_AneAuto$location_long,
  y = disp_table_AneAuto$location_lat,
  z = disp_table_AneAuto$prop_residuals,
  increment = 1,   
  resamp = 999)     

plot(cor_moran_m8_AneAutodisp_dens, ylim = c(-1, 1))
abline(h = 0, lty = 2, col = "red") 



# Plotting the figure
jpeg("moran_plots_disp_dens.jpg", width = 1200, height = 1800, res = 150)
par(mfrow = c(2, 1))  # 2 rows, 1 column

plot(cor_moran_m8_Zoodisp_dens, ylim = c(-1, 1), main = "Correlogram - seed_dens_zoo ~ forest_cover * n_patch")
abline(h = 0, lty = 2, col = "red")

plot(cor_moran_m8_AneAutodisp_dens, ylim = c(-1, 1), main = "Correlogram - seed_dens_non-zoo ~ forest_cover * n_patch")
abline(h = 0, lty = 2, col = "red")

dev.off()



# Model predictions 
pred.test5<-ggpredict(m5_AneAutodisp_dens, terms=c("n_patch_5km")) #model predictions
pred.test6<-ggpredict(m4_AneAutodisp_dens, terms=c("forest_cover_500m"))
pred.test7<-ggpredict(m8_AneAutodisp_dens, terms=c("forest_cover_500m","n_patch_5km[threenum]"))
pred.test8<-ggpredict(m2_AneAutodisp_dens, terms=c("location_lat"))


{e.predict5<-plot(pred.test5,show_data=T,show_ci=T,colors="blue",dot_size=1.5)+  
    labs(title="Non-Zoochorous species",x="number of patches",y="seed density (log)", col="")+
    #geom_point(size = 2) 
    scale_y_continuous(limits = c(-2.6, 7), breaks = c(-2.5, 0, 2.5, 5.0, 7.0))+
    theme_classic()+
    theme(legend.position = c(0.2,0.8),legend.text = element_text(size = 8))}

{f.predict6<-plot(pred.test6,show_data=T,show_ci=T,colors="blue",dot_size=1.5)+  
    labs(title="",x="forest cover (%)",y="seed density (log)", col="")+
    scale_y_continuous(limits = c(-2.6, 7), breaks = c(-2.5, 0, 2.5, 5.0, 7.0))+
    theme_classic()+
    theme(legend.position = "none")}

{g.predict7<-plot(pred.test7,show_data=T,show_ci=T,dot_size=1.5)+  
    labs(title="",x="forest cover (%)",y="seed density (log)", col="number of patches")+
    scale_y_continuous(limits = c(-2.6, 7), breaks = c(-2.5, 0, 2.5, 5.0, 7.0))+
    theme_classic()+
    theme(legend.position = c(0.3,0.9),legend.text = element_text(size = 7),
          legend.title = element_text(size = 10))}

{h.predict8<-plot(pred.test8,show_data=T,show_ci=T,colors="blue",dot_size=1.5)+  
    labs(title="",x="latitude",y="seed density (log)", col="")+
    scale_y_continuous(limits = c(-2.6, 7), breaks = c(-2.5, 0, 2.5, 5.0, 7.0))+
    scale_x_reverse()+
    theme_classic()+
    theme(legend.position = "none")}


jpeg("ModelPredictions-SeedDens-Oct2025.jpeg", width = 17, height = 28,
     units = "cm", pointsize = 5,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(a.predict1, e.predict5, 
             b.predict2, f.predict6,
             c.predict3, g.predict7,
             d.predict4, h.predict8,
             nrow = 4, ncol = 2)
dev.off() 

