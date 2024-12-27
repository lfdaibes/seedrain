install.packages("")
# Set working directory if needed
#setwd("") 
getwd()

# Load packages 
packages <- c("vegan","multcomp","lme4","languageR","bbmle","ggplot2","dplyr","gridExtra",
              "glmmTMB","DHARMa","readxl","openxlsx","ppsr","tidyr","stats",
              "ggcorrplot","corrplot","ggthemes","ggeffects","MuMIn","car",
              "interactions","performance","mgcv","glmmLasso")

lapply(packages,require,character.only = TRUE)
warnings()

# Calling data frames: 
traits <- read.csv("treecotraits-edit3.csv",sep=";",head=T,as.is=T) %>% #disp mode data
            mutate(sp_id = sub(" ", ".", sp_id))
            traits$disp_mode<-as.factor(traits$disp_mode)
            levels(traits$disp_mode) <- list(AneAuto = "anemochoric|autochoric", 
                                  AneAuto = "anemochoric", AneAuto = "autochoric",
                                  Zoo = "autochoric|zoochoric", Zoo = "zoochoric")  
             
landscape <- read_excel("LandscapeFinal-Nov6-2024-Fulbert.xlsx") # landscape data

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
spp_all <- trapsmerged %>% # disp mode proportions incl undet spp.
  filter(value != 0) %>% 
  distinct(data_owner,sp_id) 
write.xlsx(spp_all,"Spp_All.xlsx",
            rowNames=F,quote=F)

Spp_All<-read.xlsx("Spp_All.xlsx",sheet=2)
setdiff(Spp_All$sp_id,traits$sp_id)

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

id_prop <- trapsmerged %>% # two ways of doing it? # this one is the best!
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
  mutate(prop_spp = total_spp / sum(total_spp), # * 100,      # proportions of spp. calculations
          disp_mode = if_else(is.na(disp_mode), "Undet", disp_mode))


disp_mode_dens <- trapsmerged %>%    # seed density calculations - means per trap
  filter(value != 0, !is.na(family)) %>%
  group_by(data_owner, disp_mode, trap_id) %>%   # grouping by trap_id
  summarise(total_seeds = sum(value),
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
disp_table$patch_log<- log(disp_table$patch_ha)
disp_table$seed_dens_log<- log(disp_table$seed_dens)
#disp_table$prop_spp<- as.integer(disp_table$prop_spp)

write.xlsx(disp_table,"DispTable.xlsx",
            rowNames=F,quote=F)
disp_table_read <- read.xlsx("DispTable.xlsx") #just to check on it

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



##### For *disp mode proportion*
# Forest cover buffers
buffers1 <- c("forest_cover_500m", "forest_cover_1km", "forest_cover_2km", "forest_cover_5km")
disp_table_scaled <- disp_table
for (buffer in buffers1) {disp_table_scaled[[buffer]] <- scale(disp_table[[buffer]])}

results <- list()

for (buffer in buffers1) {model5 <- glmmTMB(prop_spp ~ get(buffer) + (1|locality_name), 
                        data = disp_table[disp_table$disp_mode=="Zoo",], #Zoochorous spp.
                        family = ordbeta)
                        results[[buffer]] <- list(model = model5, aic = AIC(model5))}

aic_values5 <- data.frame(buffer = buffers1, aic = sapply(results, function(x) x$aic))
aic_values5$deltaAIC <- aic_values5$aic - min(aic_values5$aic)
slopes5 <- sapply(results, function(x) summary(x$model)$coefficients$cond[2, 1])
aic_values5$slope <- slopes5


for (buffer in buffers1) {model6 <- glmmTMB(prop_spp ~ get(buffer) + (1|locality_name), 
                        data = disp_table[disp_table$disp_mode=="AneAuto",], #Ane+Auto spp.
                        family = ordbeta)
                        results[[buffer]] <- list(model = model6, aic = AIC(model6))}

aic_values6 <- data.frame(buffer = buffers1, aic = sapply(results, function(x) x$aic))
aic_values6$deltaAIC <- aic_values6$aic - min(aic_values6$aic)
slopes6 <- sapply(results, function(x) summary(x$model)$coefficients$cond[2, 1])
aic_values6$slope <- slopes6

aic_values5$dispersal <- "Zoo"
aic_values6$dispersal <- "AneAuto"

# Combine data frames
aic_values_combined3 <- rbind(aic_values5, aic_values6)

# Rename and reorder buffers in the combined data frame 
aic_values_combined3$buffer <- factor(aic_values_combined3$buffer, 
                                     levels = c("forest_cover_500m", "forest_cover_1km", 
                                                "forest_cover_2km", "forest_cover_5km"),
                                     labels = c("500m", "1km", "2km", "5km"))

e<-ggplot(aic_values_combined3, aes(x = buffer, y = deltaAIC, fill = dispersal)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "prop. of spp. ~ forest cover",
       x = "buffer size",
       y = "delta AIC") +
  ylim(0,2)+
  scale_fill_manual(values = c("Zoo" = "purple4", "AneAuto" = "transparent"),
                    labels = c("Zoo" = "Zoo", "AneAuto" = ""))


f<-ggplot(aic_values_combined3, aes(x = buffer, y = slope, color = dispersal, group = dispersal)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title = "Slopes for forest cover buffers",
       x = "Buffer",
       y = "Slope") +
  ylim(-0.025,0.025)+
  scale_color_manual(values = c("Zoo" = "purple4", "AneAuto" = "darkgoldenrod3"),
                     labels = c("Zoo" = "Zoo", "AneAuto" = "Non-zoo"))



# Total edge buffers
buffers2 <- c("edge_500m", "edge_1km", "edge_2km", "edge_5km")
disp_table_scaled <- disp_table
for (buffer in buffers2) {disp_table_scaled[[buffer]] <- scale(disp_table[[buffer]])}

results <- list()

for (buffer in buffers2) {model7 <- glmmTMB(prop_spp ~ get(buffer) + (1|locality_name), 
                        data = disp_table_scaled[disp_table_scaled$disp_mode=="Zoo",], #Zoochorous spp.
                        family = ordbeta)
                        results[[buffer]] <- list(model = model7, aic = AIC(model7))}

aic_values7 <- data.frame(buffer = buffers2, aic = sapply(results, function(x) x$aic))
aic_values7$deltaAIC <- aic_values7$aic - min(aic_values7$aic)
slopes7 <- sapply(results, function(x) summary(x$model)$coefficients$cond[2, 1])
aic_values7$slope <- slopes7


for (buffer in buffers2) {model8 <- glmmTMB(prop_spp ~ get(buffer) + (1|locality_name), 
                        data = disp_table_scaled[disp_table_scaled$disp_mode=="AneAuto",], #Ane+Auto spp.
                        family = ordbeta)
                        results[[buffer]] <- list(model = model8, aic = AIC(model8))}

aic_values8 <- data.frame(buffer = buffers2, aic = sapply(results, function(x) x$aic))
aic_values8$deltaAIC <- aic_values8$aic - min(aic_values8$aic)
slopes8 <- sapply(results, function(x) summary(x$model)$coefficients$cond[2, 1])
aic_values8$slope <- slopes8

aic_values7$dispersal <- "Zoo"
aic_values8$dispersal <- "AneAuto"

# Combine data frames
aic_values_combined4 <- rbind(aic_values7, aic_values8)

# Rename and reorder buffers in the combined data frame 
aic_values_combined4$buffer <- factor(aic_values_combined4$buffer, 
                                     levels = c("edge_500m", "edge_1km", 
                                                "edge_2km", "edge_5km"),
                                     labels = c("500m", "1km", "2km", "5km"))

g<-ggplot(aic_values_combined4, aes(x = buffer, y = deltaAIC, fill = dispersal)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "prop. of spp. ~ total edge",
       x = "buffer size",
       y = "delta AIC") +
  ylim(0,2)+
  scale_fill_manual(values = c("Zoo" = "purple4", "AneAuto" = "transparent"),
                    labels = c("Zoo" = "Zoo", "AneAuto" = ""))


h<-ggplot(aic_values_combined4, aes(x = buffer, y = slope, color = dispersal, group = dispersal)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title = "Slopes for total edge buffers",
       x = "Buffer",
       y = "Slope") +
  ylim(-0.2,0.2)+
  scale_color_manual(values = c("Zoo" = "purple4", "AneAuto" = "darkgoldenrod3"),
                     labels = c("Zoo" = "Zoo", "AneAuto" = "Non-zoo"))

jpeg("Buffers-PropOnly-OrdbetaZoo.jpeg", width = 17, height = 8,
     units = "cm", pointsize = 9,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(e, g,
             nrow = 1, ncol = 2)
dev.off() 



###### Model selection 
# glmm models for proportion of spp. per disp_mode as the response variable

# For Zoochorous spp. (biotic dispersal)
m_prop1a = glmmTMB(prop_spp~(scale(patch_log)+scale(forest_cover_5km)+
            scale(edge_5km)+ # full model
            scale(prec.total)+scale(tavg))^2+ 
            (1|locality_name), family = ordbeta,
            data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail") 
summary(m_prop1a)
simulationOutput <- simulateResiduals(fittedModel = m_prop1a, plot = TRUE)
vif_results <- check_collinearity(m_prop1a)
print(vif_results)

plot(ggeffects::ggpredict(m_prop1a, terms = c("patch_log")),rawdata=T)
plot(ggeffects::ggpredict(m_prop1a, terms = c("forest_cover_5km")),rawdata=T)
plot(ggeffects::ggpredict(m_prop1a, terms = c("edge_5km")),rawdata=T)
plot(ggeffects::ggpredict(m_prop1a, terms = c("tavg")),rawdata=T)
plot(ggeffects::ggpredict(m_prop1a, terms = c("prec.total")),rawdata=T)
plot(ggeffects::ggpredict(m_prop1a, terms = c("patch_log","prec.total")),rawdata=T)
plot(ggeffects::ggpredict(m_prop1a, terms = c("edge_5km","prec.total")),rawdata=T)
plot(ggeffects::ggpredict(m_prop1a, terms = c("prec.total","tavg")),rawdata=T)
dredgeprop1<-MuMIn::dredge(global.model = m_prop1a, rank = "AIC")
print(dredgeprop1)
write.xlsx(dredgeprop1,"dredge-FULLMODEL-prop1.xlsx",
            rowNames=F,quote=F)


m_prop1b = glmmTMB(prop_spp~scale(patch_log)*scale(forest_cover_5km)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_prop1b)
          simulationOutput <- simulateResiduals(fittedModel = m_prop1b, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop1b, terms = c("patch_log","forest_cover_5km")),rawdata=T)
          vif_results <- check_collinearity(m_prop1b)
          print(vif_results)

m_prop1c = glmmTMB(prop_spp~scale(patch_log)*scale(edge_5km)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_prop1c)
          simulationOutput <- simulateResiduals(fittedModel = m_prop1c, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop1c, terms = c("patch_log","edge_5km")),rawdata=T)
          vif_results <- check_collinearity(m_prop1c)
          print(vif_results)

m_prop1d = glmmTMB(prop_spp~scale(patch_log)*scale(prec.total)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_prop1d)
          simulationOutput <- simulateResiduals(fittedModel = m_prop1d, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop1d, terms = c("patch_log","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_prop1d)
          print(vif_results)

m_prop1e = glmmTMB(prop_spp~scale(patch_log)*scale(tavg)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_prop1e)
          simulationOutput <- simulateResiduals(fittedModel = m_prop1e, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop1e, terms = c("patch_log","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_prop1e)
          print(vif_results)

m_prop1f = glmmTMB(prop_spp~scale(forest_cover_5km)*scale(edge_5km)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_prop1f)
          simulationOutput <- simulateResiduals(fittedModel = m_prop1f, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop1f, terms = c("forest_cover_5km","edge_5km")),rawdata=T)
          vif_results <- check_collinearity(m_prop1f)
          print(vif_results)
          
m_prop1g = glmmTMB(prop_spp~scale(forest_cover_5km)*scale(prec.total)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_prop1g)
          simulationOutput <- simulateResiduals(fittedModel = m_prop1g, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop1g, terms = c("forest_cover_5km","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_prop1g)
          print(vif_results)

m_prop1h = glmmTMB(prop_spp~scale(forest_cover_5km)*scale(tavg)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_prop1h)
          simulationOutput <- simulateResiduals(fittedModel = m_prop1h, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop1h, terms = c("forest_cover_5km","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_prop1h)
          print(vif_results)

m_prop1i = glmmTMB(prop_spp~scale(edge_5km)*scale(prec.total)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_prop1i)
          simulationOutput <- simulateResiduals(fittedModel = m_prop1i, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop1i, terms = c("edge_5km","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_prop1i)
          print(vif_results)

m_prop1j = glmmTMB(prop_spp~scale(edge_5km)*scale(tavg)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_prop1j)
          simulationOutput <- simulateResiduals(fittedModel = m_prop1j, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop1j, terms = c("edge_5km","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_prop1j)
          print(vif_results)

m_prop1k = glmmTMB(prop_spp~scale(prec.total)*scale(tavg)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_prop1k)
          simulationOutput <- simulateResiduals(fittedModel = m_prop1k, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop1k, terms = c("prec.total","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_prop1k)
          print(vif_results)

m_prop1l = glmmTMB(prop_spp~scale(patch_log)+ 
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_prop1l)
          simulationOutput <- simulateResiduals(fittedModel = m_prop1l, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop1l, terms = c("patch_log")),rawdata=T)

m_prop1m = glmmTMB(prop_spp~scale(forest_cover_5km)+ 
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_prop1m)
          simulationOutput <- simulateResiduals(fittedModel = m_prop1m, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop1m, terms = c("forest_cover_5km")),rawdata=T)

m_prop1n = glmmTMB(prop_spp~scale(edge_5km)+ 
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_prop1n)
          simulationOutput <- simulateResiduals(fittedModel = m_prop1n, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop1n, terms = c("edge_5km")),rawdata=T)

m_prop1o = glmmTMB(prop_spp~scale(prec.total)+ 
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_prop1o)
          simulationOutput <- simulateResiduals(fittedModel = m_prop1o, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop1o, terms = c("prec.total")),rawdata=T)

m_prop1p = glmmTMB(prop_spp~scale(tavg)+ 
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_prop1p)
          simulationOutput <- simulateResiduals(fittedModel = m_prop1p, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop1p, terms = c("tavg")),rawdata=T)

m_null1.prop = glmmTMB(prop_spp~1+(1|locality_name),
                  family = ordbeta,
                  data = disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_null1.prop)
          
          
AIC.m_prop1 <- AICtab(m_prop1a,m_prop1b,m_prop1c,m_prop1d,
                      m_prop1e,m_prop1f,m_prop1g,m_prop1h,
                      m_prop1i,m_prop1j,m_prop1k,
                      m_prop1l,m_prop1m,m_prop1n,m_prop1o,m_prop1p,
                      m_null1.prop, delta=T,weights=T)
AIC.m_prop1 <- data.frame(AIC.m_prop1)
write.xlsx(AIC.m_prop1,"AIC-propZoo-Ordbeta-NEW.xlsx",
            rowNames=T,quote=F)


model1o.prop<-capture.output(summary(m_prop1o)) #writing the best model outputs
          write.csv(model1o.prop,"output-model1o-prop-Ordbeta.csv",
          row.names=F,quote=F)
model1a.prop<-capture.output(summary(m_prop1a)) #writing the best model outputs
          write.csv(model1a.prop,"output-model1a-prop-Ordbeta.csv",
          row.names=F,quote=F)
#model1i.prop<-capture.output(summary(m_prop1i)) 
 #         write.csv(model1i.prop,"output-model1i-prop-Ordbeta.csv",
  #        row.names=F,quote=F)
#model1g.prop<-capture.output(summary(m_prop1g)) 
 #         write.csv(model1g.prop,"output-model1g-prop-Ordbeta.csv",
  #        row.names=F,quote=F)


pred_prop1o<-ggpredict(m_prop1o, terms=c("prec.total")) #model predictions
pred_prop1a.1<-ggpredict(m_prop1a, terms=c("forest_cover_5km","prec.total"))
pred_prop1a.2<-ggpredict(m_prop1a, terms=c("edge_5km","prec.total"))
pred_prop1a.3<-ggpredict(m_prop1a, terms=c("prec.total","tavg"))

{a.pred_prop1o<-plot(pred_prop1o,rawdata=T,show_ci=T,colors="viridis")+  
  labs(title="Zoochorous dispersal",x="precipitation (mm)",y="proportion of species", col="")+
  ylim(0,1)+
  theme_classic()+
  theme(legend.position = "none")}

{a.pred_prop1a.1<-plot(pred_prop1a.1,rawdata=T,show_ci=T)+  
  labs(title="Zoochorous dispersal",x="forest cover (%)",y="proportion of species", col="precip. (mm)")+
  ylim(0,1)+
  theme_classic()+
  theme(legend.position = "top",legend.text = element_text(size = 7))}
  
library(scales)
format_k <- function(x) {ifelse(x >= 1000, paste0(x / 1000, "k"), x)} #showing 1000 meters as 1k

{b.pred_prop1a.2<-plot(pred_prop1a.2,rawdata=T,show_ci=T)+  
  labs(title="",x="edge (m)",y="proportion of species", col="precip. (mm)")+
  ylim(0,1)+
  theme_classic()+
  theme(legend.position = "top",legend.text = element_text(size = 7))+
    scale_x_continuous(labels = format_k)}
    #scale_x_continuous(labels = label_number(scale = 1e-3, suffix = "k"))}

{c.pred_prop1a.3<-plot(pred_prop1a.3,rawdata=T,show_ci=T)+  
  labs(title="",x="precipitation (mm)",y="proportion of species", col="temp. (°C)")+
  ylim(0,1)+
  theme_classic()+
  theme(legend.position = "top",legend.text = element_text(size = 7))}


jpeg("PropSpp-ZooOnly-Ordbeta-Dez2024.jpeg", width = 8.5, height = 8.5,
     units = "cm", pointsize = 9,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(a.pred_prop1o)
dev.off() 

jpeg("PropSpp-ZooOther-Ordbeta-Dez2024.jpeg", width = 8.5, height = 22,
     units = "cm", pointsize = 9,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(a.pred_prop1a.1,b.pred_prop1a.2,c.pred_prop1a.3, nrow = 3, ncol = 1)
dev.off() 



# For Anemo + Autochorous spp. (abiotic dispersal)
# role of disp_mode
disp_table_filtered <- disp_table[disp_table$disp_mode != "Undet", ]

m_prop2_disp = glmmTMB(prop_spp~disp_mode+ 
            (1|locality_name), family = ordbeta,
            data=disp_table_filtered, na.action = "na.fail") 
summary(m_prop2_disp)
simulationOutput <- simulateResiduals(fittedModel = m_prop2_disp, plot = TRUE)


m_prop2a = glmmTMB(prop_spp~(scale(patch_log)+scale(forest_cover_5km)+
            scale(edge_5km)+ # full model
            scale(prec.total)+scale(tavg))^2+ 
            (1|locality_name), family = ordbeta,
            data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail") 
summary(m_prop2a)
simulationOutput <- simulateResiduals(fittedModel = m_prop2a, plot = TRUE)
vif_results <- check_collinearity(m_prop2a)
print(vif_results)

plot(ggeffects::ggpredict(m_prop2a, terms = c("patch_log")),rawdata=T)
plot(ggeffects::ggpredict(m_prop2a, terms = c("forest_cover_5km")),rawdata=T)
plot(ggeffects::ggpredict(m_prop2a, terms = c("patch_log","forest_cover_5km")),rawdata=T)
plot(ggeffects::ggpredict(m_prop2a, terms = c("patch_log","edge_5km")),rawdata=T)
plot(ggeffects::ggpredict(m_prop2a, terms = c("edge_5km")),rawdata=T)
plot(ggeffects::ggpredict(m_prop2a, terms = c("prec.total","disp_mode")),rawdata=T)
dredgeprop2<-MuMIn::dredge(global.model = m_prop2a, rank = "AIC")
print(dredgeprop2)
write.xlsx(dredgeprop2,"dredge-FULLMODEL-prop2.xlsx",
            rowNames=F,quote=F)


m_prop2b = glmmTMB(prop_spp~scale(patch_log)*scale(forest_cover_5km)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_prop2b)
          simulationOutput <- simulateResiduals(fittedModel = m_prop2b, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop2b, terms = c("patch_log","forest_cover_5km")),rawdata=T)
          vif_results <- check_collinearity(m_prop2b)
          print(vif_results)

m_prop2c = glmmTMB(prop_spp~scale(patch_log)*scale(edge_5km)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_prop2c)
          simulationOutput <- simulateResiduals(fittedModel = m_prop2c, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop2c, terms = c("patch_log","edge_5km")),rawdata=T)
          vif_results <- check_collinearity(m_prop2c)
          print(vif_results)

m_prop2d = glmmTMB(prop_spp~scale(patch_log)*scale(prec.total)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_prop2d)
          simulationOutput <- simulateResiduals(fittedModel = m_prop2d, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop2d, terms = c("patch_log","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_prop2d)
          print(vif_results)

m_prop2e = glmmTMB(prop_spp~scale(patch_log)*scale(tavg)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_prop2e)
          simulationOutput <- simulateResiduals(fittedModel = m_prop2e, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop2e, terms = c("patch_log","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_prop2e)
          print(vif_results)

m_prop2f = glmmTMB(prop_spp~scale(forest_cover_5km)*scale(edge_5km)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_prop2f)
          simulationOutput <- simulateResiduals(fittedModel = m_prop2f, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop2f, terms = c("forest_cover_5km","edge_5km")),rawdata=T)
          vif_results <- check_collinearity(m_prop2f)
          print(vif_results)
          
m_prop2g = glmmTMB(prop_spp~scale(forest_cover_5km)*scale(prec.total)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_prop2g)
          simulationOutput <- simulateResiduals(fittedModel = m_prop2g, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop2g, terms = c("forest_cover_5km","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_prop2g)
          print(vif_results)

m_prop2h = glmmTMB(prop_spp~scale(forest_cover_5km)*scale(tavg)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_prop2h)
          simulationOutput <- simulateResiduals(fittedModel = m_prop2h, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop2h, terms = c("forest_cover_5km","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_prop2h)
          print(vif_results)

m_prop2i = glmmTMB(prop_spp~scale(edge_5km)*scale(prec.total)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_prop2i)
          simulationOutput <- simulateResiduals(fittedModel = m_prop2i, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop2i, terms = c("edge_5km","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_prop2i)
          print(vif_results)

m_prop2j = glmmTMB(prop_spp~scale(edge_5km)*scale(tavg)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_prop2j)
          simulationOutput <- simulateResiduals(fittedModel = m_prop2j, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop2j, terms = c("edge_5km","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_prop2j)
          print(vif_results)

m_prop2k = glmmTMB(prop_spp~scale(prec.total)*scale(tavg)+
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_prop2k)
          simulationOutput <- simulateResiduals(fittedModel = m_prop2k, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop2k, terms = c("prec.total","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_prop2k)
          print(vif_results)

m_prop2l = glmmTMB(prop_spp~scale(patch_log)+ 
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_prop2l)
          simulationOutput <- simulateResiduals(fittedModel = m_prop2l, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop2l, terms = c("patch_log")),rawdata=T)

m_prop2m = glmmTMB(prop_spp~scale(forest_cover_5km)+ 
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_prop2m)
          simulationOutput <- simulateResiduals(fittedModel = m_prop2m, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop2m, terms = c("forest_cover_5km")),rawdata=T)

m_prop2n = glmmTMB(prop_spp~scale(edge_5km)+ 
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_prop2n)
          simulationOutput <- simulateResiduals(fittedModel = m_prop2n, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop2n, terms = c("edge_5km")),rawdata=T)

m_prop2o = glmmTMB(prop_spp~scale(prec.total)+ 
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_prop2o)
          simulationOutput <- simulateResiduals(fittedModel = m_prop2o, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop2o, terms = c("prec.total")),rawdata=T)

m_prop2p = glmmTMB(prop_spp~scale(tavg)+ 
              (1|locality_name), family = ordbeta,
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_prop2p)
          simulationOutput <- simulateResiduals(fittedModel = m_prop2p, plot = TRUE)
          plot(ggeffects::ggpredict(m_prop2p, terms = c("tavg")),rawdata=T)

m_null2.prop = glmmTMB(prop_spp~1+(1|locality_name),
                  family = ordbeta,
                  data = disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_null2.prop)
          
          
AIC.m_prop2 <- AICtab(m_prop2a,m_prop2b,m_prop2c,m_prop2d,
                      m_prop2e,m_prop2f,m_prop2g,m_prop2h,
                      m_prop2i,m_prop2j,m_prop2k,
                      m_prop2l,m_prop2m,m_prop2n,m_prop2o,m_prop2p,
                      m_null2.prop, delta=T,weights=T)
AIC.m_prop2 <- data.frame(AIC.m_prop2)
write.xlsx(AIC.m_prop2,"AIC-propAneAuto-Ordbeta-NEW.xlsx",
            rowNames=T,quote=F)


model2o.prop<-capture.output(summary(m_prop2o)) #writing the best model outputs
          write.csv(model2o.prop,"output-model2o-prop-Ordbeta.csv",
          row.names=F,quote=F)

          
pred_prop2o<-ggpredict(m_prop2o, terms=c("prec.total")) #model predictions

{a.pred_prop2o<-plot(pred_prop2o,rawdata=T,show_ci=T, colors = "flat")+  
  labs(title="Per dispersal mode",x="precipitation (mm)",y="proportion of spp.", col="")+
  ylim(0,1)+
  theme_classic()+
  theme(legend.position = "top",legend.text = element_text(size = 7))}
    #scale_color_manual(values = c("darkgoldenrod3","purple4"),  # trying to change colors manually
    #labels = c("Abiotic", "Biotic"))+
    #scale_fill_manual(values = c("darkgoldenrod3", "purple4"),    # for cI bands
     #               labels = c("Abiotic", "Biotic"))}

#pred_prop.disp2o_zoo <- pred_prop.disp2o[pred_prop.disp2o$group == "Zoo",] # plot/select a single disp_mode

#{a.pred_prop.disp2o_zoo<-plot(pred_prop.disp2o_zoo,rawdata=T,show_ci=T)+  
 # labs(title="Biotic dispersal",x="precipitation (mm)",y="proportion of spp.", col="")+
  #  geom_line(color = "purple4") +
  #ylim(0,1)+
  #theme_classic()+
  #theme(legend.position = "none")+
   # scale_color_manual(values = "purple4")+
    #scale_fill_manual(values = "purple4") +
    #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "purple4", alpha = 0.2)}


#jpeg("PropSpp-Ordbeta-Oct2024.jpeg", width = 8.5, height = 8.5,
 #    units = "cm", pointsize = 9,
  #   bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
#grid.arrange(a.pred_prop.disp2o)
#dev.off() 


#jpeg("PropSppZoo-Ordbeta-Oct2024.jpeg", width = 8.5, height = 8,
 #    units = "cm", pointsize = 9,
  #   bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
#grid.arrange(a.pred_prop.disp2o_zoo)
#dev.off() 





##### For *seed density*
# Forest cover buffers
buffers1 <- c("forest_cover_500m", "forest_cover_1km", "forest_cover_2km", "forest_cover_5km")
disp_table_scaled <- disp_table
for (buffer in buffers1) {disp_table_scaled[[buffer]] <- scale(disp_table[[buffer]])}

results <- list()

for (buffer in buffers1) {model1 <- glmmTMB(seed_dens_log ~ get(buffer) + (1|locality_name), 
                        data = disp_table[disp_table$disp_mode=="Zoo",]) #Zoochorous spp.
                        #family = nbinom2(link = "log"))
                        results[[buffer]] <- list(model = model1, aic = AIC(model1))}

aic_values1 <- data.frame(buffer = buffers1, aic = sapply(results, function(x) x$aic))
aic_values1$deltaAIC <- aic_values1$aic - min(aic_values1$aic)
slopes1 <- sapply(results, function(x) summary(x$model)$coefficients$cond[2, 1])
aic_values1$slope <- slopes1


for (buffer in buffers1) {model2 <- glmmTMB(seed_dens_log ~ get(buffer) + (1|locality_name), 
                        data = disp_table[disp_table$disp_mode=="AneAuto",]) #Ane+Auto spp.
                        #family = nbinom2(link = "log"))
                        results[[buffer]] <- list(model = model2, aic = AIC(model2))}

aic_values2 <- data.frame(buffer = buffers1, aic = sapply(results, function(x) x$aic))
aic_values2$deltaAIC <- aic_values2$aic - min(aic_values2$aic)
slopes2 <- sapply(results, function(x) summary(x$model)$coefficients$cond[2, 1])
aic_values2$slope <- slopes2

aic_values1$dispersal <- "Zoo"
aic_values2$dispersal <- "AneAuto"

# Combine data frames
aic_values_combined1 <- rbind(aic_values1, aic_values2)

# Rename and reorder buffers in the combined data frame 
aic_values_combined1$buffer <- factor(aic_values_combined1$buffer, 
                                     levels = c("forest_cover_500m", "forest_cover_1km", 
                                                "forest_cover_2km", "forest_cover_5km"),
                                     labels = c("500m", "1km", "2km", "5km"))

a<-ggplot(aic_values_combined1, aes(x = buffer, y = deltaAIC, fill = dispersal)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "seed dens. ~ forest cover",
       x = "buffer size",
       y = "delta AIC") +
  ylim(0,6)+
  scale_fill_manual(values = c("Zoo" = "purple4", "AneAuto" = "darkgoldenrod3"),
                    labels = c("Zoo" = "Zoo", "AneAuto" = "Non-Zoo"))


b<-ggplot(aic_values_combined1, aes(x = buffer, y = slope, color = dispersal, group = dispersal)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title = "Slopes for forest cover buffers",
       x = "Buffer",
       y = "Slope") +
  ylim(-0.05,0.05)+
  scale_color_manual(values = c("Zoo" = "purple4", "AneAuto" = "darkgoldenrod3"),
                     labels = c("Zoo" = "Zoo", "AneAuto" = "Non-Zoo"))



# Total edge buffers
buffers2 <- c("edge_500m", "edge_1km", "edge_2km", "edge_5km")
disp_table_scaled <- disp_table
for (buffer in buffers2) {disp_table_scaled[[buffer]] <- scale(disp_table[[buffer]])}

results <- list()

for (buffer in buffers2) {model3 <- glmmTMB(seed_dens_log ~ get(buffer) + (1|locality_name), 
                        data = disp_table_scaled[disp_table_scaled$disp_mode=="Zoo",]) #Zoochorous spp.
                        #family = nbinom2(link = "log"))
                        results[[buffer]] <- list(model = model3, aic = AIC(model3))}

aic_values3 <- data.frame(buffer = buffers2, aic = sapply(results, function(x) x$aic))
aic_values3$deltaAIC <- aic_values3$aic - min(aic_values3$aic)
slopes3 <- sapply(results, function(x) summary(x$model)$coefficients$cond[2, 1])
aic_values3$slope <- slopes3


for (buffer in buffers2) {model4 <- glmmTMB(seed_dens_log ~ get(buffer) + (1|locality_name), 
                        data = disp_table_scaled[disp_table_scaled$disp_mode=="AneAuto",]) #Ane+Auto spp.
                        #family = nbinom2(link = "log"))
                        results[[buffer]] <- list(model = model4, aic = AIC(model4))}

aic_values4 <- data.frame(buffer = buffers2, aic = sapply(results, function(x) x$aic))
aic_values4$deltaAIC <- aic_values4$aic - min(aic_values4$aic)
slopes4 <- sapply(results, function(x) summary(x$model)$coefficients$cond[2, 1])
aic_values4$slope <- slopes4

aic_values3$dispersal <- "Zoo"
aic_values4$dispersal <- "AneAuto"

# Combine data frames
aic_values_combined2 <- rbind(aic_values3, aic_values4)

# Rename and reorder buffers in the combined data frame 
aic_values_combined2$buffer <- factor(aic_values_combined2$buffer, 
                                     levels = c("edge_500m", "edge_1km", 
                                                "edge_2km", "edge_5km"),
                                     labels = c("500m", "1km", "2km", "5km"))

c<-ggplot(aic_values_combined2, aes(x = buffer, y = deltaAIC, fill = dispersal)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "seed dens. ~ total edge",
       x = "buffer size",
       y = "delta AIC") +
  ylim(0,6)+
  scale_fill_manual(values = c("Zoo" = "purple4", "AneAuto" = "darkgoldenrod3"),
                    labels = c("Zoo" = "Zoo", "AneAuto" = "Non-Zoo"))


d<-ggplot(aic_values_combined2, aes(x = buffer, y = slope, color = dispersal, group = dispersal)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title = "Slopes for total edge buffers",
       x = "Buffer",
       y = "Slope") +
  ylim(-1,1)+
  scale_color_manual(values = c("Zoo" = "purple4", "AneAuto" = "darkgoldenrod3"),
                     labels = c("Zoo" = "Zoo", "AneAuto" = "Non-Zoo"))

# Figure for seed density buffers
jpeg("Buffers-PropAndDens-OrdbetaAndlog-Dez23-2024.jpeg", width = 17, height = 15,
     units = "cm", pointsize = 9,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(e, g, a, c,
             nrow = 2, ncol = 2)
dev.off() 



###### Model selection 
# glmm models for seed density as the response variable

# For Zoochorous spp.
m_dens1a = glmmTMB(seed_dens_log~(scale(patch_log)+scale(forest_cover_500m)+
            scale(edge_500m)+ # full model
            scale(prec.total)+scale(tavg))^2+ 
            (1|locality_name), #family = nbinom2(link = "log"),
            data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail") 
summary(m_dens1a)
simulationOutput <- simulateResiduals(fittedModel = m_dens1a, plot = TRUE)
vif_results <- check_collinearity(m_dens1a)
print(vif_results)
plot(ggeffects::ggpredict(m_dens1a, terms = c("disp_mode")),rawdata=F)
plot(ggeffects::ggpredict(m_dens1a, terms = c("patch_log")),rawdata=T)
plot(ggeffects::ggpredict(m_dens1a, terms = c("forest_cover_500m")),rawdata=T)
plot(ggeffects::ggpredict(m_dens1a, terms = c("patch_log","forest_cover_500m")),rawdata=T)
plot(ggeffects::ggpredict(m_dens1a, terms = c("patch_log","edge_500m")),rawdata=T)
plot(ggeffects::ggpredict(m_dens1a, terms = c("edge_500m")),rawdata=T)
plot(ggeffects::ggpredict(m_dens1a, terms = c("prec.total")),rawdata=T)
dredgedens1<-MuMIn::dredge(global.model = m_dens1a, rank = "AIC")
print(dredgedens1)
write.xlsx(dredgedens1,"dredge-FULLMODEL-dens1.xlsx",
            rowNames=F,quote=F)


m_dens1b = glmmTMB(seed_dens_log~scale(patch_log)*scale(forest_cover_500m)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_dens1b)
          simulationOutput <- simulateResiduals(fittedModel = m_dens1b, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens1b, terms = c("patch_log","forest_cover_500m")),rawdata=T)
          vif_results <- check_collinearity(m_dens1b)
          print(vif_results)

m_dens1c = glmmTMB(seed_dens_log~scale(patch_log)*scale(edge_500m)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_dens1c)
          simulationOutput <- simulateResiduals(fittedModel = m_dens1c, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens1c, terms = c("patch_log","edge_500m")),rawdata=T)
          vif_results <- check_collinearity(m_dens1c)
          print(vif_results)

m_dens1d = glmmTMB(seed_dens_log~scale(patch_log)*scale(prec.total)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_dens1d)
          simulationOutput <- simulateResiduals(fittedModel = m_dens1d, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens1d, terms = c("patch_log","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_dens1d)
          print(vif_results)
          
m_dens1e = glmmTMB(seed_dens_log~scale(patch_log)*scale(tavg)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_dens1e)
          simulationOutput <- simulateResiduals(fittedModel = m_dens1e, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens1e, terms = c("patch_log","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_dens1e)
          print(vif_results)
          
m_dens1f = glmmTMB(seed_dens_log~scale(forest_cover_500m)*scale(edge_500m)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_dens1f)
          simulationOutput <- simulateResiduals(fittedModel = m_dens1f, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens1f, terms = c("forest_cover_500m","edge_500m")),rawdata=T)
          vif_results <- check_collinearity(m_dens1f)
          print(vif_results)
                    
m_dens1g = glmmTMB(seed_dens_log~scale(forest_cover_500m)*scale(prec.total)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_dens1g)
          simulationOutput <- simulateResiduals(fittedModel = m_dens1g, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens1g, terms = c("forest_cover_500m","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_dens1g)
          print(vif_results)
          
m_dens1h = glmmTMB(seed_dens_log~scale(forest_cover_500m)*scale(tavg)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_dens1h)
          simulationOutput <- simulateResiduals(fittedModel = m_dens1h, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens1h, terms = c("forest_cover_500m","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_dens1h)
          print(vif_results)
          
m_dens1i = glmmTMB(seed_dens_log~scale(edge_500m)*scale(prec.total)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_dens1i)
          simulationOutput <- simulateResiduals(fittedModel = m_dens1i, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens1i, terms = c("edge_500m","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_dens1i)
          print(vif_results)
          
m_dens1j = glmmTMB(seed_dens_log~scale(edge_500m)*scale(tavg)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_dens1j)
          simulationOutput <- simulateResiduals(fittedModel = m_dens1j, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens1j, terms = c("edge_500m","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_dens1j)
          print(vif_results)
          
m_dens1k = glmmTMB(seed_dens_log~scale(prec.total)*scale(tavg)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_dens1k)
          simulationOutput <- simulateResiduals(fittedModel = m_dens1k, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens1k, terms = c("prec.total","disp_mode")),rawdata=T)
          vif_results <- check_collinearity(m_dens1k)
          print(vif_results)
          
m_dens1l = glmmTMB(seed_dens_log~scale(patch_log)+ 
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_dens1l)
          simulationOutput <- simulateResiduals(fittedModel = m_dens1l, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens1l, terms = c("patch_log")),rawdata=T)
          
m_dens1m = glmmTMB(seed_dens_log~scale(forest_cover_500m)+ 
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_dens1m)
          simulationOutput <- simulateResiduals(fittedModel = m_dens1m, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens1m, terms = c("forest_cover_500m")),rawdata=T)

m_dens1n = glmmTMB(seed_dens_log~scale(edge_500m)+ 
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_dens1n)
          simulationOutput <- simulateResiduals(fittedModel = m_dens1n, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens1n, terms = c("edge_500m")),rawdata=T)

m_dens1o = glmmTMB(seed_dens_log~scale(prec.total)+ 
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_dens1o)
          simulationOutput <- simulateResiduals(fittedModel = m_dens1o, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens1o, terms = c("prec.total")),rawdata=T)

m_dens1p = glmmTMB(seed_dens_log~scale(tavg)+ 
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_dens1p)
          simulationOutput <- simulateResiduals(fittedModel = m_dens1p, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens1p, terms = c("tavg","disp_mode")),rawdata=T)

m_null1.dens = glmmTMB(seed_dens_log~1+(1|locality_name),
                  #family = nbinom2(link = "log"),
                  data = disp_table[disp_table$disp_mode=="Zoo",], na.action = "na.fail")
          summary(m_null1)
          
          
AIC.m_dens1 <- AICtab(m_dens1a,m_dens1b,m_dens1c,m_dens1d,
                      m_dens1e,m_dens1f,m_dens1g,m_dens1h,
                      m_dens1i,m_dens1j,m_dens1k,
                      m_dens1l,m_dens1m,m_dens1n,m_dens1o,m_dens1p,
                      m_null1.dens, delta=T,weights=T)
AIC.m_dens1 <- data.frame(AIC.m_dens1)
write.xlsx(AIC.m_dens1,"AIC-densZoo-log-NEW.xlsx",
            rowNames=T,quote=F)


model1n.dens<-capture.output(summary(m_dens1n)) #writing the best model outputs
          write.csv(model1n.dens,"output-model1n-dens-log.csv",
          row.names=F,quote=F)
model1i.dens<-capture.output(summary(m_dens1i)) 
          write.csv(model1i.dens,"output-model1i-dens-log.csv",
          row.names=F,quote=F)
model1o.dens<-capture.output(summary(m_dens1o)) 
          write.csv(model1o.dens,"output-model1o-dens-log.csv",
          row.names=F,quote=F)


pred_dens1n<-ggpredict(m_dens1n, terms=c("edge_500m")) #model predictions
pred_dens1i<-ggpredict(m_dens1i, terms=c("edge_500m","prec.total"))
pred_dens1o<-ggpredict(m_dens1o, terms=c("prec.total"))

{a.pred_dens1n<-plot(pred_dens1n,rawdata=T,show_ci=T,colors="viridis")+  
  labs(title="",x="edge (m)",y="seed density (log)", col="")+
  #ylim(0,6)+
  theme_classic()+
  theme(legend.position = "top",legend.text = element_text(size = 7))}

{b.pred_dens1i<-plot(pred_dens1i,rawdata=T,show_ci=T)+  
  labs(title="",x="edge (m)",y="seed density (log)", col="precip. (mm)")+
  #ylim(0,6)+
  theme_classic()+
  theme(legend.position = "top",legend.text = element_text(size = 7))}

{c.pred_dens1o<-plot(pred_dens1o,rawdata=T,show_ci=T,colors="viridis")+  
  labs(title="Zoochorous dispersal",x="precipitation (mm)",y="seed density (log)", col="")+
  #ylim(0,6)+
  theme_classic()+
  theme(legend.position = "none")}


# For Anemo + Autochorous spp.
# role of disp_mode
disp_table_filtered <- disp_table[disp_table$disp_mode != "Undet", ]

m_dens2_disp = glmmTMB(seed_dens_log~disp_mode+ 
            (1|locality_name), 
            data=disp_table_filtered, na.action = "na.fail") 
summary(m_dens2_disp)
simulationOutput <- simulateResiduals(fittedModel = m_dens2_disp, plot = TRUE)


m_dens2a = glmmTMB(seed_dens_log~(scale(patch_log)+scale(forest_cover_500m)+
            scale(edge_500m)+ # full model
            scale(prec.total)+scale(tavg))^2+ 
            (1|locality_name), #family = nbinom2(link = "log"),
            data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail") 
summary(m_dens2a)
simulationOutput <- simulateResiduals(fittedModel = m_dens2a, plot = TRUE)
vif_results <- check_collinearity(m_dens2a)
print(vif_results)
          
plot(ggeffects::ggpredict(m_dens2a, terms = c("patch_log")),rawdata=T)
plot(ggeffects::ggpredict(m_dens2a, terms = c("forest_cover_500m")),rawdata=T)
plot(ggeffects::ggpredict(m_dens2a, terms = c("patch_log","forest_cover_500m")),rawdata=T)
plot(ggeffects::ggpredict(m_dens2a, terms = c("patch_log","edge_500m")),rawdata=T)
plot(ggeffects::ggpredict(m_dens2a, terms = c("edge_500m")),rawdata=T)
plot(ggeffects::ggpredict(m_dens2a, terms = c("tavg","disp_mode")),rawdata=T)
plot(ggeffects::ggpredict(m_dens2a, terms = c("prec.total","disp_mode")),rawdata=T)
dredgedens2<-MuMIn::dredge(global.model = m_dens2a, rank = "AIC")
print(dredgedens2)
write.xlsx(dredgedens2,"dredge-FULLMODEL-dens2.xlsx",
            rowNames=F,quote=F)


m_dens2b = glmmTMB(seed_dens_log~scale(patch_log)*scale(forest_cover_500m)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_dens2b)
          simulationOutput <- simulateResiduals(fittedModel = m_dens2b, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens2b, terms = c("patch_log","forest_cover_500m")),rawdata=T)
          vif_results <- check_collinearity(m_dens2b)
          print(vif_results)

m_dens2c = glmmTMB(seed_dens_log~scale(patch_log)*scale(edge_500m)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_dens2c)
          simulationOutput <- simulateResiduals(fittedModel = m_dens2c, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens2c, terms = c("patch_log","edge_500m")),rawdata=T)
          vif_results <- check_collinearity(m_dens2c)
          print(vif_results)

m_dens2d = glmmTMB(seed_dens_log~scale(patch_log)*scale(prec.total)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_dens2d)
          simulationOutput <- simulateResiduals(fittedModel = m_dens2d, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens2d, terms = c("patch_log","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_dens2d)
          print(vif_results)

m_dens2e = glmmTMB(seed_dens_log~scale(patch_log)*scale(tavg)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_dens2e)
          simulationOutput <- simulateResiduals(fittedModel = m_dens2e, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens2e, terms = c("patch_log","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_dens2e)
          print(vif_results)

m_dens2f = glmmTMB(seed_dens_log~scale(forest_cover_500m)*scale(edge_500m)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_dens2f)
          simulationOutput <- simulateResiduals(fittedModel = m_dens2f, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens2f, terms = c("forest_cover_500m","edge_500m")),rawdata=T)
          vif_results <- check_collinearity(m_dens2f)
          print(vif_results)
          
m_dens2g = glmmTMB(seed_dens_log~scale(forest_cover_500m)*scale(prec.total)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_dens2g)
          simulationOutput <- simulateResiduals(fittedModel = m_dens2g, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens2g, terms = c("forest_cover_500m","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_dens2g)
          print(vif_results)

m_dens2h = glmmTMB(seed_dens_log~scale(forest_cover_500m)*scale(tavg)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_dens2h)
          simulationOutput <- simulateResiduals(fittedModel = m_dens2h, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens2h, terms = c("forest_cover_500m","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_dens2h)
          print(vif_results)

m_dens2i = glmmTMB(seed_dens_log~scale(edge_500m)*scale(prec.total)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_dens2i)
          simulationOutput <- simulateResiduals(fittedModel = m_dens2i, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens2i, terms = c("edge_500m","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_dens2i)
          print(vif_results)

m_dens2j = glmmTMB(seed_dens_log~scale(edge_500m)*scale(tavg)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_dens2j)
          simulationOutput <- simulateResiduals(fittedModel = m_dens2j, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens2j, terms = c("edge_500m","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_dens2j)
          print(vif_results)

m_dens2k = glmmTMB(seed_dens_log~scale(prec.total)*scale(tavg)+
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_dens2k)
          simulationOutput <- simulateResiduals(fittedModel = m_dens2k, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens2k, terms = c("prec.total","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_dens2k)
          print(vif_results)

m_dens2l = glmmTMB(seed_dens_log~scale(patch_log)+ 
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_dens2l)
          simulationOutput <- simulateResiduals(fittedModel = m_dens2l, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens2l, terms = c("patch_log")),rawdata=T)

m_dens2m = glmmTMB(seed_dens_log~scale(forest_cover_500m)+ 
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_dens2m)
          simulationOutput <- simulateResiduals(fittedModel = m_dens2m, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens2m, terms = c("forest_cover_500m")),rawdata=T)

m_dens2n = glmmTMB(seed_dens_log~scale(edge_500m)+ 
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_dens2n)
          simulationOutput <- simulateResiduals(fittedModel = m_dens2n, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens2n, terms = c("edge_500m")),rawdata=T)

m_dens2o = glmmTMB(seed_dens_log~scale(prec.total)+ 
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_dens2o)
          simulationOutput <- simulateResiduals(fittedModel = m_dens2o, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens2o, terms = c("prec.total")),rawdata=T)

m_dens2p = glmmTMB(seed_dens_log~scale(tavg)+ 
              (1|locality_name), #family = nbinom2(link = "log"),
              data=disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_dens2p)
          simulationOutput <- simulateResiduals(fittedModel = m_dens2p, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens2p, terms = c("tavg")),rawdata=T)

m_null2.dens = glmmTMB(seed_dens_log~1+(1|locality_name),
                  #family = nbinom2(link = "log"),
                  data = disp_table[disp_table$disp_mode=="AneAuto",], na.action = "na.fail")
          summary(m_null2)
          
          
AIC.m_dens2 <- AICtab(m_dens2a,m_dens2b,m_dens2c,m_dens2d,
                      m_dens2e,m_dens2f,m_dens2g,m_dens2h,
                      m_dens2i,m_dens2j,m_dens2k,
                      m_dens2l,m_dens2m,m_dens2n,m_dens2o,m_dens2p,
                      m_null2.dens, delta=T,weights=T)
AIC.m_dens2 <- data.frame(AIC.m_dens2)
write.xlsx(AIC.m_dens2,"AIC-densAneAuto-log-NEW.xlsx",
            rowNames=T,quote=F)


model2p.dens<-capture.output(summary(m_dens2p)) #writing the best model outputs
          write.csv(model2p.dens,"output-model2p-dens-log.csv",
          row.names=F,quote=F)
model2j.dens<-capture.output(summary(m_dens2j)) 
          write.csv(model2j.dens,"output-model2j-dens-log.csv",
          row.names=F,quote=F)
model2n.dens<-capture.output(summary(m_dens2n)) 
          write.csv(model2n.dens,"output-model2n-dens-log.csv",
          row.names=F,quote=F)


pred_dens2p<-ggpredict(m_dens2p, terms=c("tavg")) #model predictions
pred_dens2j<-ggpredict(m_dens2j, terms=c("edge_500m","tavg"))
pred_dens2n<-ggpredict(m_dens2n, terms=c("edge_500m"))

{a.pred_dens2p<-plot(pred_dens2p,rawdata=T,show_ci=T,colors="#E69F00")+  
  labs(title="Non-Zoochorous dispersal",x="temperature (°C)",y="seed density (log)", col="")+
  #ylim(0,6)+
  theme_classic()+
  theme(legend.position = "none")}

{b.pred_dens2j<-plot(pred_dens2j,rawdata=T,show_ci=T)+  
  labs(title="",x="edge (m)",y="seed density (log)", col="temp. (°C)")+
  #ylim(0,6)+
  theme_classic()+
  theme(legend.position = "top",legend.text = element_text(size = 7))}

{c.pred_dens2n<-plot(pred_dens2n,rawdata=T,show_ci=T,colors="flat")+  
  labs(title="",x="edge (m)",y="seed density (log)", col="")+
  #ylim(-3,7)+
  theme_classic()+
  theme(legend.position = "none")}

#library(patchwork)

# Combine os gráficos
#combined_plot <- c.pred_dens1n + c.pred_dens2n

# Exiba o gráfico combinado
#combined_plot

# Obtenha os dados de predição de ambos os modelos
#df1 <- as.data.frame(pred_dens1n)  # Predições do primeiro modelo
#df2 <- as.data.frame(pred_dens2n)  # Predições do segundo modelo

# Adicione uma coluna para diferenciar as predições
#df1$model <- "Model 1"
#df2$model <- "Model 2"

# Combine os dataframes
#combined_df <- rbind(df1, df2)

# Plote os dois modelos no mesmo gráfico
#combined_plot <- ggplot(combined_df, aes(x = x, y = predicted, color = model)) +
 # geom_line() +  # Linhas de predição
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = model), alpha = 0.2) +  # Intervalos de confiança
  #geom_point(data = combined_df, aes(x = x, y = predicted), shape = 1) +  # Dados brutos
  #labs(title = "", x = "Edge (m)", y = "Seed density (log)") +
  #theme_classic() +
  #theme(legend.position = "top", legend.text = element_text(size = 7))

# Exiba o gráfico combinado
#combined_plot

m_dens_edge = glmmTMB(seed_dens_log~scale(edge_500m)*disp_mode+ # a single model for edge per disp_mode
              (1|locality_name), 
              data=disp_table_filtered, na.action = "na.fail")
          summary(m_dens_edge)
          simulationOutput <- simulateResiduals(fittedModel = m_dens_edge, plot = TRUE)
          plot(ggeffects::ggpredict(m_dens_edge, terms = c("edge_500m","disp_mode")),rawdata=T)

pred_dens_edge<-ggpredict(m_dens_edge, terms=c("edge_500m","disp_mode"))

# Carregar o pacote
library(viridis)
library(RColorBrewer)

okabe_ito_colors()

# Paleta "viridis"
viridis_colors <- viridis(8)
viridis_colors <- flat(8)

#Paleta RColor
set1_colors <- brewer.pal(9, "Set1")
print(viridis_colors)

show_pals("viridis")

{c.pred_dens_edge<-plot(pred_dens_edge,rawdata=T,show_ci=T,colors=c("#E69F00","#440154FF"))+  
  labs(title="Both dispersal modes",x="edge (m)",y="seed density (log)", col="")+
  #ylim(0,6)+
  theme_classic()+
  theme(legend.position = c(0.8,0.95), legend.text = element_text(size = 7),
        legend.direction = "vertical", legend.background = element_blank())+
  scale_color_manual(values = c("#E69F00", "#440154FF"),
                     labels = c("AneAuto" = "Non-Zoo", "Zoo" = "Zoo"))+
  scale_fill_manual(values = c("#E69F00", "#440154FF"))}



jpeg("SeedDensMainEdge-Dez2024.jpeg", width = 8.5, height = 22,
     units = "cm", pointsize = 9,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(a.pred_dens2p, c.pred_dens_edge,
             c.pred_dens1o, 
             nrow = 3, ncol = 1)
dev.off() 


jpeg("SeedDensMain-Oct2024.jpeg", width = 17, height = 16,
     units = "cm", pointsize = 9,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(a.pred_dens2p, a.pred_dens1o, 
             c.pred_dens2n, c.pred_dens1n,
             nrow = 2, ncol = 2)
dev.off() 








