install.packages("")
# Set working directory if needed
#setwd("") 
getwd()
citation("ncf")


# Load packages 
packages <- c("vegan","multcomp","lme4","languageR","bbmle","ggplot2","dplyr","gridExtra",
              "glmmTMB","DHARMa","readxl","openxlsx","ppsr","tidyr","stats",
              "ggcorrplot","corrplot","ggthemes","ggeffects","MuMIn","car",
              "interactions","performance","mgcv","glmmLasso","spdep","ncf")

lapply(packages,require,character.only = TRUE)
warnings()
citation("ncf")

# Calling data frames: 

landscape4 <- read_excel("LandscapeFinalEdited-Sep2025-Fulbert2.xlsx")
str(landscape4)
landscape4$patch_size_log<-log(landscape4$patch_size_ha)
landscape4$locality_name<- as.factor(landscape4$locality_name)

#landscape4 <- landscape4 %>%
 # mutate(lat_deg = factor(paste0(sprintf("%02d",floor(abs(location_lat))), "°")))

str(landscape4)


hist(landscape4$richness)
hist(landscape4$beta_SIM)


# Correlations among landscape variables
landscape5 <- as.data.frame(landscape4) %>% type.convert(as.is = F)
str(landscape5)
landsvar500 <- landscape5 %>% select(5,54,38,46,50,14,17) %>% scale()
landsvar1 <- landscape5 %>% select(5,54,39,47,51,14,17) %>% scale()
landsvar2 <- landscape5 %>% select(5,54,40,48,52,14,17) %>% scale()
landsvar5 <- landscape5 %>% select(5,54,41,49,53,14,17) %>% scale() #scaled variables


# Previously scaled 
tiff(filename = "LandsCorScaledSites-Log-OCTOBER-2025-Edited-52Patches.tif", width = 17, height = 14, 
     units = "cm", pointsize = 7,
     bg = "white",  res = 400, restoreConsole = TRUE)

par(mfrow=c(2,2),mar=c(3,2,2,1),oma=c(2,2,2,1),bty=("l"))


cortraits500 = cor(landsvar500,method = c("spearman"),use = "pairwise.complete.obs")
c500 <- corrplot.mixed(cortraits500,sig.level = c(.05), insig = "label_sig", 
                       lower = "circle", upper = "n", tl.pos = "lt", addgrid.col = "grey")       

cortraits1 = cor(landsvar1,method = c("spearman"),use = "pairwise.complete.obs")
c1 <- corrplot.mixed(cortraits1,sig.level = c(.05), insig = "label_sig", 
                     lower = "circle", upper = "n", tl.pos = "lt", addgrid.col = "grey")    

cortraits2 = cor(landsvar2,method = c("spearman"),use = "pairwise.complete.obs")
c2 <- corrplot.mixed(cortraits2,sig.level = c(.05), insig = "label_sig", 
                     lower = "circle", upper = "n", tl.pos = "lt", addgrid.col = "grey")    

cortraits5 = cor(landsvar5,method = c("spearman"),use = "pairwise.complete.obs")
c5 <- corrplot.mixed(cortraits5,sig.level = c(.05), insig = "label_sig", 
                     lower = "circle", upper = "n", tl.pos = "lt", addgrid.col = "grey")    

dev.off() 




##### Buffers for *species richness* as response variable
# Forest cover buffers
m1_rich_cover_5km = glmmTMB(richness~forest_cover_5km,
                            family = Gamma(link = "log"),
                            data=landscape4, na.action = "na.fail")
                            summary(m1_rich_cover_5km)
                            
m2_rich_cover_2km = glmmTMB(richness~forest_cover_2km,
                            family = Gamma(link = "log"),
                            data=landscape4, na.action = "na.fail")
                            summary(m2_rich_cover_2km)
                            
m3_rich_cover_1km = glmmTMB(richness~forest_cover_1km,
                            family = Gamma(link = "log"),
                            data=landscape4, na.action = "na.fail")
                            summary(m3_rich_cover_1km)
                           
m4_rich_cover_500m = glmmTMB(richness~forest_cover_500m,
                            family = Gamma(link = "log"),
                            data=landscape4, na.action = "na.fail")
                            summary(m4_rich_cover_500m)
                            
AIC.m_rich_cover <- AICtab(m1_rich_cover_5km,
                     m2_rich_cover_2km,
                     m3_rich_cover_1km,
                     m4_rich_cover_500m,
                     delta=T,weights=T)
                            
AIC.m_rich_cover <- data.frame(AIC.m_rich_cover)
AIC.m_rich_cover
                           


# Number of patches buffers
m1_rich_npatch_5km = glmmTMB(richness~n_patch_5km,
                            family = Gamma(link = "log"),
                            data=landscape4, na.action = "na.fail")
                            summary(m1_rich_npatch_5km)
                            
m2_rich_npatch_2km = glmmTMB(richness~n_patch_2km,
                            family = Gamma(link = "log"),
                            data=landscape4, na.action = "na.fail")
                            summary(m2_rich_npatch_2km)
                            
m3_rich_npatch_1km = glmmTMB(richness~n_patch_1km,
                            family = Gamma(link = "log"),
                            data=landscape4, na.action = "na.fail")
                            summary(m3_rich_npatch_1km)

m4_rich_npatch_500m = glmmTMB(richness~n_patch_500m,
                            family = Gamma(link = "log"),
                            data=landscape4, na.action = "na.fail")
                            summary(m4_rich_npatch_500m)

AIC.m_rich_npatch <- AICtab(m1_rich_npatch_5km,
                      m2_rich_npatch_2km,
                      m3_rich_npatch_1km,
                      m4_rich_npatch_500m,
                      delta=T,weights=T)

AIC.m_rich_npatch <- data.frame(AIC.m_rich_npatch)
AIC.m_rich_npatch





# Model selection
# Full model
m1_rich_full = glmmTMB(richness~scale(patch_size_log)+
                                  scale(forest_cover_500m)+
                                  scale(n_patch_2km)+
                                  scale(prec.total)+
                                  location_lat+
                                  scale(patch_size_log)*scale(n_patch_2km) +
                                  scale(forest_cover_500m)*scale(n_patch_2km) +      
                                  scale(patch_size_log)*scale(prec.total) +
                                  scale(forest_cover_500m)*scale(prec.total) +
                                  scale(n_patch_2km)*scale(prec.total),
                                  #(1|locality_name), 
                                        family = Gamma(link = "log"),
                                        data=landscape4, na.action = "na.fail")
#control = glmmTMBControl(optimizer = "nlminb", optArgs = list(eval.max = 1e4, iter.max = 1e4)))
#control = glmmTMBControl(optimizer = optim, #nlminb
#optArgs = list(method = "BFGS")))  
# 'arg' can be “Nelder-Mead”, “BFGS”, “CG”, “L-BFGS-B”, “SANN”, “Brent”
summary(m1_rich_full)
simulationOutput <- simulateResiduals(fittedModel = m1_rich_full, plot = TRUE)
#step_model <- step(m1_rich_5km_a.full, direction = "both")
#summary(step_model)
vif_results <- check_collinearity(m1_rich_full)
print(vif_results)
plot(ggeffects::ggpredict(m1_rich_full, terms = c("location_lat")),show_data=T,jitter=T)
plot(ggeffects::ggpredict(m1_rich_full, terms = c("patch_size_log")),show_data=T)
plot(ggeffects::ggpredict(m1_rich_full, terms = c("forest_cover_500m")),show_data=T)
plot(ggeffects::ggpredict(m1_rich_full, terms = c("n_patch_2km")), show_data=T,jitter=T)
plot(ggeffects::ggpredict(m1_rich_full, terms = c("prec.total")),show_data=T)

#model.selection <- dredge(m1_rich_full, rank = "AICc")
#model.selection
#best.models <- get.models(model.selection, subset = delta < 2)
#best.models
#simulationOutput <- simulateResiduals(best.models[[1]], plot = TRUE)
#avg.model <- model.avg(best.models)
#summary(avg.model)


m1_rich_full2 = glmmTMB(richness~scale(patch_size_log)+
                                  scale(forest_cover_500m)+
                                  scale(n_patch_2km)+
                                  scale(prec.total)+
                                  location_lat,
                       family = Gamma(link = "log"),
                       data=landscape4, na.action = "na.fail")

# Latitude (control variable)
m2_rich = glmmTMB(richness ~ location_lat, family = Gamma(link = "log"),
                  data = landscape4, na.action = "na.fail")

# Each predictor
m3_rich = glmmTMB(richness ~ scale(patch_size_log) + location_lat, family = Gamma(link="log"), 
                  data=landscape4, na.action="na.fail")
m4_rich = glmmTMB(richness ~ scale(forest_cover_500m) + location_lat, family = Gamma(link="log"), 
                  data=landscape4, na.action="na.fail")
m5_rich = glmmTMB(richness ~ scale(n_patch_2km) + location_lat, family = Gamma(link="log"), 
                  data=landscape4, na.action="na.fail")
m6_rich = glmmTMB(richness ~ scale(prec.total) + location_lat, family = Gamma(link="log"), 
                  data=landscape4, na.action="na.fail")

# Plausible interactions
m7_rich = glmmTMB(richness ~ scale(patch_size_log)*scale(n_patch_2km) + location_lat, family = Gamma(link="log"), 
                   data=landscape4, na.action="na.fail")
m8_rich = glmmTMB(richness ~ scale(forest_cover_500m)*scale(n_patch_2km) + location_lat, family = Gamma(link="log"), 
                   data=landscape4, na.action="na.fail")
m9_rich = glmmTMB(richness ~ scale(patch_size_log)*scale(prec.total) + location_lat, family = Gamma(link="log"), 
                   data=landscape4, na.action="na.fail")
m10_rich = glmmTMB(richness ~ scale(forest_cover_500m)*scale(prec.total) + location_lat, family = Gamma(link="log"), 
                   data=landscape4, na.action="na.fail")
m11_rich = glmmTMB(richness ~ scale(n_patch_2km)*scale(prec.total) + location_lat, family = Gamma(link="log"), 
                   data=landscape4, na.action="na.fail") 

# Additive models
#m12_rich = glmmTMB(richness ~ scale(patch_size_log)+scale(n_patch_2km) + location_lat, family = Gamma(link="log"), 
 #                  data=landscape4, na.action="na.fail")
#m13_rich = glmmTMB(richness ~ scale(forest_cover_500m)+scale(n_patch_2km) + location_lat, family = Gamma(link="log"), 
 #                  data=landscape4, na.action="na.fail")
#m14_rich = glmmTMB(richness ~ scale(patch_size_log)+scale(prec.total) + location_lat, family = Gamma(link="log"), 
 #                  data=landscape4, na.action="na.fail")
#m15_rich = glmmTMB(richness ~ scale(forest_cover_500m)+scale(prec.total) + location_lat, family = Gamma(link="log"), 
 #                  data=landscape4, na.action="na.fail") 
#m16_rich = glmmTMB(richness ~ scale(n_patch_2km)+scale(prec.total) + location_lat, family = Gamma(link="log"), 
 #                  data=landscape4, na.action="na.fail") 

# Null model
m_rich_null = glmmTMB(richness ~ 1,
                  family = Gamma(link = "log"), data = landscape4,
                  na.action = "na.fail")

# AIC
AIC.m_rich <- AICtab(#m1_rich_full, 
  #m1_rich_full2, 
  m2_rich, m3_rich, m4_rich, m5_rich, m6_rich, m7_rich,
  m8_rich, m9_rich, m10_rich, m11_rich, #m12_rich,
  #m13_rich, m14_rich, m15_rich, m16_rich,
  m_rich_null, delta = TRUE, weights = TRUE)

AIC.m_rich <- data.frame(AIC.m_rich)
AIC.m_rich
AIC.m_rich_df <- data.frame(model = rownames(AIC.m_rich),
                                 AIC.m_rich,row.names = NULL)

write.xlsx(AIC.m_rich_df, file = "AIC_m_rich.xlsx", rowNames = FALSE)


# Best models:

#summary(m1_rich_full2)
#simulationOutput <- simulateResiduals(fittedModel = m1_rich_full2, plot = TRUE)
#r2(m1_rich_full2)
#vif_results <- check_collinearity(m1_rich_full2)
#print(vif_results)
#plot(ggeffects::ggpredict(m1_rich_full2, terms = c("location_lat")),show_data=T,jitter=T)
#plot(ggeffects::ggpredict(m1_rich_full2, terms = c("forest_cover_500m")),show_data=T)
#plot(ggeffects::ggpredict(m1_rich_full2, terms = c("prec.total")),show_data=T)

#modelOutput.rich_m1full2<-capture.output(summary(m1_rich_full2)) #writing the best model outputs
#write.csv(modelOutput.rich_m1full2,"modelOutput-rich-m1-full2.csv",row.names=F,quote=F)


summary(m10_rich)
simulationOutput <- simulateResiduals(fittedModel = m10_rich, plot = TRUE)
r2(m10_rich)
vif_results <- check_collinearity(m10_rich)
print(vif_results)
plot(ggeffects::ggpredict(m10_rich, terms = c("location_lat")),show_data=T,jitter=T)
plot(ggeffects::ggpredict(m10_rich, terms = c("forest_cover_500m")),show_data=T)
plot(ggeffects::ggpredict(m10_rich, terms = c("prec.total")),show_data=T)

modelOutput.rich_m10<-capture.output(summary(m10_rich)) 
write.csv(modelOutput.rich_m10,"modelOutput-rich-m10.csv",row.names=F,quote=F)


# Moran's I correlogram of model residuals
residuals_m10_rich <- residuals(m10_rich, type = "pearson")
landscape4$residuals_m10_rich <- residuals_m10_rich

cor_moran_m10_rich <- correlog(
  x = landscape4$location_long,
  y = landscape4$location_lat,
  z = landscape4$residuals_m10_rich,
  increment = 1,   
  resamp = 999)     

plot(cor_moran_m10_rich, ylim = c(-1, 1))
abline(h = 0, lty = 2, col = "red") 


# Plotting the figure
jpeg("moran_plots_rich.jpg", width = 1200, height = 900, res = 150)
par(mfrow = c(1, 1))  

plot(cor_moran_m10_rich, ylim = c(-1, 1), main = "Correlogram - rich ~ forest_cover * prec.total")
abline(h = 0, lty = 2, col = "red")

dev.off()


#coords <- cbind(landscape4$location_long, landscape4$location_lat)
#dists <- spdep::nbdists(dnearneigh(coords, 0, Inf), coords)
#neighbor distances
#max_neigh <- max(sapply(dists, min))
#max_neigh
#nb <- dnearneigh(coords, d1 = 0, d2 = max_neigh)
#lw <- nb2listw(nb, style = "W")
#test <- moran.test(residuals, lw)
#str(test)



# Model predictions 
pred.test1<-ggpredict(m10_rich, terms=c("forest_cover_500m")) #model predictions
pred.test2<-ggpredict(m10_rich, terms=c("prec.total"))
pred.test3<-ggpredict(m10_rich, terms=c("forest_cover_500m","prec.total"))
pred.test4<-ggpredict(m10_rich, terms=c("location_lat"))


{a.predict1<-plot(pred.test1,show_data=T,show_ci=T,colors="blue",dot_size=1.5)+  
  labs(title="",x="forest cover (%)",y="species richness", col="")+
    #geom_point(size = 2) 
  ylim(0,150)+
  theme_classic()+
  theme(legend.position = "none")}

{b.predict2<-plot(pred.test2,show_data=T,show_ci=T,colors="blue",dot_size=1.5)+  
  labs(title="",x="precipitation (mm)",y="species richness", col="")+
  ylim(0,150)+
  theme_classic()+
  theme(legend.position = "none")}

{c.predict3<-plot(pred.test3,show_data=T,show_ci=T,dot_size=1.5)+  
  labs(title="",x="forest cover (%)",y="species richness", col="precipitation (mm)")+
  ylim(0,150)+
  theme_classic()+
  theme(legend.position = c(0.3,0.8),legend.text = element_text(size = 8))}

{d.predict4<-plot(pred.test4,show_data=T,show_ci=T,colors="blue",dot_size=1.5)+  
  labs(title="",x="latitude",y="species richness", col="")+
  ylim(0,150)+
  scale_x_reverse()+
  theme_classic()+
  theme(legend.position = "none")}


jpeg("ModelPredictions-Richness-Oct2025.jpeg", width = 17, height = 14,
     units = "cm", pointsize = 5,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(a.predict1, b.predict2, 
             c.predict3, d.predict4, 
             nrow = 2, ncol = 2)
dev.off() 





##### Buffers for *species turnover (beta_SIM)* as response variable
# Forest cover buffers
m1_beta_cover_5km = glmmTMB(beta_SIM~forest_cover_5km,
                            family = ordbeta(link = "logit"),
                            data=landscape4, na.action = "na.fail")
                            summary(m1_beta_cover_5km)

m2_beta_cover_2km = glmmTMB(beta_SIM~forest_cover_2km,
                            family = ordbeta(link = "logit"),
                            data=landscape4, na.action = "na.fail")
                            summary(m2_beta_cover_2km)

m3_beta_cover_1km = glmmTMB(beta_SIM~forest_cover_1km,
                            family = ordbeta(link = "logit"),
                            data=landscape4, na.action = "na.fail")
                            summary(m3_beta_cover_1km)

m4_beta_cover_500m = glmmTMB(beta_SIM~forest_cover_500m,
                            family = ordbeta(link = "logit"),
                            data=landscape4, na.action = "na.fail")
                            summary(m4_beta_cover_500m)

AIC.m_beta_cover <- AICtab(m1_beta_cover_5km,
                           m2_beta_cover_2km,
                           m3_beta_cover_1km,
                           m4_beta_cover_500m,
                           delta=T,weights=T)

AIC.m_beta_cover <- data.frame(AIC.m_beta_cover)
AIC.m_beta_cover



# Number of patches buffers
m1_beta_npatch_5km = glmmTMB(beta_SIM~n_patch_5km,
                            family = ordbeta(link = "logit"),
                            data=landscape4, na.action = "na.fail")
                            summary(m1_beta_npatch_5km)

m2_beta_npatch_2km = glmmTMB(beta_SIM~n_patch_2km,
                            family = ordbeta(link = "logit"),
                            data=landscape4, na.action = "na.fail")
                            summary(m2_beta_npatch_2km)

m3_beta_npatch_1km = glmmTMB(beta_SIM~n_patch_1km,
                             family = ordbeta(link = "logit"),
                             data=landscape4, na.action = "na.fail")
                             summary(m3_beta_npatch_1km)

m4_beta_npatch_500m = glmmTMB(beta_SIM~n_patch_500m,
                              family = ordbeta(link = "logit"),
                              data=landscape4, na.action = "na.fail")
                              summary(m4_beta_npatch_500m)

AIC.m_beta_npatch <- AICtab(m1_beta_npatch_5km,
                            m2_beta_npatch_2km,
                            m3_beta_npatch_1km,
                            m4_beta_npatch_500m,
                            delta=T,weights=T)

AIC.m_beta_npatch <- data.frame(AIC.m_beta_npatch)
AIC.m_beta_npatch





# Model selection
# Full model
m1_beta_full = glmmTMB(beta_SIM~scale(patch_size_log)+
                         scale(forest_cover_1km)+
                         scale(n_patch_1km)+
                         scale(prec.total)+
                         location_lat+
                         scale(patch_size_log)*scale(n_patch_1km) +
                         scale(forest_cover_1km)*scale(n_patch_1km) +      
                         scale(patch_size_log)*scale(prec.total) +
                         scale(forest_cover_1km)*scale(prec.total) +
                         scale(n_patch_1km)*scale(prec.total),
                         #(1|locality_name), 
                       family = ordbeta(link = "logit"),
                       data=landscape4, na.action = "na.fail")
#control = glmmTMBControl(optimizer = "nlminb", optArgs = list(eval.max = 1e4, iter.max = 1e4)))
#control = glmmTMBControl(optimizer = optim, #nlminb
#optArgs = list(method = "BFGS")))  
# 'arg' can be “Nelder-Mead”, “BFGS”, “CG”, “L-BFGS-B”, “SANN”, “Brent”
summary(m1_beta_full)
simulationOutput <- simulateResiduals(fittedModel = m1_beta_full, plot = TRUE)
#step_model <- step(m1_beta_5km_a.full, direction = "both")
#summary(step_model)
vif_results <- check_collinearity(m1_beta_full)
print(vif_results)
plot(ggeffects::ggpredict(m1_beta_full, terms = c("location_lat")),show_data=T,jitter=T)
plot(ggeffects::ggpredict(m1_beta_full, terms = c("patch_size_log")),show_data=T)
plot(ggeffects::ggpredict(m1_beta_full, terms = c("forest_cover_1km")),show_data=T)
plot(ggeffects::ggpredict(m1_beta_full, terms = c("forest_cover_1km","prec.total")), show_data=T,jitter=T)
plot(ggeffects::ggpredict(m1_beta_full, terms = c("prec.total")),show_data=T)

#model.selection <- dredge(m1_beta_full, rank = "AICc")
#model.selection
#best.models <- get.models(model.selection, subset = delta < 2)
#best.models
#simulationOutput <- simulateResiduals(best.models[[1]], plot = TRUE)
#avg.model <- model.avg(best.models)
#summary(avg.model)


m1_beta_full2 = glmmTMB(beta_SIM~scale(patch_size_log)+
                          scale(forest_cover_1km)+
                          scale(n_patch_1km)+
                          scale(prec.total)+
                          location_lat,
                        family = ordbeta(link = "logit"),
                        data=landscape4, na.action = "na.fail")

# Latitude (control variable)
m2_beta = glmmTMB(beta_SIM ~ location_lat, family = ordbeta(link = "logit"),
                  data = landscape4, na.action = "na.fail")

# Each predictor
m3_beta = glmmTMB(beta_SIM ~ scale(patch_size_log) + location_lat, family = ordbeta(link="logit"), 
                  data=landscape4, na.action="na.fail")
m4_beta = glmmTMB(beta_SIM ~ scale(forest_cover_1km) + location_lat, family = ordbeta(link="logit"), 
                  data=landscape4, na.action="na.fail")
m5_beta = glmmTMB(beta_SIM ~ scale(n_patch_1km) + location_lat, family = ordbeta(link="logit"), 
                  data=landscape4, na.action="na.fail")
m6_beta = glmmTMB(beta_SIM ~ scale(prec.total) + location_lat, family = ordbeta(link="logit"), 
                  data=landscape4, na.action="na.fail")

# Plausible interactions
m7_beta = glmmTMB(beta_SIM ~ scale(patch_size_log)*scale(n_patch_1km) + location_lat, family = ordbeta(link="logit"), 
                  data=landscape4, na.action="na.fail")
m8_beta = glmmTMB(beta_SIM ~ scale(forest_cover_1km)*scale(n_patch_1km) + location_lat, family = ordbeta(link="logit"), 
                  data=landscape4, na.action="na.fail")
m9_beta = glmmTMB(beta_SIM ~ scale(patch_size_log)*scale(prec.total) + location_lat, family = ordbeta(link="logit"), 
                  data=landscape4, na.action="na.fail")
m10_beta = glmmTMB(beta_SIM ~ scale(forest_cover_1km)*scale(prec.total) + location_lat, family = ordbeta(link="logit"), 
                   data=landscape4, na.action="na.fail")
m11_beta = glmmTMB(beta_SIM ~ scale(n_patch_1km)*scale(prec.total) + location_lat, family = ordbeta(link="logit"), 
                   data=landscape4, na.action="na.fail") 

# Additive models
#m12_beta = glmmTMB(beta_SIM ~ scale(patch_size_log)+scale(n_patch_1km) + location_lat, family = ordbeta(link="logit"), 
 #                  data=landscape4, na.action="na.fail")
#m13_beta = glmmTMB(beta_SIM ~ scale(forest_cover_1km)+scale(n_patch_1km) + location_lat, family = ordbeta(link="logit"), 
 #                  data=landscape4, na.action="na.fail")
#m14_beta = glmmTMB(beta_SIM ~ scale(patch_size_log)+scale(prec.total) + location_lat, family = ordbeta(link="logit"), 
 #                  data=landscape4, na.action="na.fail")
#m15_beta = glmmTMB(beta_SIM ~ scale(forest_cover_1km)+scale(prec.total) + location_lat, family = ordbeta(link="logit"), 
 #                  data=landscape4, na.action="na.fail") 
#m16_beta = glmmTMB(beta_SIM ~ scale(n_patch_1km)+scale(prec.total) + location_lat, family = ordbeta(link="logit"), 
 #                  data=landscape4, na.action="na.fail") 

# Null model
m_beta_null = glmmTMB(beta_SIM ~ 1,
                      family = ordbeta(link = "logit"), data = landscape4,
                      na.action = "na.fail")

# AIC
AIC.m_beta <- AICtab(#m1_beta_full, 
  #m1_beta_full2, 
  m2_beta, m3_beta, m4_beta, m5_beta, m6_beta, m7_beta,
  m8_beta, m9_beta, m10_beta, m11_beta, #m12_beta,
  #m13_beta, m14_beta, m15_beta, m16_beta,
  m_beta_null, delta = TRUE, weights = TRUE)

AIC.m_beta <- data.frame(AIC.m_beta)
AIC.m_beta
AIC.m_beta_df <- data.frame(model = rownames(AIC.m_beta),
                            AIC.m_beta,row.names = NULL)

write.xlsx(AIC.m_beta_df, file = "AIC_m_beta.xlsx", rowNames = FALSE)


# Best models:

summary(m10_beta)
simulationOutput <- simulateResiduals(fittedModel = m10_beta, plot = TRUE)
r2(m10_beta)
vif_results <- check_collinearity(m10_beta)
print(vif_results)
plot(ggeffects::ggpredict(m10_beta, terms = c("location_lat")),show_data=T,jitter=T)
plot(ggeffects::ggpredict(m10_beta, terms = c("forest_cover_1km","prec.total")),show_data=T)
plot(ggeffects::ggpredict(m10_beta, terms = c("forest_cover_1km")),show_data=T)
plot(ggeffects::ggpredict(m10_beta, terms = c("prec.total")),show_data=T,jitter=T)

modelOutput.beta_m10<-capture.output(summary(m10_beta)) #writing the best model outputs
write.csv(modelOutput.beta_m10,"modelOutput-beta-m10.csv",row.names=F,quote=F)



# Moran's I correlogram of model residuals
beta_residuals <- residuals(m10_beta, type = "pearson")
landscape4$beta_residuals <- beta_residuals

cor_moran_beta <- correlog(
  x = landscape4$location_long,
  y = landscape4$location_lat,
  z = landscape4$beta_residuals,
  increment = 1,   
  resamp = 999)     

plot(cor_moran_beta, ylim = c(-1, 1))
abline(h = 0, lty = 2, col = "red") 


# Plotting the figure
jpeg("moran_plots_beta.jpg", width = 1200, height = 900, res = 150)
par(mfrow = c(1, 1))  

plot(cor_moran_beta, ylim = c(-1, 1), main = "Correlogram - turn ~ forest_cover * prec.total")
abline(h = 0, lty = 2, col = "red")

dev.off()



# Model predictions 
pred.test5<-ggpredict(m10_beta, terms=c("forest_cover_1km")) #model predictions
pred.test6<-ggpredict(m10_beta, terms=c("prec.total"))
pred.test7<-ggpredict(m10_beta, terms=c("forest_cover_1km","prec.total"))
pred.test8<-ggpredict(m10_beta, terms=c("location_lat"))


{e.predict5<-plot(pred.test5,show_data=T,show_ci=T,colors="blue",dot_size=1.5)+  
    labs(title="",x="forest cover (%)",y="spatial turnover", col="")+
    #geom_point(size = 2) 
    ylim(0.4,1)+
    theme_classic()+
    theme(legend.position = "none")}

{f.predict6<-plot(pred.test6,show_data=T,show_ci=T,colors="blue",dot_size=1.5)+  
    labs(title="",x="precipitation (mm)",y="spatial turnover", col="")+
    ylim(0.4,1)+
    theme_classic()+
    theme(legend.position = "none")}

{g.predict7<-plot(pred.test7,show_data=T,show_ci=T,dot_size=1.5)+  
    labs(title="",x="forest cover (%)",y="spatial turnover", col="precipitation (mm)")+
    ylim(0.4,1)+
    theme_classic()+
    theme(legend.position = c(0.8,0.25),legend.text = element_text(size = 8))}

{h.predict8<-plot(pred.test8,show_data=T,show_ci=T,colors="blue",ci_style="dash",dot_size=1.5)+  
    labs(title="",x="latitude",y="spatial turnover", col="")+
    ylim(0.4,1)+
    scale_x_reverse()+
    theme_classic()+
    theme(legend.position = "none")}


jpeg("ModelPredictions-TurnBeta-Oct2025.jpeg", width = 17, height = 14,
     units = "cm", pointsize = 5,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(e.predict5, f.predict6, 
             g.predict7, h.predict8, 
             nrow = 2, ncol = 2)
dev.off() 


