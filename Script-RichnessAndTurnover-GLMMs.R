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

landscape4 <- read_excel("LandscapeFinal-Nov6-2024-Fulbert.xlsx")
str(landscape4)
landscape4$patch_log<-log(landscape4$patch_ha)
landscape4$locality_name<- as.factor(landscape4$locality_name)
#landscape4$richness<-as.integer(landscape4$richness)
#landscape4$rich_log<-log(landscape4$richness)
#landscape4$rich_sqrt<-sqrt(landscape4$richness)
#landscape4$forest_sqrt<-sqrt(landscape4$forest_cover_5km)
#landscape4$forest_asin<-asin(landscape4$forest_sqrt)
#landscape4$edge_log<-log(landscape4$edge_5km)

hist(landscape4$patch_ha)
hist(landscape4$patch_log)
hist(landscape4$forest_cover_5km)
hist(landscape4$forest_sqrt)
hist(landscape4$edge_5km)
hist(landscape4$edge_index)
hist(landscape4$heterogeneity_5km)
hist(landscape4$tavg)
hist(landscape4$prec.total)

hist(landscape4$richness)
hist(landscape4$rich_log)
hist(landscape4$rich_sqrt)
hist(landscape4$beta_SIM)


# Correlations among landscape variables
landscape5 <- as.data.frame(landscape4) %>% type.convert(as.is = F)
str(landscape5)
landsvar500 <- landscape5 %>% select(5,6,44,12,20,31,34) %>% scale()
landsvar1 <- landscape5 %>% select(5,6,44,13,21,31,34) %>% scale()
landsvar2 <- landscape5 %>% select(5,6,44,14,22,31,34) %>% scale()
landsvar5 <- landscape5 %>% select(5,6,44,15,23,31,34) %>% scale() #scaled variables
clim <- landscape5 %>% select(31:35) %>% scale()

# Previously scaled 
tiff(filename = "LandsCorScaledSites-Log-May30-2024.tif", width = 17, height = 14, 
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

jpeg("Correlations-5km.jpeg", width = 8, height = 7,units = "cm", pointsize = 6, #for 5km only
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
cortraits5 = cor(landsvar5,method = c("spearman"),use = "pairwise.complete.obs")
c5 <- corrplot.mixed(cortraits5,sig.level = c(.05), insig = "label_sig", 
                    lower = "circle", upper = "n", tl.pos = "lt", addgrid.col = "grey")    
dev.off() 


cortraits.clim = cor(clim,method = c("spearman"),use = "pairwise.complete.obs")
c.clim <- corrplot.mixed(cortraits.clim,sig.level = c(.05), insig = "label_sig", 
                     lower = "circle", upper = "n", tl.pos = "lt", addgrid.col = "grey")    



##### For *species richness*
# Forest cover buffers
buffers1 <- c("forest_cover_500m", "forest_cover_1km", "forest_cover_2km", "forest_cover_5km")
landscape4_scaled <- landscape4
for (buffer in buffers1) {landscape4_scaled[[buffer]] <- scale(landscape4[[buffer]])}

results <- list()

for (buffer in buffers1) {model1 <- glmmTMB(richness ~ get(buffer) + (1|locality_name), 
                        data = landscape4_scaled, 
                        family = nbinom2(link = "log"))
                        results[[buffer]] <- list(model = model1, aic = AIC(model1))}

aic_values1 <- data.frame(buffer = buffers1, aic = sapply(results, function(x) x$aic))
aic_values1$deltaAIC <- aic_values1$aic - min(aic_values1$aic)
slopes1 <- sapply(results, function(x) summary(x$model)$coefficients$cond[2, 1])
aic_values1$slope <- slopes1


# Rename and reorder buffers in the data frame 
aic_values1$buffer <- factor(aic_values1$buffer, 
                                     levels = c("forest_cover_500m", "forest_cover_1km", 
                                                "forest_cover_2km", "forest_cover_5km"),
                                     labels = c("500m", "1km", "2km", "5km"))

a<-ggplot(aic_values1, aes(x = buffer, y = deltaAIC)) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue4") +
  theme_minimal() +
  labs(title = "richness ~ forest cover",
       x = "buffer size",
       y = "delta AIC") +
  ylim(0,5)
  

b<-ggplot(aic_values1, aes(x = buffer, y = slope, group = 1)) +
  geom_point(color = "blue4") +
  geom_line(color = "blue4") +
  theme_minimal() +
  labs(title = "Slopes for forest cover buffers",
       x = "Buffer",
       y = "Slope") +
  ylim(-0.2,0.2)



# Total edge buffers
buffers2 <- c("edge_500m", "edge_1km", "edge_2km", "edge_5km")
landscape4_scaled <- landscape4
for (buffer in buffers2) {landscape4_scaled[[buffer]] <- scale(landscape4[[buffer]])}

results <- list()

for (buffer in buffers2) {model2 <- glmmTMB(richness ~ get(buffer) + (1|locality_name), 
                        data = landscape4_scaled, 
                        family = nbinom2(link = "log"))
                        results[[buffer]] <- list(model = model2, aic = AIC(model2))}

aic_values2 <- data.frame(buffer = buffers2, aic = sapply(results, function(x) x$aic))
aic_values2$deltaAIC <- aic_values2$aic - min(aic_values2$aic)
slopes2 <- sapply(results, function(x) summary(x$model)$coefficients$cond[2, 1])
aic_values2$slope <- slopes2


# Rename and reorder buffers in the data frame
aic_values2$buffer <- factor(aic_values2$buffer, 
                                     levels = c("edge_500m", "edge_1km", 
                                                "edge_2km", "edge_5km"),
                                     labels = c("500m", "1km", "2km", "5km"))

c<-ggplot(aic_values2, aes(x = buffer, y = deltaAIC)) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue4") +
  theme_minimal() +
  labs(title = "richness ~ total edge",
       x = "buffer size",
       y = "delta AIC") +
  ylim(0,5)


d<-ggplot(aic_values2, aes(x = buffer, y = slope, group = 1)) +
  geom_point(color = "blue4") +
  geom_line(color = "blue4") +
  theme_minimal() +
  labs(title = "Slopes for total edge buffers",
       x = "Buffer",
       y = "Slope") +
  ylim(-0.3,0.3)

# Figure for species richness buffers
jpeg("Buffers-RichOnly-New.jpeg", width = 17, height = 8,
     units = "cm", pointsize = 9,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(a, c,
             nrow = 1, ncol = 2)
dev.off() 


###### Model selection 
# glmm models for species richness as the response variable

# For the whole community
m_rich1a.full = glmmTMB(richness~(scale(patch_log)+scale(forest_cover_5km)+
            scale(edge_5km)+ # full model
            scale(prec.total)+scale(tavg))^2 + 
            (1|locality_name), family = nbinom2(link = "log"),
            data=landscape4, na.action = "na.fail") #, control = glmmTMBControl(optimizer = optim,
                                                 #optArgs = list(method = "BFGS"))) 
summary(m_rich1a.full)
simulationOutput <- simulateResiduals(fittedModel = m_rich1a.full, plot = TRUE)
vif_results <- check_collinearity(m_rich1a.full)
print(vif_results)
plot(ggeffects::ggpredict(m_rich1a.full, terms = c("patch_log")),rawdata=T)
plot(ggeffects::ggpredict(m_rich1a.full, terms = c("forest_cover_5km")),rawdata=T)
plot(ggeffects::ggpredict(m_rich1a.full, terms = c("patch_log","forest_cover_5km")),rawdata=T)
plot(ggeffects::ggpredict(m_rich1a.full, terms = c("patch_log","edge_5km")),rawdata=T)
plot(ggeffects::ggpredict(m_rich1a.full, terms = c("edge_5km")),rawdata=T)
dredgerich1<-MuMIn::dredge(global.model = m_rich1a, rank = "AIC")
print(dredgerich1)
write.xlsx(dredgerich1,"dredge-FULLMODEL-rich1-nbinom2.xlsx",
            rowNames=F,quote=F)


m_rich1b = glmmTMB(richness~scale(patch_log)*scale(forest_cover_5km)+
              (1|locality_name), family = nbinom2(link = "log"),
              data=landscape4, na.action = "na.fail") #, control = glmmTMBControl(optimizer = optim,
                                                 #optArgs = list(method = "BFGS")))
          summary(m_rich1b)
          simulationOutput <- simulateResiduals(fittedModel = m_rich1b, plot = TRUE)
          plot(ggeffects::ggpredict(m_rich1b, terms = c("patch_log","forest_cover_5km")),rawdata=T)
          vif_results <- check_collinearity(m_rich1b)
          print(vif_results)

m_rich1c = glmmTMB(richness~scale(patch_log)*scale(edge_5km)+
              (1|locality_name), family = nbinom2(link = "log"),
              data=landscape4, na.action = "na.fail") #, control = glmmTMBControl(optimizer = optim,
                                                 #optArgs = list(method = "BFGS")))
          summary(m_rich1c)
          simulationOutput <- simulateResiduals(fittedModel = m_rich1c, plot = TRUE)
          plot(ggeffects::ggpredict(m_rich1c, terms = c("patch_log","edge_5km")),rawdata=T)
          vif_results <- check_collinearity(m_rich1c)
          print(vif_results)

m_rich1d = glmmTMB(richness~scale(patch_log)*scale(prec.total)+
              (1|locality_name), family = nbinom2(link = "log"),
              data=landscape4, na.action = "na.fail") #, control = glmmTMBControl(optimizer = optim,
                                                 #optArgs = list(method = "BFGS")))
          summary(m_rich1d)
          simulationOutput <- simulateResiduals(fittedModel = m_rich1d, plot = TRUE)
          plot(ggeffects::ggpredict(m_rich1d, terms = c("patch_log","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_rich1d)
          print(vif_results)
          
m_rich1e = glmmTMB(richness~scale(patch_log)*scale(tavg)+
              (1|locality_name), family = nbinom2(link = "log"),
              data=landscape4, na.action = "na.fail") #, control = glmmTMBControl(optimizer = optim,
                                                 #optArgs = list(method = "BFGS")))
          summary(m_rich1e)
          simulationOutput <- simulateResiduals(fittedModel = m_rich1e, plot = TRUE)
          plot(ggeffects::ggpredict(m_rich1e, terms = c("patch_log","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_rich1e)
          print(vif_results)
          
m_rich1f = glmmTMB(richness~scale(forest_cover_5km)*scale(edge_5km)+
              (1|locality_name), family = nbinom2(link = "log"),
              data=landscape4, na.action = "na.fail") #, control = glmmTMBControl(optimizer = optim,
                                                 #optArgs = list(method = "BFGS")))
          summary(m_rich1f)
          simulationOutput <- simulateResiduals(fittedModel = m_rich1f, plot = TRUE)
          plot(ggeffects::ggpredict(m_rich1f, terms = c("forest_cover_5km","edge_5km")),rawdata=T)
          vif_results <- check_collinearity(m_rich1f)
          print(vif_results)
                    
m_rich1g = glmmTMB(richness~scale(forest_cover_5km)*scale(prec.total)+
              (1|locality_name), family = nbinom2(link = "log"),
              data=landscape4, na.action = "na.fail") #, control = glmmTMBControl(optimizer = optim,
                                                 #optArgs = list(method = "BFGS")))
          summary(m_rich1g)
          simulationOutput <- simulateResiduals(fittedModel = m_rich1g, plot = TRUE)
          plot(ggeffects::ggpredict(m_rich1g, terms = c("forest_cover_5km","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_rich1g)
          print(vif_results)
          
m_rich1h = glmmTMB(richness~scale(forest_cover_5km)*scale(tavg)+
              (1|locality_name), family = nbinom2(link = "log"),
              data=landscape4, na.action = "na.fail") #, control = glmmTMBControl(optimizer = optim,
                                                 #optArgs = list(method = "BFGS")))
          summary(m_rich1h)
          simulationOutput <- simulateResiduals(fittedModel = m_rich1h, plot = TRUE)
          plot(ggeffects::ggpredict(m_rich1h, terms = c("forest_cover_5km","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_rich1h)
          print(vif_results)
          
m_rich1i = glmmTMB(richness~scale(edge_5km)*scale(prec.total)+
              (1|locality_name), family = nbinom2(link = "log"),
              data=landscape4, na.action = "na.fail") #, control = glmmTMBControl(optimizer = optim,
                                                 #optArgs = list(method = "BFGS")))
          summary(m_rich1i)
          simulationOutput <- simulateResiduals(fittedModel = m_rich1i, plot = TRUE)
          plot(ggeffects::ggpredict(m_rich1i, terms = c("edge_5km","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_rich1i)
          print(vif_results)
          
m_rich1j = glmmTMB(richness~scale(edge_5km)*scale(tavg)+
              (1|locality_name), family = nbinom2(link = "log"),
              data=landscape4, na.action = "na.fail") #, control = glmmTMBControl(optimizer = optim,
                                                 #optArgs = list(method = "BFGS")))
          summary(m_rich1j)
          simulationOutput <- simulateResiduals(fittedModel = m_rich1j, plot = TRUE)
          plot(ggeffects::ggpredict(m_rich1j, terms = c("edge_5km","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_rich1j)
          print(vif_results)
          
m_rich1k = glmmTMB(richness~scale(prec.total)*scale(tavg)+
              (1|locality_name), family = nbinom2(link = "log"),
              data=landscape4, na.action = "na.fail") #, control = glmmTMBControl(optimizer = optim,
                                                 #optArgs = list(method = "BFGS")))
          summary(m_rich1i)
          simulationOutput <- simulateResiduals(fittedModel = m_rich1i, plot = TRUE)
          plot(ggeffects::ggpredict(m_rich1k, terms = c("prec.total","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_rich1k)
          print(vif_results)
          
m_rich1l = glmmTMB(richness~scale(patch_log)+ 
              (1|locality_name), family = nbinom2(link = "log"),
              data=landscape4, na.action = "na.fail") #, control = glmmTMBControl(optimizer = optim,
                                                 #optArgs = list(method = "BFGS")))
          summary(m_rich1l)
          simulationOutput <- simulateResiduals(fittedModel = m_rich1l, plot = TRUE)
          plot(ggeffects::ggpredict(m_rich1l, terms = c("patch_log")),rawdata=T)
          
m_rich1m = glmmTMB(richness~scale(forest_cover_5km)+ 
              (1|locality_name), family = nbinom2(link = "log"),
              data=landscape4, na.action = "na.fail") #, control = glmmTMBControl(optimizer = optim,
                                                 #optArgs = list(method = "BFGS")))
          summary(m_rich1m)
          simulationOutput <- simulateResiduals(fittedModel = m_rich1m, plot = TRUE)
          plot(ggeffects::ggpredict(m_rich1m, terms = c("forest_cover_5km")),rawdata=T)

m_rich1n = glmmTMB(richness~scale(edge_5km)+ 
              (1|locality_name), family = nbinom2(link = "log"),
              data=landscape4, na.action = "na.fail") #, control = glmmTMBControl(optimizer = optim,
                                                 #optArgs = list(method = "BFGS")))
          summary(m_rich1n)
          simulationOutput <- simulateResiduals(fittedModel = m_rich1n, plot = TRUE)
          plot(ggeffects::ggpredict(m_rich1n, terms = c("edge_5km")),rawdata=T)

m_rich1o = glmmTMB(richness~scale(prec.total)+ 
              (1|locality_name), family = nbinom2(link = "log"),
              data=landscape4, na.action = "na.fail") #, control = glmmTMBControl(optimizer = optim,
                                                 #optArgs = list(method = "BFGS")))
          summary(m_rich1o)
          simulationOutput <- simulateResiduals(fittedModel = m_rich1o, plot = TRUE)
          plot(ggeffects::ggpredict(m_rich1o, terms = c("prec.total")),rawdata=T)

m_rich1p = glmmTMB(richness~scale(tavg)+ 
              (1|locality_name), family = nbinom2(link = "log"),
              data=landscape4, na.action = "na.fail") #, control = glmmTMBControl(optimizer = optim,
                                                 #optArgs = list(method = "BFGS")))
          summary(m_rich1p)
          simulationOutput <- simulateResiduals(fittedModel = m_rich1p, plot = TRUE)
          plot(ggeffects::ggpredict(m_rich1p, terms = c("tavg")),rawdata=T)

m_null1 = glmmTMB(richness~1+(1|locality_name),
                  family = nbinom2(link = "log"),
                  data = landscape4, na.action = "na.fail") #, control = glmmTMBControl(optimizer = optim,
                                                 #optArgs = list(method = "BFGS")))
          summary(m_null1)
          
          
AIC.m_rich1 <- AICtab(m_rich1a.full,m_rich1b,m_rich1c,m_rich1d,
                      m_rich1e,m_rich1f,m_rich1g,m_rich1h,
                      m_rich1i,m_rich1j,m_rich1k,
                      m_rich1l,m_rich1m,m_rich1n,m_rich1o,m_rich1p,
                      m_null1, delta=T,weights=T)
AIC.m_rich1 <- data.frame(AIC.m_rich1)
write.xlsx(AIC.m_rich1,"AIC-richness-nb2.xlsx",
            rowNames=T,quote=F)


model1c.rich<-capture.output(summary(m_rich1c)) #writing the best model outputs
          write.csv(model1c.rich,"output-model1c-rich-new.csv",
          row.names=F,quote=F)
model1n.rich<-capture.output(summary(m_rich1n)) 
          write.csv(model1n.rich,"output-model1n-rich-new.csv",
          row.names=F,quote=F)
model1m.rich<-capture.output(summary(m_rich1m)) 
          write.csv(model1m.rich,"output-model1m-rich-new.csv",
          row.names=F,quote=F)
model1i.rich<-capture.output(summary(m_rich1i)) 
          write.csv(model1i.rich,"output-model1i-rich-new.csv",
          row.names=F,quote=F)


pred.test1<-ggpredict(m_rich1c, terms=c("patch_log","edge_5km")) #model predictions
pred.test2<-ggpredict(m_rich1n, terms=c("edge_5km"))
pred.test3<-ggpredict(m_rich1m, terms=c("forest_cover_5km"))
pred.test4<-ggpredict(m_rich1i, terms=c("edge_5km","prec.total"))


{a.predict1<-plot(pred.test1,rawdata=T,show_ci=T)+  
  labs(title="",x="patch size (log)",y="species richness", col="edge (m)")+
  ylim(0,150)+
  theme_classic()+
  theme(legend.position = c(0.2,0.8),legend.text = element_text(size = 8))}

format_k <- function(x) {ifelse(x >= 1000, paste0(x / 1000, "k"), x)} #showing 1000 meters as 1k

{b.predict2<-plot(pred.test2,rawdata=T,show_ci=T,colors="viridis")+  
  labs(title="",x="edge (m)",y="species richness", col="")+
  ylim(0,150)+
    scale_x_continuous(labels = format_k)+
  theme_classic()+
  theme(legend.position = "none")}

{c.predict3<-plot(pred.test3,rawdata=T,show_ci=T,colors="viridis")+  
  labs(title="",x="forest cover (%)",y="species richness", col="")+
  ylim(0,150)+
  theme_classic()+
  theme(legend.position = "none")}

{d.predict4<-plot(pred.test4,rawdata=T,show_ci=T)+  
  labs(title="",x="edge (m)",y="species richness", col="precip. (mm)")+
  ylim(0,150)+
    scale_x_continuous(labels = format_k)+
  theme_classic()+
  theme(legend.position = c(0.8,0.8),legend.text = element_text(size = 8))}



jpeg("ModelPredictions-Richness-Dez2024.jpeg", width = 17, height = 15,
     units = "cm", pointsize = 9,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(a.predict1, b.predict2, 
             c.predict3, d.predict4, 
             nrow = 2, ncol = 2)
dev.off() 







##### For *species turnover (beta diversity)*
# Forest cover buffers
buffers3 <- c("forest_cover_500m", "forest_cover_1km", "forest_cover_2km", "forest_cover_5km")
landscape4_scaled <- landscape4
for (buffer in buffers3) {landscape4_scaled[[buffer]] <- scale(landscape4[[buffer]])}

results <- list()

for (buffer in buffers3) {model3 <- glmmTMB(beta_SIM ~ get(buffer) + (1|locality_name), 
                        data = landscape4_scaled, 
                        family = ordbeta)
                        results[[buffer]] <- list(model = model3, aic = AIC(model3))}

aic_values3 <- data.frame(buffer = buffers3, aic = sapply(results, function(x) x$aic))
aic_values3$deltaAIC <- aic_values3$aic - min(aic_values3$aic)
slopes3 <- sapply(results, function(x) summary(x$model)$coefficients$cond[2, 1])
aic_values3$slope <- slopes3


# Rename and reorder buffers in the data frame
aic_values3$buffer <- factor(aic_values3$buffer, 
                                     levels = c("forest_cover_500m", "forest_cover_1km", 
                                                "forest_cover_2km", "forest_cover_5km"),
                                     labels = c("500m", "1km", "2km", "5km"))

e<-ggplot(aic_values3, aes(x = buffer, y = deltaAIC)) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue4") +
  theme_minimal() +
  labs(title = "turnover ~ forest cover",
       x = "buffer size",
       y = "delta AIC") +
  ylim(0,5)
  

f<-ggplot(aic_values3, aes(x = buffer, y = slope, group = 1)) +
  geom_point(color = "blue4") +
  geom_line(color = "blue4") +
  theme_minimal() +
  labs(title = "Slopes for forest cover buffers",
       x = "Buffer",
       y = "Slope") +
  ylim(-0.5,0.5)



# Total edge buffers
buffers4 <- c("edge_500m", "edge_1km", "edge_2km", "edge_5km")
landscape4_scaled <- landscape4
for (buffer in buffers4) {landscape4_scaled[[buffer]] <- scale(landscape4[[buffer]])}

results <- list()

for (buffer in buffers4) {model4 <- glmmTMB(beta_SIM ~ get(buffer) + (1|locality_name), 
                        data = landscape4_scaled, 
                        family = ordbeta)
                        results[[buffer]] <- list(model = model4, aic = AIC(model4))}

aic_values4 <- data.frame(buffer = buffers4, aic = sapply(results, function(x) x$aic))
aic_values4$deltaAIC <- aic_values4$aic - min(aic_values4$aic)
slopes4 <- sapply(results, function(x) summary(x$model)$coefficients$cond[2, 1])
aic_values4$slope <- slopes4


# Renomear e ordenar os buffers no data frame combinado
aic_values4$buffer <- factor(aic_values4$buffer, 
                                     levels = c("edge_500m", "edge_1km", 
                                                "edge_2km", "edge_5km"),
                                     labels = c("500m", "1km", "2km", "5km"))

g<-ggplot(aic_values4, aes(x = buffer, y = deltaAIC)) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue4") +
  theme_minimal() +
  labs(title = "turnover ~ total edge",
       x = "buffer size",
       y = "delta AIC") +
  ylim(0,5)


h<-ggplot(aic_values4, aes(x = buffer, y = slope, group =1)) +
  geom_point(color = "blue4") +
  geom_line(color = "blue4") +
  theme_minimal() +
  labs(title = "Slopes for total edge buffers",
       x = "Buffer",
       y = "Slope") +
  ylim(-0.5,0.5)

# Figure for turnover buffers
jpeg("Buffers-RichnessAndTurnover-New.jpeg", width = 17, height = 15,
     units = "cm", pointsize = 9,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(a, c, e, g,
             nrow = 2, ncol = 2)
dev.off() 


###### Model selection 
# glmm models for turnover as the response variable

# For the whole community
m_turn1a.full = glmmTMB(beta_SIM~(scale(patch_log)+scale(forest_cover_1km)+
            scale(edge_1km)+ # full model
            scale(prec.total)+scale(tavg))^2+ 
            (1|locality_name), family = ordbeta,
            data=landscape4, na.action = "na.fail") 
summary(m_turn1a.full)
simulationOutput <- simulateResiduals(fittedModel = m_turn1a.full, plot = TRUE)
vif_results <- check_collinearity(m_turn1a.full)
print(vif_results)
plot(ggeffects::ggpredict(m_turn1a.full, terms = c("patch_log")),rawdata=T)
plot(ggeffects::ggpredict(m_turn1a.full, terms = c("forest_cover_1km")),rawdata=T)
plot(ggeffects::ggpredict(m_turn1a.full, terms = c("patch_log","forest_cover_1km")),rawdata=T)
plot(ggeffects::ggpredict(m_turn1a.full, terms = c("patch_log","edge_1km")),rawdata=T)
plot(ggeffects::ggpredict(m_turn1a.full, terms = c("edge_1km")),rawdata=T)
dredgeturn1<-MuMIn::dredge(global.model = m_turn1a, rank = "AIC")
print(dredgeturn1)
write.xlsx(dredgeturn1,"dredge-FULLMODEL-turn1.xlsx",
            rowNames=F,quote=F)


m_turn1b = glmmTMB(beta_SIM~scale(patch_log)*scale(forest_cover_1km)+
              (1|locality_name), family = ordbeta,
              data=landscape4, na.action = "na.fail")
          summary(m_turn1b)
          simulationOutput <- simulateResiduals(fittedModel = m_turn1b, plot = TRUE)
          plot(ggeffects::ggpredict(m_turn1b, terms = c("patch_log","forest_cover_1km")),rawdata=T)
          vif_results <- check_collinearity(m_turn1b)
          print(vif_results)

m_turn1c = glmmTMB(beta_SIM~scale(patch_log)*scale(edge_1km)+
              (1|locality_name), family = ordbeta,
              data=landscape4, na.action = "na.fail")
          summary(m_turn1c)
          simulationOutput <- simulateResiduals(fittedModel = m_turn1c, plot = TRUE)
          plot(ggeffects::ggpredict(m_turn1c, terms = c("patch_log","edge_1km")),rawdata=T)
          vif_results <- check_collinearity(m_turn1c)
          print(vif_results)

m_turn1d = glmmTMB(beta_SIM~scale(patch_log)*scale(prec.total)+
              (1|locality_name), family = ordbeta,
              data=landscape4, na.action = "na.fail")
          summary(m_turn1d)
          simulationOutput <- simulateResiduals(fittedModel = m_turn1d, plot = TRUE)
          plot(ggeffects::ggpredict(m_turn1d, terms = c("patch_log","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_turn1d)
          print(vif_results)
          
m_turn1e = glmmTMB(beta_SIM~scale(patch_log)*scale(tavg)+
              (1|locality_name), family = ordbeta,
              data=landscape4, na.action = "na.fail")
          summary(m_turn1e)
          simulationOutput <- simulateResiduals(fittedModel = m_turn1e, plot = TRUE)
          plot(ggeffects::ggpredict(m_turn1e, terms = c("patch_log","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_turn1e)
          print(vif_results)
          
m_turn1f = glmmTMB(beta_SIM~scale(forest_cover_1km)*scale(edge_1km)+
              (1|locality_name), family = ordbeta,
              data=landscape4, na.action = "na.fail")
          summary(m_turn1f)
          simulationOutput <- simulateResiduals(fittedModel = m_turn1f, plot = TRUE)
          plot(ggeffects::ggpredict(m_turn1f, terms = c("forest_cover_1km","edge_1km")),rawdata=T)
          vif_results <- check_collinearity(m_turn1f)
          print(vif_results)
                    
m_turn1g = glmmTMB(beta_SIM~scale(forest_cover_1km)*scale(prec.total)+
              (1|locality_name), family = ordbeta,
              data=landscape4, na.action = "na.fail")
          summary(m_turn1g)
          simulationOutput <- simulateResiduals(fittedModel = m_turn1g, plot = TRUE)
          plot(ggeffects::ggpredict(m_turn1g, terms = c("forest_cover_1km","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_turn1g)
          print(vif_results)
          
m_turn1h = glmmTMB(beta_SIM~scale(forest_cover_1km)*scale(tavg)+
              (1|locality_name), family = ordbeta,
              data=landscape4, na.action = "na.fail")
          summary(m_turn1h)
          simulationOutput <- simulateResiduals(fittedModel = m_turn1h, plot = TRUE)
          plot(ggeffects::ggpredict(m_turn1h, terms = c("forest_cover_1km","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_turn1h)
          print(vif_results)
          
m_turn1i = glmmTMB(beta_SIM~scale(edge_1km)*scale(prec.total)+
              (1|locality_name), family = ordbeta,
              data=landscape4, na.action = "na.fail")
          summary(m_turn1i)
          simulationOutput <- simulateResiduals(fittedModel = m_turn1i, plot = TRUE)
          plot(ggeffects::ggpredict(m_turn1i, terms = c("edge_1km","prec.total")),rawdata=T)
          vif_results <- check_collinearity(m_turn1i)
          print(vif_results)
          
m_turn1j = glmmTMB(beta_SIM~scale(edge_1km)*scale(tavg)+
              (1|locality_name), family = ordbeta,
              data=landscape4, na.action = "na.fail")
          summary(m_turn1j)
          simulationOutput <- simulateResiduals(fittedModel = m_turn1j, plot = TRUE)
          plot(ggeffects::ggpredict(m_turn1j, terms = c("edge_1km","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_turn1j)
          print(vif_results)
          
m_turn1k = glmmTMB(beta_SIM~scale(prec.total)*scale(tavg)+
              (1|locality_name), family = ordbeta,
              data=landscape4, na.action = "na.fail")
          summary(m_turn1i)
          simulationOutput <- simulateResiduals(fittedModel = m_turn1i, plot = TRUE)
          plot(ggeffects::ggpredict(m_turn1k, terms = c("prec.total","tavg")),rawdata=T)
          vif_results <- check_collinearity(m_turn1k)
          print(vif_results)
          
m_turn1l = glmmTMB(beta_SIM~scale(patch_log)+ 
              (1|locality_name), family = ordbeta,
              data=landscape4, na.action = "na.fail")
          summary(m_turn1l)
          simulationOutput <- simulateResiduals(fittedModel = m_turn1l, plot = TRUE)
          plot(ggeffects::ggpredict(m_turn1l, terms = c("patch_log")),rawdata=T)
          
m_turn1m = glmmTMB(beta_SIM~scale(forest_cover_1km)+ 
              (1|locality_name), family = ordbeta,
              data=landscape4, na.action = "na.fail")
          summary(m_turn1m)
          simulationOutput <- simulateResiduals(fittedModel = m_turn1m, plot = TRUE)
          plot(ggeffects::ggpredict(m_turn1m, terms = c("forest_cover_1km")),rawdata=T)

m_turn1n = glmmTMB(beta_SIM~scale(edge_1km)+ 
              (1|locality_name), family = ordbeta,
              data=landscape4, na.action = "na.fail")
          summary(m_turn1n)
          simulationOutput <- simulateResiduals(fittedModel = m_turn1n, plot = TRUE)
          plot(ggeffects::ggpredict(m_turn1n, terms = c("edge_1km")),rawdata=T)

m_turn1o = glmmTMB(beta_SIM~scale(prec.total)+ 
              (1|locality_name), family = ordbeta,
              data=landscape4, na.action = "na.fail")
          summary(m_turn1o)
          simulationOutput <- simulateResiduals(fittedModel = m_turn1o, plot = TRUE)
          plot(ggeffects::ggpredict(m_turn1o, terms = c("prec.total")),rawdata=T)

m_turn1p = glmmTMB(beta_SIM~scale(tavg)+ 
              (1|locality_name), family = ordbeta,
              data=landscape4, na.action = "na.fail")
          summary(m_turn1p)
          simulationOutput <- simulateResiduals(fittedModel = m_turn1p, plot = TRUE)
          plot(ggeffects::ggpredict(m_turn1p, terms = c("tavg")),rawdata=T)

m_null1 = glmmTMB(beta_SIM~1+(1|locality_name),
                  family = ordbeta,
                  data = landscape4, na.action = "na.fail")
          summary(m_null1)
          
          
AIC.m_turn1 <- AICtab(m_turn1a.full,m_turn1b,m_turn1c,m_turn1d,
                      m_turn1e,m_turn1f,m_turn1g,m_turn1h,
                      m_turn1i,m_turn1j,m_turn1k,
                      m_turn1l,m_turn1m,m_turn1n,m_turn1o,m_turn1p,
                      m_null1, delta=T,weights=T)
AIC.m_turn1 <- data.frame(AIC.m_turn1)
write.xlsx(AIC.m_turn1,"AIC-beta_SIM-Ordbeta.xlsx",
            rowNames=T,quote=F)


model1m.turn<-capture.output(summary(m_turn1m)) #writing the best model outputs
          write.csv(model1m.turn,"output-model1m-turn-Ordbeta.csv",
          row.names=F,quote=F)
model1g.turn<-capture.output(summary(m_turn1g)) 
          write.csv(model1g.turn,"output-model1g-turn-Ordbeta.csv",
          row.names=F,quote=F)
model1i.turn<-capture.output(summary(m_turn1i)) 
          write.csv(model1i.turn,"output-model1i-turn-Ordbeta.csv",
          row.names=F,quote=F)


pred.test5<-ggpredict(m_turn1m, terms=c("forest_cover_1km")) #model predictions
pred.test6<-ggpredict(m_turn1g, terms=c("forest_cover_1km","prec.total"))
pred.test7<-ggpredict(m_turn1i, terms=c("edge_1km","prec.total"))


{a.predict5<-plot(pred.test5,rawdata=T,show_ci=T,colors="viridis")+  
  labs(title="",x="forest cover (%)",y="turnover", col="")+
  ylim(0.5,1)+
  theme_classic()+
  theme(legend.position = "none")}

{b.predict6<-plot(pred.test6,rawdata=T,show_ci=T)+  
  labs(title="",x="forest cover (%)",y="turnover", col="precip. (mm)")+
  ylim(0.5,1)+
  theme_classic()+
  theme(legend.position = c(0.8,0.24),legend.text = element_text(size = 7))}

{c.predict7<-plot(pred.test7,rawdata=T,show_ci=T)+  
  labs(title="",x="edge (m)",y="turnover", col="precip. (mm)")+
  ylim(0.5,1)+
  theme_classic()+
  theme(legend.position = c(0.2,0.35),legend.text = element_text(size = 7))}

jpeg("ModelPredictions-Turnover-Dez2024.jpeg", width = 8.5, height = 22,
     units = "cm", pointsize = 9,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(a.predict5, b.predict6, c.predict7, 
             nrow = 3, ncol = 1)
dev.off() 







