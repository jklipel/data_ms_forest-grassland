
#     Variability in leaf traits reveals contrasting strategies ##
#     between forest and grassland woody communities across southern Brazil  ##

### AIM I ###

# data
test_i <- read.table("diver_cwm.txt", h=T); head(test_i)
names(test_i)
# lme
library(nlme)
library(MuMIn)
library(ape)

ses_PD_all <- lme(ses_PD_all ~ Habitat, data=test_i, random=~1|Blocos, method = "ML"); summary(ses_PD_all);r.squaredGLMM(ses_PD_all)
ses_PD_ang <- lme(ses_PD_ang ~ Habitat, data=test_i, random=~1|Blocos, method = "ML"); summary(ses_PD_ang);r.squaredGLMM(ses_PD_ang)
lap <- lme(log(FD_LAp) ~ Habitat, data=test_i, random=~1|Blocos, method = "ML"); summary(lap);r.squaredGLMM(lap)
slap <- lme(log(FD_SLAp) ~ Habitat, data=test_i, random=~1|Blocos, method = "ML"); summary(slap);r.squaredGLMM(slap)
ldmcp <- lme(log(FD_LDMCp) ~ Habitat, data=test_i, random=~1|Blocos, method = "ML"); summary(ldmcp);r.squaredGLMM(ldmcp)
la <- lme(log(CWM_LA) ~ Habitat, data=test_i, random=~1|Blocos, method = "ML"); summary(la); r.squaredGLMM(la)
sla <- lme(log(CWM_SLA) ~ Habitat, data=test_i, random=~1|Blocos, method = "ML"); summary(sla);  r.squaredGLMM(sla)
ldmc <- lme(log(CWM_LDMC) ~ Habitat, data=test_i, random=~1|Blocos, method = "ML"); summary(ldmc) ; r.squaredGLMM(ldmc)

###  AIM II ###
# data
habitat <- read.table("habitat_traits.txt", h=T); head(habitat)
habitat[,3:5] <- log(habitat[,3:5])

#test
eco_LA <- aov(LA~Habitat, habitat); summary(eco_LA)
TukeyHSD(eco_LA)
plot(TukeyHSD(eco_LA))


eco_SLA <- aov(SLA~Habitat, habitat); summary(eco_SLA)
TukeyHSD(eco_SLA)
plot(TukeyHSD(eco_SLA))

eco_LDMC <- aov(LDMC~Habitat, habitat); summary(eco_LDMC)
TukeyHSD(eco_LDMC)
plot(TukeyHSD(eco_LDMC))

### AIM III ###

#data
test_intra <- read.table("test_t_intra.txt", h=T); head(test_intra)
test_intra[,2:7] <- log(test_intra[,2:7])

#test
la_test <- t.test(test_intra$LA_F, test_intra$LA_G, paired= T)
la_test 
boxplot(test_intra$LA_F, test_intra$LA_G)
p.adjust(la_test$p.value, method = "BH")

sla_test <- t.test(test_intra$SLA_F, test_intra$SLA_G, paired= T)
sla_test 
boxplot(test_intra$SLA_F, test_intra$SLA_G)
p.adjust(sla_test$p.value, method = "BH")

ldmc_test <- t.test(test_intra$LDMC_F, test_intra$LDMC_G, paired= T)
ldmc_test 
boxplot(test_intra$LDMC_F, test_intra$LDMC_G)
p.adjust(ldmc_test$p.value, method = "BH")
