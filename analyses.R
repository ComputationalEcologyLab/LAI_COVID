library("dplyr")
library("MASS")
library("glmmTMB")
library("ggstance")
library("ggplot2")

#Main analysis
glmm.off.main2 <- glmmTMB(Deaths ~ factor(LAI_dec) + LessHSEdu + MoreThan1PerRoom 
                          + Medicaid18_64 + Over64 + NativeAmerican + Black   
                         + Phys_Inactive_pct + NbrCOVIDmean + 
                           (1|State) + offset(log(TotPopulation))
                         , data = M1, family = nbinom1(), ziformula =~1, REML = TRUE) 

summary(glmm.off.main2)

exp(confint(glmm.off.main2,"factor(LAI_dec)2"))
exp(confint(glmm.off.main2,"factor(LAI_dec)3"))
exp(confint(glmm.off.main2,"factor(LAI_dec)4"))
exp(confint(glmm.off.main2,"factor(LAI_dec)5"))
exp(confint(glmm.off.main2,"factor(LAI_dec)6"))
exp(confint(glmm.off.main2,"factor(LAI_dec)7"))
exp(confint(glmm.off.main2,"factor(LAI_dec)8"))
exp(confint(glmm.off.main2,"factor(LAI_dec)9"))
exp(confint(glmm.off.main2,"factor(LAI_dec)10"))

exp(confint(glmm.off.main2,"LessHSEdu"))
exp(confint(glmm.off.main2,"MoreThan1PerRoom"))
exp(confint(glmm.off.main2,"Medicaid18_64"))
exp(confint(glmm.off.main2,"Over64"))
exp(confint(glmm.off.main2,"NativeAmerican"))
exp(confint(glmm.off.main2,"Black"))

#Main model: Unadjusted 
glmm.off.main3 <- glmmTMB(Deaths ~ factor(LAI_dec)
                          , data = M1, family = nbinom1(), ziformula =~1, REML = TRUE) 

summary(glmm.off.main3)

exp(confint(glmm.off.main3,"factor(LAI_dec)2"))
exp(confint(glmm.off.main3,"factor(LAI_dec)3"))
exp(confint(glmm.off.main3,"factor(LAI_dec)4"))
exp(confint(glmm.off.main3,"factor(LAI_dec)5"))
exp(confint(glmm.off.main3,"factor(LAI_dec)6"))
exp(confint(glmm.off.main3,"factor(LAI_dec)7"))
exp(confint(glmm.off.main3,"factor(LAI_dec)8"))
exp(confint(glmm.off.main3,"factor(LAI_dec)9"))
exp(confint(glmm.off.main3,"factor(LAI_dec)10"))

##MRR figure

# Create labels
boxLabels = c("LAI decile 1", "LAI decile 2", "LAI decile 3", "LAI decile 4", "LAI decile 5", "LAI decile 6", 
              "LAI decile 7", "LAI decile 8", "LAI decile 9", "LAI decile10")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
boxLabels

#Main adjusted model
df <- data.frame(yAxis = length(boxLabels):1,boxOdds = c(1.0,0.90,0.94,0.95,0.86,0.94,0.93,0.82,0.78,0.59), 
                 boxCILow = c(0,0.82,0.86,0.86,0.78,0.85,0.83,0.72,0.68,0.50),
                 boxCIHigh = c(0,1.00,1.02,1.05,0.95,1.05,1.04,0.93,0.89,0.69))
df$yAxis 

(p <- ggplot(df, aes(x = boxOdds, y = boxLabels)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed", color="red4") + 
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = 
                     .2, color = "black") +
    geom_point(size = 3.5, color = "chartreuse4") +
    
    
    scale_x_continuous(n.breaks = NULL, labels = waiver(),
                       limits = (c(0.4, 1.3))) +
    theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    ylab("Leaf Area Index") +
    xlab("Mortality Rate Ratio") +
    ggtitle("Figure 3b. Adjusted COVID-19 Mortality Rate Ratio")
) 

#Main Unadjusted model
df2 <- data.frame(yAxis = length(boxLabels):1,boxOdds = c(1.0,0.79,1.07,1.15,1.18,1.23,1.22,1.13,1.03,0.72), 
                 boxCILow = c(0,0.65,0.89,0.96,0.99,1.02,1.01,0.94,0.86,0.59),
                 boxCIHigh = c(0,0.97,1.29,1.39,1.42,1.49,1.46,1.36,1.25,0.87))
df2$yAxis 

(p <- ggplot(df2, aes(x = boxOdds, y = boxLabels)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed", color="red4") + 
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = 
                     .2, color = "black") +
    geom_point(size = 3.5, color = "chartreuse4") +
    
    
    scale_x_continuous(n.breaks = NULL, labels = waiver(),
                       limits = (c(0.4, 2))) +
    theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    ylab("Leaf Area Index") +
    xlab("Mortality Rate Ratio") +
    ggtitle("Figure 3a. Unadjusted COVID-19 Mortality Rate Ratio")
)
