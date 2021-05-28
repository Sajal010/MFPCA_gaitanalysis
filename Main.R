# Calling all the files that are necessary for getting the index
source("GDI_New.R")     # File that contains function which calculates the Index
source("Data.R")        # File that contains the data

## createFunData
PelD1L <- funData(argvals = 1:51, X = as.matrix(leftdataNewPelAnglesD1[,-1]))
PelD2L <- funData(argvals = 1:51, X = as.matrix(leftdataNewPelAnglesD2[,-1]))
PelD3L <- funData(argvals = 1:51, X = as.matrix(leftdataNewPelAnglesD3[,-1]))
HipD1L <- funData(argvals = 1:51, X = as.matrix(leftdataHipAnglesD1[,-1]))
HipD2L <- funData(argvals = 1:51, X = as.matrix(leftdataHipAnglesD2[,-1]))
HipD3L <- funData(argvals = 1:51, X = as.matrix(leftdataHipAnglesD3[,-1]))
KneeL  <- funData(argvals = 1:51, X = as.matrix(leftdataKneeAngles[,-1]))
AnkleL <- funData(argvals = 1:51, X = as.matrix(leftdataAnkleAngles[,-1]))
FootL    <- funData(argvals = 1:51, X = as.matrix(leftdataFootProgressAngles[,-1]))
PelD1R   <- funData(argvals = 1:51, X = as.matrix(rightdataNewPelAnglesD1[,-1]))
PelD2R   <- funData(argvals = 1:51, X = as.matrix(rightdataNewPelAnglesD2[,-1]))
PelD3R   <- funData(argvals = 1:51, X = as.matrix(rightdataNewPelAnglesD3[,-1]))
HipD1R   <- funData(argvals = 1:51, X = as.matrix(rightdataHipAnglesD1[,-1]))
HipD2R   <- funData(argvals = 1:51, X = as.matrix(rightdataHipAnglesD2[,-1]))
HipD3R   <- funData(argvals = 1:51, X = as.matrix(rightdataHipAnglesD3[,-1]))
KneeR    <- funData(argvals = 1:51, X = as.matrix(rightdataKneeAngles[,-1]))
AnkleR <- funData(argvals = 1:51, X = as.matrix(rightdataAnkleAngles[,-1]))
FootR    <- funData(argvals = 1:51, X = as.matrix(rightdataFootProgressAngles[,-1]))

## createMultiFunData combining all the individual FunData 
CPData <- multiFunData(PelD1L,PelD2L,PelD3L,HipD1L,HipD2L,HipD3L,KneeL,AnkleL,FootL,
                       PelD1R,PelD2R,PelD3R,HipD1R,HipD2R,HipD3R,KneeR,AnkleR,FootR)

#  Calculated univariate FPCA on each joints

#~~~~~~~~~~~~ Left leg
 # Pelvis Ant/Pst
 pca <- PACE(PelD1L)
 pca$sigma2 #0.4634371
 pca$npc #4
 plot(PelD1L, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE)
 
 # Pelvis Up/Dn
 pca <- PACE(PelD2L)
 pca$sigma2 #0.1097007
 pca$npc #6
 plot(PelD2L, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE)
 
 # Pelvis Int/Ext
 pca <- PACE(PelD3L)
 pca$sigma2 #0.1514366
 pca$npc #6
 plot(PelD3L, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE)
 
 # Hip Flx/Ext
 pca <- PACE(HipD1L)
 pca$sigma2 #0.5076788
 pca$npc #5
 plot(HipD1L, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE)
 
 # Hip Add/Abd
 pca <- PACE(HipD2L)
 pca$sigma2 # 0.2501607
 pca$npc #7
 plot(HipD2L, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE)
 
 # Hip Int/Ext
 pca <- PACE(HipD3L)
 pca$sigma2 #  1.718192
 pca$npc #7
 plot(HipD3L, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE)
 
 # Knee angle 
 pca <- PACE(KneeL)
 pca$sigma2 # 0.8132441
 pca$npc #6
 plot(KneeL, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE)
 
 # Ankle angle 
 pca <- PACE(AnkleL)
 pca$sigma2 # 4.984619
 pca$npc #7
 plot(AnkleL, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE) 
 
 #~~~~ Foot angle 
 pca <- PACE(FootL)
 pca$sigma2 # 4.798399
 pca$npc #5
 plot(FootL, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE)

 #~~~~~~~~~~~~ Right leg
 
 # Pelvis Ant/Pst
 pca <- PACE(PelD1R)
 pca$sigma2 #0.5657283
 pca$npc #3
 plot(PelD1R, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE)
 
 # Pelvis Up/Dn
 pca <- PACE(PelD2R)
 pca$sigma2 #0.1783681
 pca$npc #5
 plot(PelD2R, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE)
 
 # Pelvis Int/Ext
 pca <- PACE(PelD3R)
 pca$sigma2 #0.1252056
 pca$npc #7
 plot(PelD3R, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE)
 
 # Hip Flx/Ext
 pca <- PACE(HipD1R)
 pca$sigma2 # 0.3806759
 pca$npc #5
 plot(HipD1R, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE)
 
 # Hip Add/Abd
 pca <- PACE(HipD2R)
 pca$sigma2 # 0.3028449
 pca$npc #7
 plot(HipD2R, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE)
 
 # Hip Int/Ext
 pca <- PACE(HipD3R)
 pca$sigma2 # 1.592641
 pca$npc #7
 plot(HipD3R, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE)
 
 # Knee angle 
 pca <- PACE(KneeR)
 pca$sigma2 # 0.9426296
 pca$npc #7
 plot(KneeR, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE)
 
 # Ankle angle 
 pca <- PACE(AnkleR)
 pca$sigma2 #  4.196122
 pca$npc #6
 plot(AnkleR, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE) 
 
 #~~~~ Foot angle
 pca <- PACE(FootR)
 pca$sigma2 # 2.051429
 pca$npc #6
 plot(FootR, lty = 2, obs = 1:7)
 plot(pca$fit, lty = 2, obs = 1:7, add = TRUE)
 
# List containing data of all the joints together for both the legs 
CPData <- list(leftdataNewPelAnglesD1[,-1],
               leftdataNewPelAnglesD2[,-1],
               leftdataNewPelAnglesD3[,-1],
               leftdataHipAnglesD1[,-1],
               leftdataHipAnglesD2[,-1],
               leftdataHipAnglesD3[,-1],
               leftdataKneeAngles[,-1],
               leftdataAnkleAngles[,-1],
               leftdataFootProgressAngles[,-1],
               rightdataNewPelAnglesD1[,-1],
               rightdataNewPelAnglesD2[,-1],
               rightdataNewPelAnglesD3[,-1],
               rightdataHipAnglesD1[,-1],
               rightdataHipAnglesD2[,-1],
               rightdataHipAnglesD3[,-1],
               rightdataKneeAngles[,-1],
               rightdataAnkleAngles[,-1],
               rightdataFootProgressAngles[,-1])

# No. of PC's of all the joints calculated above using univariate fubctional principal components.
NC = c(4,6,6,5,7,7,6,7,5,
       3,5,7,5,7,7,7,6,6)

# Calculating Index with 50 PC's
Index = GDI_new(CPData,ID,NC,50)


# Multiplying the Index by 100 so that they are on the scale of 0 to 100 where 0 means no abnormality and 100 means most abnormal behaviour.
Index*100
