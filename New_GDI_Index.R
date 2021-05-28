# Function for calculating index for all subjects
GDI_new <- function(G,ID,NPC,Mval){
  
  # No.of time points in the data
  NT = ncol(G[[1]])
  
  ## createFunData that stores data for each joint of the 9 joints for both the legs
  PelD1L <- funData(argvals = 1:NT, X = as.matrix(G[[1]]))
  PelD2L <- funData(argvals = 1:NT, X = as.matrix(G[[2]]))
  PelD3L <- funData(argvals = 1:NT, X = as.matrix(G[[3]]))
  HipD1L <- funData(argvals = 1:NT, X = as.matrix(G[[4]]))
  HipD2L <- funData(argvals = 1:NT, X = as.matrix(G[[5]]))
  HipD3L <- funData(argvals = 1:NT, X = as.matrix(G[[6]]))
  KneeL  <- funData(argvals = 1:NT, X = as.matrix(G[[7]]))
  AnkleL <- funData(argvals = 1:NT, X = as.matrix(G[[8]]))
  FootL    <- funData(argvals = 1:NT, X = as.matrix(G[[9]]))
  PelD1R   <- funData(argvals = 1:NT, X = as.matrix(G[[10]]))
  PelD2R   <- funData(argvals = 1:NT, X = as.matrix(G[[11]]))
  PelD3R   <- funData(argvals = 1:NT, X = as.matrix(G[[12]]))
  HipD1R   <- funData(argvals = 1:NT, X = as.matrix(G[[13]]))
  HipR   <- funData(argvals = 1:NT, X = as.matrix(G[[14]]))
  KneeR    <- funData(argvals = 1:NT, X = as.matrix(G[[15]]))
  AnkleD1R <- funData(argvals = 1:NT, X = as.matrix(G[[16]]))
  AnkleD3R <- funData(argvals = 1:NT, X = as.matrix(G[[17]]))
  FootR    <- funData(argvals = 1:NT, X = as.matrix(G[[18]]))
  
  ## createMultiFunData combining all the individual FunData 
  CPData <- multiFunData(PelD1L,PelD2L,PelD3L,HipD1L,HipD2L,HipD3L,KneeL,AnkleL,FootL,
                         PelD1R,PelD2R,PelD3R,HipD1R,HipD2R,HipD3R,KneeR,AnkleR,FootR)
  
  # List containing information about no. of PC's for each joint for each leg
  uniExpansions <- list(list(type = "uFPCA", npc = NPC[1]), 
                        list(type = "uFPCA", npc = NPC[2]),
                        list(type = "uFPCA", npc = NPC[3]),
                        list(type = "uFPCA", npc = NPC[4]),
                        list(type = "uFPCA", npc = NPC[5]),
                        list(type = "uFPCA", npc = NPC[6]),
                        list(type = "uFPCA", npc = NPC[7]),
                        list(type = "uFPCA", npc = NPC[8]),
                        list(type = "uFPCA", npc = NPC[9]),
                        list(type = "uFPCA", npc = NPC[10]),
                        list(type = "uFPCA", npc = NPC[11]), 
                        list(type = "uFPCA", npc = NPC[12]),
                        list(type = "uFPCA", npc = NPC[13]),
                        list(type = "uFPCA", npc = NPC[14]),
                        list(type = "uFPCA", npc = NPC[15]),
                        list(type = "uFPCA", npc = NPC[16]),
                        list(type = "uFPCA", npc = NPC[17]),
                        list(type = "uFPCA", npc = NPC[18])) 
  
  ## MFPCA_fit_weights it calculates the individual joints variance
  var1 <- funData(argvals = CPData[[1]]@argvals, 
                  X = matrix(apply(CPData[[1]]@X, 2, var), nrow = 1))
  var2 <- funData(argvals = CPData[[2]]@argvals, 
                  X = matrix(apply(CPData[[2]]@X, 2, var), nrow = 1))
  var3 <- funData(argvals = CPData[[3]]@argvals, 
                  X = matrix(apply(CPData[[3]]@X, 2, var), nrow = 1))
  var4 <- funData(argvals = CPData[[4]]@argvals, 
                  X = matrix(apply(CPData[[4]]@X, 2, var), nrow = 1))
  var5 <- funData(argvals = CPData[[5]]@argvals, 
                  X = matrix(apply(CPData[[5]]@X, 2, var), nrow = 1))
  var6 <- funData(argvals = CPData[[6]]@argvals, 
                  X = matrix(apply(CPData[[6]]@X, 2, var), nrow = 1))
  var7 <- funData(argvals = CPData[[7]]@argvals, 
                  X = matrix(apply(CPData[[7]]@X, 2, var), nrow = 1))
  var8 <- funData(argvals = CPData[[8]]@argvals, 
                  X = matrix(apply(CPData[[8]]@X, 2, var), nrow = 1))
  var9 <- funData(argvals = CPData[[9]]@argvals, 
                  X = matrix(apply(CPData[[9]]@X, 2, var), nrow = 1))
  var10 <- funData(argvals = CPData[[10]]@argvals, 
                   X = matrix(apply(CPData[[10]]@X, 2, var), nrow = 1))
  var11 <- funData(argvals = CPData[[11]]@argvals, 
                   X = matrix(apply(CPData[[11]]@X, 2, var), nrow = 1))
  var12 <- funData(argvals = CPData[[12]]@argvals, 
                   X = matrix(apply(CPData[[12]]@X, 2, var), nrow = 1))
  var13 <- funData(argvals = CPData[[13]]@argvals, 
                   X = matrix(apply(CPData[[13]]@X, 2, var), nrow = 1))
  var14 <- funData(argvals = CPData[[14]]@argvals, 
                   X = matrix(apply(CPData[[14]]@X, 2, var), nrow = 1))
  var15 <- funData(argvals = CPData[[15]]@argvals, 
                   X = matrix(apply(CPData[[15]]@X, 2, var), nrow = 1))
  var16 <- funData(argvals = CPData[[16]]@argvals, 
                   X = matrix(apply(CPData[[16]]@X, 2, var), nrow = 1))
  var17 <- funData(argvals = CPData[[17]]@argvals, 
                   X = matrix(apply(CPData[[17]]@X, 2, var), nrow = 1))
  var18 <- funData(argvals = CPData[[18]]@argvals, 
                   X = matrix(apply(CPData[[18]]@X, 2, var), nrow = 1))
  
  # It calculates the weight of each joint so that all of them are on the same scale for compasrison
  weight <- c(1/integrate(var1), 1/integrate(var2),
              1/integrate(var3), 1/integrate(var4),
              1/integrate(var5), 1/integrate(var6),
              1/integrate(var7), 1/integrate(var8),
              1/integrate(var9), 1/integrate(var10),
              1/integrate(var11), 1/integrate(var12),
              1/integrate(var13), 1/integrate(var14),
              1/integrate(var15), 1/integrate(var16),
              1/integrate(var17), 1/integrate(var18))
  
  # MFPCA function that runs the Multivarite functional principal component analysis on all the joints
  MFPCACP <- MFPCA(CPData, M = Mval, 
                   uniExpansions = uniExpansions, 
                   weights = weight, fit = TRUE)
  
  # Segregating according to the labels whether the curve belongs to Controls or Cases
  ind_ctrl = which(ID=="Control")
  ind_case = which(ID=="Case")
  
  # Extracting curve that belongs to Control group
  ctrl = extractObs(MFPCACP$fit, obs = ind_ctrl)
  
  # Subtracting each curve by the mean of control group curve
  D    = (MFPCACP$fit - meanFunction(ctrl))^2
  
  # It integrates the curves to get a single number and divide it by its maximum so that the scale of the index is same
  GDI_MAT = integrate(D)/max(integrate(D))
  
  # Return the index
  return(GDI_MAT)
  
}  
