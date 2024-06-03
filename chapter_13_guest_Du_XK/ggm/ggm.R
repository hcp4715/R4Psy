
#################################################################
##     Tutorial script on GGM estimation and mod selection     ##
##                          Xinkai Du                          ##
##                    xinkai.du.xd@gmail.com                   ##
#################################################################

# load libraries
pacman::p_load(bootnet, qgraph, psychonetrics, tidyverse, BGGM)

# load data
data("bfi")


# preprocessing -----------------------------------------------------------


# only use the first 25 items
bfi <- bfi[, 1:25]

bfi_na.rm <- na.omit(bfi)

# estimate unconstrained networks -----------------------------------------

##-----------
##  bootnet  
##-----------

# estimate
net_boot <- estimateNetwork(data = bfi_na.rm, default = "pcor")

# store the ggm
graph_boot <- net_boot$graph

# Item descriptions
Names <- scan("http://sachaepskamp.com/files/BFIitems.txt",
              what = "character", sep = "\n")

# Form item clusters
Traits <- rep(c(
  'Agreeableness',
  'Conscientiousness',
  'Extraversion',
  'Neuroticism',
  'Opennness' ),each=5)

# plot the network
qgraph(graph_boot,
     layout = "spring", 
     theme = "colorblind", 
     groups = Traits, 
     nodeNames = Names, 
     legend.cex = 0.4,
     filetype = "pdf", 
     filename = "ggm/bootnet") 

##-----------------
##  psychonetrics  
##-----------------

# FIML handles missing data
net_psy <- ggm(bfi, estimator = "FIML") %>% runmodel

# store ggm
graph_psy <- getmatrix(net_psy, "omega")

# plot the network
qgraph(graph_psy,
       layout = "spring", 
       theme = "colorblind", 
       groups = Traits, 
       nodeNames = Names, 
       legend.cex = 0.4,
       filetype = "pdf", 
       filename = "ggm/psychonetrics")

##--------
##  BGGM  
##--------

# handles missing data with mice imputation by default
net_bggm <- explore(bfi)

graph_bggm <- net_bggm$pcor_mat

qgraph(graph_bggm,
       layout = "spring", 
       theme = "colorblind", 
       groups = Traits, 
       nodeNames = Names, 
       legend.cex = 0.4,
       filetype = "pdf", 
       filename = "ggm/bggm")

# compare the three networks
L <- averageLayout(graph_boot, graph_psy, graph_bggm) 
layout(t(1:3)) 

qgraph(graph_boot, title = "bootnet", layout = L) 
qgraph(graph_psy, title = "psychonetrics", layout = L)
qgraph(graph_bggm, title = "bggm", layout = L)


dev.off()


# compare parameter estimations 
plot(c(graph_boot), c(graph_psy), xlab = "bootnet", ylab = "psychonetrics")

abline(coef(lm(c(graph_boot) ~ c(graph_psy)))[1], coef(lm(c(graph_boot) ~ c(graph_psy)))[2])


plot(c(graph_bggm), c(graph_psy), xlab = "bggm", ylab = "psychonetrics")

abline(coef(lm(c(graph_bggm) ~ c(graph_psy)))[1], coef(lm(c(graph_boot) ~ c(graph_psy)))[2])


# model selection ---------------------------------------------------------

##----------------
##  Thresholding  
##----------------

# bootnet: sig
net_boot_thresh_sig <- estimateNetwork(bfi, default = "pcor", threshold = "sig", alpha = 0.01)

qgraph(net_boot_thresh_sig$graph, layout = L, title = "threshold alpha = .01")
qgraph(net_boot$graph, layout = L)

# bootnet: boot
net_boot_booted <- bootnet(net_boot, nCores = parallel::detectCores(), nBoots = 100)

net_boot_thresh_boot <- bootThreshold(net_boot_booted, threshold = 0.01)

qgraph(net_boot_thresh_boot$graph, layout = L, title = "threshold boot .01")

# BGGM: threshold with credible interval
net_bggm_thresh_ci <- estimate(bfi) %>% select

qgraph(net_bggm_thresh_ci$pcor_adj, layout = L, title = "bggm threshold ci")

# BGGM: threshold with BF
net_bggm_thresh_bf <- explore(bfi) %>% select

qgraph(net_bggm_thresh_bf$pcor_mat, layout = L, title = "bggm threshold bf")

##-----------
##  pruning  
##-----------

net_psy_prune <- ggm(bfi, estimator = "FIML") %>% prune(alpha = 0.01, iterative = TRUE) %>% runmodel

qgraph(getmatrix(net_psy_prune, "omega"), layout = L, title = "psychonetrics pruned")

##------------------
##  regularization  
##------------------

net_boot_reg <- estimateNetwork(bfi_na.rm, default = "EBICglasso", tuning = 0.5)

qgraph(net_boot_reg$graph, layout = L, title = "bootnet EBICglasso")
