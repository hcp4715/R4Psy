# A tutorial on Bayesian networks for psychopathology researchers
# Briganti G, Scutari M, McNally RJ

library(stats)
library(qgraph)
library(readr)
library(bootnet)
library(dplyr)
library(corpcor)
library(bnlearn)
library(psych)
library (ggplot2)

data <- read_delim("dag/data_dag.csv", ";", 
                   escape_double = FALSE, trim_ws = TRUE)

names <- c("DepMood", 
           "Sleep", 
           "Weight", 
           "Fatigue", 
           "Irritable", 
           "SuicideId", 
           "Anhedonia")

longnames <- c("Depressive Mood", 
               "Sleep Problems", 
               "Weight", 
               "Fatigue", 
               "Irritable", 
               "Suicidal Ideation", 
               "Anhedonia")

data <- as.data.frame(matrix(as.numeric(as.matrix(data)), 
                             ncol=ncol(data), 
                             byrow=TRUE))  #set data as numerics

# PC algorithm

BNpc<-pc.stable(data)
pdf("pcdag.pdf", width=10, height=6)
qgraph(BNpc, labels=names, 
       nodeNames=longnames, 
       legend.cex=.5, 
       vsize=6, 
       layout="circle")
dev.off()

# Boot stability for the DAG
# Strength: connection strength, e.g. 0.85 > 
#              connection appears in 86% of the fitted networks.
# Direction: probability of the direction
#     e.g. 0.57 means that in 57% of the fitted networks the connection goes in 
#                the direction depicted in the graph.

BST <- boot.strength(data, 
                     R = 20, 
                     algorithm = "pc.stable")  
head(BST)
qgraph(BST) # visualize output with qgraph
BST[BST$strength > 0.85 & BST$direction > 0.5, ]

avgnet1 <- averaged.network(BST, 
                            threshold = 0.85)
avgnet1

bnlearn::score(avgnet1, data = data)

astr1 <- arc.strength(avgnet1, 
                      data, "bic-g")  # compute edge strengths


pdf("DAGstable.pdf", 
    width=10, 
    height=6)
qgraph(astr1, 
       labels=names, 
       nodeNames=longnames, 
       legend.cex=.5, 
       vsize=6, 
       layout="circle")
dev.off()


# Hill Climbing algorithm

# Boot stability for the DAG
# Strength: connection strength, e.g. 0.85 > 
#              connection appears in 86% of the fitted networks.
# Direction: probability of the direction
#     e.g. 0.57 means that in 57% of the fitted networks the connection goes in 
#                the direction depicted in the graph.

BST <- boot.strength(data, R = 1000, 
                     algorithm = "hc", 
                     debug = TRUE)  
head(BST)
BST[BST$strength > 0.85 & BST$direction > 0.5, ]
qgraph(BST)

avgnet1 <- averaged.network(BST, 
                            threshold = 0.85)
avgnet1

bnlearn::score(avgnet1, data = data)

astr1 <- arc.strength(avgnet1, data, "bic-g")   ## compute edge strengths

pdf("hcDAGstable.pdf", width=10, height=6)
qgraph(astr1, 
       labels=names, 
       nodeNames=longnames, 
       legend.cex=.5, 
       vsize=6, 
       layout="circle")
dev.off()


# Equivalence classes

BNpc<-pc.stable(data)
BNcp <- cpdag(BNpc)
pdf("cpdag.pdf", width=10, height=6)
qgraph(BNcp, 
       labels=names, 
       nodeNames=longnames, 
       legend.cex=.5, 
       vsize=6, 
       layout="circle")
dev.off()

vstructs(BNcp)

BST <- boot.strength(data, R = 1000, 
                     algorithm = "pc.stable", 
                     debug = TRUE, 
                     cpdag=TRUE)


head(BST)
BST[BST$strength > 0.85 & BST$direction > 0.5, ]
qgraph(BST)

avgnet1 <- averaged.network(BST, 
                            threshold = 0.85)
avgnet1

bnlearn::score(avgnet1, data = data)

astr1 <- arc.strength(avgnet1, 
                      data, 
                      "bic-g")   ## compute edge strengths


pdf("cpDAGstable.pdf", width=10, height=6)
qgraph(astr1, 
       labels=names, 
       nodeNames=longnames, 
       legend.cex=.5, 
       vsize=6, 
       layout="circle")
dev.off()


# Grow shrink
BNgs <- gs(data)
pdf("gsDAG.pdf", width=10, height = 6)
qgraph(BNgs, 
       labels=names, 
       nodeNames=longnames, 
       legend.cex=.5, 
       vsize=6, 
       layout="circle")
dev.off()


BST <- boot.strength(data, R = 1000, 
                     algorithm = "gs", 
                     debug = TRUE)  
head(BST)
BST[BST$strength > 0.85 & BST$direction > 0.5, ]
qgraph(BST)

avgnet1 <- averaged.network(BST, threshold = 0.85)
avgnet1

bnlearn::score(avgnet1, data = data)

astr1 <- arc.strength(avgnet1, data, "bic-g")   ## compute edge strengths

pdf("gsDAGstable.pdf", width=10, height=6)
qgraph(astr1, 
       labels=names, 
       nodeNames=longnames, 
       legend.cex=.5, 
       vsize=6, 
       layout="circle")
dev.off()



# Restricted Maximization hybrid algorithm

BNrs <- rsmax2(data)
BST <- boot.strength(data, 
                     R = 1000, 
                     algorithm = "rsmax2", 
                     debug = TRUE)  
head(BST)
BST[BST$strength > 0.85 & BST$direction > 0.5, ]

avgnet1 <- averaged.network(BST, 
                            threshold = 0.85)
avgnet1

bnlearn::score(avgnet1, data = data)

astr1 <- arc.strength(avgnet1, data, "bic-g")   ## compute edge strengths

pdf("rsDAGstable.pdf", width=10, height=6)
qgraph(astr1, 
       labels=names, 
       nodeNames=longnames, 
       legend.cex=.5, 
       vsize=6, 
       layout="circle")
dev.off()


## GGM for comparison
n1 <- estimateNetwork(data, default="EBICglasso", 
                      threshold=TRUE)
pdf("glasso.pdf", width=10, height=6)
plot(n1, layout="circle", 
     labels=names, 
     nodeNames=longnames,
     theme="colorblind", 
     legend.cex=0.5,
     vsize=6)
dev.off()
