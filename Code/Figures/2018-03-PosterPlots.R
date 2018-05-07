####LIBRARIES####
library(tidyverse)
library(ggpubr)
library(broom)
setwd('..')
source("PlotFaces.R")
source("Distances.R")

####DATABASES####
setwd('..')
path <- getwd()
setwd(paste(path, "/Results/MergedData", sep = ""))
load("MergedDat.RData")
MergedDat[[1]]$Sex <- as.factor(MergedDat[[1]]$Sex)

setwd(paste(path, "/Results/FacePCA", sep = ""))
face.eigenvalues  <- read_delim("eigenvalues.csv", delim = " ", col_names = FALSE)
face.eigenvectors <- read_csv("eigenvectors.csv", col_names = FALSE)
face.facets       <- read_csv("facets.csv", col_names = FALSE)
face.means        <- read_csv("means.csv", col_names = FALSE)

setwd(paste(path, "/Results/GenPCA", sep = ""))
gen.eigenvalues <- read_delim("PCA.eigenval", delim = " ", col_names = FALSE)

#Color palette
OkabeIto <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

####REGRESSION MODELS AND VECTOR####
matrix  <- dplyr::select(MergedDat[[1]], starts_with("Face"))
covs    <- MergedDat[[1]] %>% select(c("Sex", "Age", "Height", "Weight", "BMI", "prediction"))
covs[,c(2:5)] <- scale(covs[,c(2:5)])

regressnon <- cbind(covs, matrix) %>% group_by(prediction) %>%
  do(model = lm(as.matrix(dplyr::select(., starts_with("Face"))) ~ Sex +  Age + Height + BMI
                , data = .)) %>% tidy(model)

regresstot <- cbind(covs, matrix) %>% group_by(prediction) %>%
  do(model = lm(as.matrix(dplyr::select(., starts_with("Face"))) ~ Sex + Age + BMI
                , data = .)) %>% tidy(model)

extractRegress <- function(dat, filter, select){
  coefs <- dat %>% group_by(prediction) %>% filter(term == filter) %>%
    dplyr::select(select)
  final <- as.data.frame(matrix(0, ncol = 88, nrow = 5))
  colnames(final) <- c("Pop", colnames(dplyr::select(MergedDat[[1]], starts_with("Face") ) ) )
  final$Pop       <- levels(MergedDat[[1]]$prediction)
  for(i in 1:nrow(final)){
    t <- coefs %>% filter(prediction == final[i,1])
    final[i,2:88] <- c(t[,select])[[1]]
  }
  return(final)
}

sexcoefstot   <- extractRegress(regresstot, "SexMale", "estimate")
sexcoefstotse <- extractRegress(regresstot, "SexMale", "std.error")
sexcoefstotunit <- sexcoefstot
sexcoefstotunit[,2:88] <- t(apply(sexcoefstot[,2:88], 1, function(x) x/norm(x, type = "2")))

sexcoefsnon   <- extractRegress(regressnon, "SexMale", "estimate")
sexcoefsnonse <- extractRegress(regressnon, "SexMale", "std.error")
sexcoefsnonunit <- sexcoefsnon
sexcoefsnonunit[,2:87] <- t(apply(sexcoefsnon[,2:87], 1, function(x) x/norm(x, type = "2")))

heightcoefs   <- extractRegress(regressnon, "Height", "estimate")
row <- 1
for(i in seq(1,10,2)){ #to scale by sex difference
  t <- covs %>% group_by(prediction, Sex) %>% select(Height) %>% summarise_all(funs(mean(., na.rm = T)))
  heightcoefs[row, 2:88] <- heightcoefs[row, 2:88] * abs(t$Height[(i+1)] - t$Height[i])
  row <- row + 1
}
heightcoefsse <- extractRegress(regressnon, "Height", "std.error")
heightcoefsunit <- heightcoefs
heightcoefsunit[,2:87] <- t(apply(heightcoefs[,2:87], 1, function(x) x/norm(x, type = "2")))

####FUNCTIONS####
plotGPCA <- function(dat, x1, y1, xlabel, ylabel, col = "super_pop"){
  p1 <- ggplot(dat, aes_string(x = x1, y = y1, color = col)) +
    geom_point(alpha = 0.3, size = 3) + labs(color = "Pop") +
    theme_pubr() + 
    theme(legend.title = element_text(size=18)) + #Change legend size 
    theme(legend.text  = element_text(size=18)) + #Change text size
    guides(color = guide_legend(override.aes = list(size = 10, alpha = 1))) + #Change legend size and alpha
    xlab(xlabel) +
    ylab(ylabel)
  p1 <- ggpar(p1, palette = OkabeIto)
  return(p1)
}

plotFPCA <- function(PCA, x1, y1, xlim = NULL, ylim = NULL){
  xname <- paste0("Face ", gsub("^.*?_", "" , x1), " (35.8%)")
  yname <- paste0("Face ", gsub("^.*?_", "" , y1), " (12.7%)")
  xmax  <- max(PCA[x1])
  ymax  <- max(PCA[y1])
  xmin  <- min(PCA[x1])
  ymin  <- min(PCA[y1])
  p1   <- ggplot(PCA, aes_string(x = x1, y = y1, color = "Sex")) +
    geom_point(alpha = 0.3) +
    # y axis line
    geom_segment(x = 0, xend = 0, 
                 y = ymin, yend = ymax - (ymax/10) ,
                 size = 1, 
                 color = "black") +
    # x axis line
    geom_segment(y = 0, yend = 0, 
                 x = xmin, xend = xmax,
                 size = 1, 
                 color = "black") +
    labs(x = xname, y = yname) +
    theme(legend.title = element_text(size=1)) + #Change legend size 
    theme(legend.text  = element_text(size=1)) + #Change text size
    guides(color = guide_legend(override.aes = list(size = 10, alpha = 1))) + #Change legend size
    annotate("text", x = xmax - (xmax/10), y = -xmax/10, label = xname, size = 4.5) +
    annotate("text", x = 0, y = ymax, label = yname, size = 4.5) +
    theme_pubr() 
  
  if(is.null(xlim)){
  } else {
    p1 <- p1 + xlim(xlim[1], xlim[2])
  }
  if(is.null(ylim)){
  } else {
    p1 <- p1 + ylim(ylim[1], ylim[2])
  }
  
  p1 <- ggpar(p1, palette = OkabeIto)
  p1 <- p1 + theme_void(base_size=20)
  return(p1)
}

plotFPCApop <- function(PCA, cont, x1, y1, avgs){
  xmax  <- max(PCA[x1])
  ymax  <- max(PCA[y1])
  xmin  <- min(PCA[x1])
  ymin  <- min(PCA[y1])
  highPCA <- filter(PCA, cont)
  backPCA <- filter(PCA, !cont)
  sexes   <- filter(PCA, cont)$Sex
  if(is.null(avgs)){
    avgs    <- highPCA %>% select(x1, y1, Sex) %>% 
              group_by(Sex) %>% summarise_all(funs(mean))
  }
  p1 <- ggplot(highPCA, aes_string(x = x1, y = y1)) + 
    geom_point(aes(color = sexes), alpha = 0.5) +
    geom_point(data = avgs, size = 5, aes(color = Sex)) +
    geom_point(data = avgs, size = 3, color = "black") +
    #geom_point(data = backPCA, alpha = 0.01, color = "white", aes_string(x = x1, y = y1)) + 
    #y axis line
    geom_segment(x = 0, xend = 0, 
                 y = ymin, yend = ymax - (ymax/20) ,
                 size = 1, 
                 color = "black") +
    # x axis line
    geom_segment(y = 0, yend = 0, 
                 x = xmin, xend = xmax,
                 size = 1, 
                 color = "black") +
    ylim(ymin, ymax) + 
    xlim(xmin, xmax) +
    theme_pubr() 
  
  p1 <- ggpar(p1, palette = OkabeIto)
  p1 <- p1 + theme_void()
  return(p1)
}

plotsquare <- function(PCA, x1, y1, vectors, se, xlim = NULL, ylim = NULL){
  minmax  <- data.frame(ymin = vectors[,y1] - se[,y1], ymax = vectors[,y1] + se[,y1],
                        xmin = vectors[,x1] - se[,x1], xmax = vectors[,x1] + se[,x1])
  vectors <- cbind(vectors, minmax)
  xname <- paste0("Face ", gsub("^.*?_", "" , x1))
  yname <- paste0("Face ", gsub("^.*?_", "" , y1))
  p1   <- ggplot(PCA, aes_string(x = x1, y = y1)) +
    geom_point(data = vectors, aes_string(x = x1, y = y1, color = "Pop"),
               size = 3) +
    geom_errorbar(data = vectors, aes(ymin = ymin, ymax = ymax, color = Pop), show.legend = F) + 
    geom_errorbarh(data = vectors, aes(xmin = xmin, xmax = xmax, color = Pop), show.legend = F) + 
    geom_segment(data = vectors, 
                 aes_string(x = 0, y = 0,
                            xend = paste(x1, "*1", sep = ""), 
                            yend = paste(y1, "*1", sep = ""),
                            color = "Pop"),
                 alpha = 0.3, size = 2, lineend = "butt", show.legend = F) +
    # y axis line
    geom_segment(x = 0, xend = 0, 
                 y = ylim[1], yend = ylim[2] - (ylim[2]/10),
                 size = 0.5) +
    # x axis line
    geom_segment(y = 0, yend = 0, 
                 x = xlim[1], xend = xlim[2] - (xlim[2]/10),
                 size = 0.5) +
    #ylim(-lims$y, lims$y) +
    #xlim(-lims$x, lims$x) + 
    labs(x = xname, y = yname) +
    theme(legend.title = element_text(size=18)) + #Change legend size 
    theme(legend.text  = element_text(size=18)) + #Change text size
    guides(color = guide_legend(override.aes = list(size = 10))) + #Change legend size
    annotate("text", x = xlim[2] - (xlim[2]/10), y = -xlim[2]/20, label = xname, size = 4.5) +
    annotate("text", x = 0, y = ylim[2], label = yname, size = 4.5) +
    theme_pubr() 
  
  if(is.null(xlim)){
  } else {
    p1 <- p1 + xlim(xlim[1], xlim[2]+0.1)
  }
  if(is.null(ylim)){
  } else {
    p1 <- p1 + ylim(ylim[1], ylim[2]+0.1)
  }
  
  p1 <- ggpar(p1, palette = OkabeIto)
  p1 <- p1 + theme_void(base_size=20)
  return(p1)
}

####FIGURE 1####
p1 <- plotGPCA(MergedDat[[2]], "PC1", "PC2", xlabel = "Genetic PC1", ylabel = "Genetic PC2")
p2 <- plotGPCA(MergedDat[[2]], "PC3", "PC4", xlabel = "Genetic PC3", ylabel = "Genetic PC4")
p3 <- plotGPCA(MergedDat[[1]], "Gen_PC1", "Gen_PC2", xlabel = "Genetic PC1", 
              ylabel = "Genetic PC2", col = "prediction")
p4 <- plotGPCA(MergedDat[[1]], "Gen_PC3", "Gen_PC4", xlabel = "Genetic PC3", 
              ylabel = "Genetic PC4", col = "prediction")

setwd(paste(path, "/Docs/AAPA18/Figures", sep = ""))
pdf("Figure1.pdf", width = 8, height = 5)

ggarrange(p1,p2,p3,p4,
          nrow = 2, ncol = 2,
          common.legend = TRUE, legend = "top", 
          align = "v")

dev.off()

####FIGURE 2####
p1 <- plotFPCA(MergedDat[[1]], "Face_PC1", "Face_PC2", xlim = c(-6,6))
avgstot <- as.data.frame(matrix(0, nrow = 10, ncol = 88))
colnames(avgstot) <- c("Sex", colnames(select(MergedDat[[1]], starts_with("Face"))))
avgstot$Sex <- rep(c("Female", "Male"), 5)
row <- 1
for(i in 1:5){
  avgstot[row,2:88] <- extractRegress(regresstot, "(Intercept)", "estimate")[i,2:88]
  row <- row + 1
  avgstot[row,2:88] <- extractRegress(regresstot, "(Intercept)", "estimate")[i,2:88] + 
                             extractRegress(regresstot, "SexMale", "estimate")[i,2:88]
  row <- row + 1
}

p2 <- plotFPCApop(MergedDat[[1]], MergedDat[[1]]$prediction == "AFR",  "Face_PC1", "Face_PC2", avgstot[1:2,])
p3 <- plotFPCApop(MergedDat[[1]], MergedDat[[1]]$prediction == "AMR",  "Face_PC1", "Face_PC2", avgstot[3:4,])
p4 <- plotFPCApop(MergedDat[[1]], MergedDat[[1]]$prediction == "EAS",  "Face_PC1", "Face_PC2", avgstot[5:6,])
p5 <- plotFPCApop(MergedDat[[1]], MergedDat[[1]]$prediction == "EUR",  "Face_PC1", "Face_PC2", avgstot[7:8,])
p6 <- plotFPCApop(MergedDat[[1]], MergedDat[[1]]$prediction == "SAS",  "Face_PC1", "Face_PC2", avgstot[9:10,])

fr <- ggarrange(p1, common.legend = T, legend = "top")
sr <- ggarrange(p2, p3, p4, p5, p6, 
                ncol = 5, 
                common.legend = T, legend = "none",
                labels = c("AFR", "AMR", "EAS", "EUR", "SAS"))

setwd(paste(path, "/Docs/AAPA18/Figures", sep = ""))
pdf("Figure2.pdf" , width = 8, height = 3.75)
ggarrange(fr, sr, nrow = 2, common.legend = T, 
          legend = "top", heights = c(2,1))
dev.off()

####FIGURE 3####
p1 <- plotsquare(MergedDat[[1]], "Face_PC1", "Face_PC2", sexcoefstot, sexcoefstotse, 
                 xlim = c(-1,3), ylim = c(-1, 2))
p2 <- plotsquare(MergedDat[[1]], "Face_PC3", "Face_PC4", sexcoefstot, sexcoefstotse, 
                 xlim = c(-0.5, 0.7), ylim = c(-0.5, 0.6))
p3 <- plotsquare(MergedDat[[1]], "Face_PC1", "Face_PC2", sexcoefsnon, sexcoefsnonse, 
                 xlim = c(-1,3), ylim = c(-1, 2))
p4 <- plotsquare(MergedDat[[1]], "Face_PC3", "Face_PC4", sexcoefsnon, sexcoefsnonse, 
                 xlim = c(-0.5, 0.7), ylim = c(-0.5, 0.6))
p5 <- plotsquare(MergedDat[[1]], "Face_PC1", "Face_PC2", heightcoefs, heightcoefsse, 
                 xlim = c(-1,3), ylim = c(-1, 2))
p6 <- plotsquare(MergedDat[[1]], "Face_PC3", "Face_PC4", heightcoefs, heightcoefsse, 
                 xlim = c(-0.5, 0.7), ylim = c(-0.5, 0.6))
 
setwd(paste(path, "/Docs/AAPA18/Figures", sep = ""))
pdf("Figure3.pdf", width = 8, height = 8)

ggarrange(p1, p2, p3, p4, p5, p6,
          nrow = 3, ncol = 2,
          common.legend = TRUE, legend = "right", 
          align = "v")

dev.off()

###FACES###
#PCS#
pcacoords           <- data.frame(matrix(0, nrow = nrow(face.means), ncol = 4))
colnames(pcacoords) <- c("PC1 pos", "PC1 neg", "PC2 pos", "PC2 neg")

pcacoords[,1] <- c(6, rep(0, 86)) %*% 
                  t(as.matrix(face.eigenvectors)) + face.means
pcacoords[,2] <- c(-6, rep(0, 86)) %*% 
                  t(as.matrix(face.eigenvectors)) + face.means
pcacoords[,3] <- c(0, 6, rep(0, 85)) %*% 
                   t(as.matrix(face.eigenvectors)) + face.means
pcacoords[,4] <- c(0, -6, rep(0, 85)) %*% 
                   t(as.matrix(face.eigenvectors)) + face.means
setwd(paste(path, "/Docs/AAPA18/Figures/Faces/PCs", sep = ""))
for(i in 1:ncol(pcacoords)){
  filename <- paste0(colnames(pcacoords)[i], ".obj")
  SaveObj(pcacoords[,i], face.facets, filename)
}


#EFFECTS#
mastertot <- lm(as.matrix(matrix) ~ covs$Sex + covs$prediction + covs$Age + covs$BMI)
masternon <- lm(as.matrix(matrix) ~ covs$Sex + covs$Height + covs$prediction + covs$Age + covs$BMI)
weighetav <- MergedDat[[1]] %>% group_by(prediction, Sex) %>% dplyr::select(starts_with("Face")) %>%
             summarise_all(funs(mean(. , na.rm = T))) 
weighetav <- weighetav %>% select(starts_with("Face")) %>% summarise_all(mean)
weighetav <- weighetav %>% select(starts_with("Face")) %>% summarise_all(mean)

fsdcoords     <- data.frame(matrix(0, nrow = nrow(face.means), ncol = 6))
colnames(fsdcoords) <- c("Total pos", "Total neg", "Non-allo pos", "Non-allo neg", "Allo pos", "Allo neg")
fsdcoords[,1] <- (as.matrix(weighetav + mastertot$coefficients[2,] * 1.5 ) %*% 
                            t(as.matrix(face.eigenvectors)) ) + face.means
fsdcoords[,2] <- (as.matrix(weighetav + mastertot$coefficients[2,] * -1.5 ) %*% 
                    t(as.matrix(face.eigenvectors)) ) + face.means
fsdcoords[,3] <- (as.matrix(weighetav + masternon$coefficients[2,] * 1.5 ) %*% 
                    t(as.matrix(face.eigenvectors)) ) + face.means
fsdcoords[,4] <- (as.matrix(weighetav + masternon$coefficients[2,] * -1.5 ) %*% 
                    t(as.matrix(face.eigenvectors)) ) + face.means
fsdcoords[,5] <- (as.matrix(weighetav + masternon$coefficients[3,] * 3 ) %*% 
                    t(as.matrix(face.eigenvectors)) ) + face.means
fsdcoords[,6] <- (as.matrix(weighetav + masternon$coefficients[3,] * -3 ) %*% 
                    t(as.matrix(face.eigenvectors)) ) + face.means

setwd(paste(path, "/Docs/AAPA18/Figures/Faces/Total", sep = ""))
for(i in 1:2){
  filename <- paste0("Face", i, ".obj")
  SaveObj(fsdcoords[,i], face.facets, filename)
}
setwd(paste(path, "/Docs/AAPA18/Figures/Faces/Nonallo", sep = ""))
for(i in 3:4){
  filename <- paste0("Face", i, ".obj")
  SaveObj(fsdcoords[,i], face.facets, filename)
}
setwd(paste(path, "/Docs/AAPA18/Figures/Faces/Allo", sep = ""))
for(i in 5:6){
  filename <- paste0("Face", i, ".obj")
  SaveObj(fsdcoords[,i], face.facets, filename)
}
