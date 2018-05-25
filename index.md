---
title: Evolution of Human Facial Sexual Dimorphism
output: html_document
---

## Summary

This project aims to test for differences in patterns of facial sexual dimorphism across populations.
The main purpose is to test whether because of morphological divergence, effects of sex on facial morphology have changed as well.
Therefore, the first step is to estimate populations based on genetic data.
Next, we will ask whether different populations show differences in facial morphology (using the whole sample, and divided by sex as well).
Finally, between those populations showing differences in facial morphology, we will test whether patterns of sexual dimorphism are shared or not.
The workflow can be summarized as follows:

- Obtain the 3D landmark data from participants' faces and run a standard geometric morphometrics approach, that involves generalized procrustes analysis, PCA, and retention of PCs that explain 90% of the variance.
- Estimate facial sexual dimorphism (FSD) using multivariate linear regression of face shape on sex
- Decompose FSD in allometric and non-allometric components.
- Generate different groupings based on ancestry analysis from DNA.
- Compare pattern of FSD between populations.

## Content

The links below will take you to the step-by-step analysis.
Each link is in sequential order, many of them building on the output from the previous scripts.
The general idea is first to generate the corresponding PCAs for facial and genetic data, and merge them.

- Here you can see the generation of the [Face PCA](https://nbviewer.jupyter.org/github/tomszar/FacialSD/blob/master/Code/2017-11-FacePCA.ipynb). 
- [Here](https://nbviewer.jupyter.org/github/tomszar/FacialSD/blob/master/Code/2018-05-MergeGenotypes.ipynb) you can see the merging of genotype data with the reference samples. 
Note that the merging of the reference samples was done [elsewhere](https://tomszar.github.io/HGDP_1000G_Merge/).
- Here you can see the clustering using the [Genetic PCA](Code/2018-03-GeneticPCA.html). 
**Note:** the genetic PCA was made in plink, using only the reference samples (1000G and HGDP) to create the PCA, and then projecting our samples.
- Here you can see some [descriptive statistics](Code/2017-07-DescriptiveStats.html) from the merged data set.
- Here is the [hypothesis testing](Code/2017-07-HTesting.html) on the effect of sex and height on the face, using the whole dataset.
This is also the preliminary analysis on the allometric and non-allometric decomposition.
- Here you can visualize interactively some preliminary [face effects](Code/2017-07-FaceEffects.html) of sexual dimorphism mande in the previous script. 
Remember to open this file in chrome.
- This is the core of the analysis, on the [population comparisons](Code/2017-08-PopComparison.html). 
- Here you can see [FSD across human populations](Code/2018-03-FacePopEffects.html) 