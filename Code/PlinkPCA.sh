#Navigate to Results folder
cd ../Results/MergeGeno/Merge1000G/
pwd

#We'll generate a PCA using only the 1000G samples, and proyecting ours
outpath="$(cd ../../GenClustering ;pwd)"
./plink.exe --bfile Merge1000Gsamples --pca 50 --pca-cluster-names 0 --within 1000Gmerged.fam --out Clus
#Moving output to new folder
files=$(ls Clus.*)
mv $files "$outpath"
