#Navigate to Results Folder
cd ../Results/MergeGeno/MergeSamples/
pwd

#First attempt to merge files. Returns temp1.missnp
./plink --merge-list FirstMergeList.txt --out temp1
#Exclude possibly triallelic snps from samples exported to temp1.missnp from every file
counter=1
for file in Originals/*.bed
do
	inputname=$(echo $file | cut -f 1 -d '.' | tr '/' '\\')
	echo $inputname
	outname="source${counter}_tmp"
	./plink --bfile $inputname --exclude temp1.missnp --make-bed --out $outname
	counter=$((counter + 1))
done

#Merge source files, with the problematic triallelic snps removed
./plink --merge-list FinalMergeList.txt --out Merged
./plink --bfile Merged --geno --indep 50 5 2 --out Clean
./plink --bfile Merged --extract Clean.prune.in --remove Clean.nosex --make-bed --out CleanMerged

#Remove source files
temps=$(ls source*.bed | cut -f 1 -d '.')
for temp in $temps
do
	rm $temp.*	
done

#Copy final file to Merge1000G to be merged with the 1000Genomes samples
export=$(ls CleanMerged.*)
cp $export ../Merge1000G/

#rm -rf Originals