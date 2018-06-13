#!/bin/bash

#This script will phase genotypes using IMPUTE2 and the 1000G phase 3 as reference samples.
#It will divide each chromosome in chunks of 7Mb, containing at least 200 SNPs, 
#in which case the interval will increase by 1Mb until reaching 200 SNPs
#Note that this script should be used in the Penn State cluster
#Remember to have your unphased genotypes in the ~/work/phasing directory, divided by chromosome
#Finally, run the following script as follows:
#cd ~/work/phasing
#chmod +x 2018-05-PhasingGenos.sh
#./2018-05-PhasingGenos.sh > 2018-05-PhasingGenos.log 2>&1 &

#Move to work/phasing directory
cd ~/work/phasing

#Download IMPUTEv2 static version
wget -q https://mathgen.stats.ox.ac.uk/impute/impute_v2.3.2_x86_64_static.tgz
tar -zxvf impute_v2.3.2_x86_64_static.tgz

mv impute_v2.3.2_x86_64_static/impute2 ~/work/phasing/impute2

#Download Genetic map and reference samples for hg 19 coordinates in scratch 
cd ~/scratch
if [ ! -f 1000GP_Phase3.tgz ]; then
	wget -q https://mathgen.stats.ox.ac.uk/impute/1000GP_Phase3.tgz
	tar -zxvf 1000GP_Phase3.tgz
fi

#For chr 1 to 22 create the pbs script and submit it to the cluster
cd ~/work/phasing
for i in {1..22}
do
	echo "Starting chr${i}..."
	mkdir Chr_${i}
	echo "Runnning gunzip..."
	gunzip ~/scratch/1000GP_Phase3/1000GP_Phase3_chr${i}.hap.gz
	gunzip ~/scratch/1000GP_Phase3/1000GP_Phase3_chr${i}.legend.gz

	echo "Getting initial variables ready..."
	max=`awk 'BEGIN{a=   0}{if ($3>0+a) a=$3} END{print a}' Cleaned_split_${i}.gen` #Max position number in the chromosome
	increment=7000000 #Increment of 7Mb
	start=1
	end=7000000
	count=1

	#While starting value is less than the max position number, continue in chunks
	while (( start < max ))
	do
		#Counting how many SNPs in start to end interval
		countinterval=`awk -v start="$start" -v end="$end" '($3>start && $3<end){ ++count } END{ print count }' Cleaned_split_${i}.gen`
		
		#While there are less than 200 SNPs, end value will increase in 1Mb. Except when end value is greater than max, in which case there are no more SNPs left
		while (( countinterval < 200 && end < max))
		do
			end=$(( $end + 1000000 ))
			countinterval=`awk -v start="$start" -v end="$end" '($3>start && $3<end){ ++count } END{ print count }' Cleaned_split_${i}.gen`
		done

		#Creating job requesting 1 processor in 1 node, with 8gb of memory, and a walltime of 5 hours
		echo "Creating pbs file..."
		echo "#!/bin/bash
#PBS -l nodes=1:ppn=1
#PBS -l walltime=05:00:00
#PBS -l pmem=8gb
#PBS -A jlt22_b_g_sc_default
#PBS -j oe

#Moving to phasing directory
cd ~/work/phasing

#Phasing command
./impute2 -phase \
-m ~/scratch/1000GP_Phase3/genetic_map_chr${i}_combined_b37.txt \
-h ~/scratch/1000GP_Phase3/1000GP_Phase3_chr${i}.hap \
-l ~/scratch/1000GP_Phase3/1000GP_Phase3_chr${i}.legend \
-g Cleaned_split_${i}.gen \
-int $start $end \
-Ne 20000 \
-os 2 \
-allow_large_regions \
-o Chr_${i}/Cleaned_phased_chr${i}_chunk${count}.impute2" >> Chr_${i}/phasing_chr${i}_chunk${count}.pbs
		
		echo "Submitting job..."
		qsub Chr_${i}/phasing_chr${i}_chunk${count}.pbs

		echo "Sliding chromosome window..."
		start=$(( $end + 1 ))
		end=$(( $end + $increment ))
		count=$(( $count + 1 ))

		sleep 5m
	done

	echo "Waiting for chromosome to finish..."
	sleep 2h
	echo "Concatenating chunks..."
	cat Chr_${i}/Cleaned_phased_chr${i}_chunk*.impute2_haps > Cleaned_phased_chr${i}.impute2_haps
	echo -e "Finish chr${i}...\n"
	sleep 30s
done
