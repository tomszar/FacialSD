#This script will download, and merge the 1000Genomes files with your samples
#Be sure to have your samples ready, as well as the list of snps from those samples

#First lets see whether the 1000Genomes needs to be downloaded
read -p "Do you want to download the 1000Genomes data (y/n)?: " answer
case ${answer:0:1} in
    y|Y )
        if [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]
        then
            cd ~/Desktop/
        elif [ "$(expr substr $(uname -s) 1 6)" == "CYGWIN" ]
        then
            if [ "$(whoami)" == "tomas" ]
            then
                cd /cygdrive/c/Users/tomas/Desktop
            elif [ "$(whoami)" == "tug156" ]
            then
                cd /cygdrive/c/Users/tzarzar/Desktop
            fi
        fi
        mkdir 1000Genomes
        cd 1000Genomes/
        wget -r ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20130502/*
        #The code above should download the whole fodler to the 1000Genomes folder
        #cd ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20130502/
    ;;
    * )
        read -p "Then paste the path to the folder with the 1000G files (path/to/folder): " path1000G
        cd $path1000G
        wget -c -N ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20130502/
    ;;
esac

read -p "Please indicate the full path to the file with the subset of snps (path/to/file.txt): " snps
cp $snps snplist.txt

echo "Now I'll keep only the snps in the snplist for every chromosome and merge them all"

for file in *chr*.gz
do
    outname=$echo$(echo $file | cut -d '.' -f 2)
    vcftools --gzvcf $file --snps snplist.txt --recode --out $outname
done

echo "Now I'll concatenate the vcf files, from chr1 to chr22"

inputnames=$(ls chr[0-9]*.recode.vcf)
vcf-concat $inputnames | gzip -c > allmerged.vcf.gz

echo "Now will convert the vcf file into a plink (bed, bim, fam) file"
read -p "Do you need to download plink (y/n)?: " answer
case ${answer:0:1} in
    y|Y )
        wget https://www.cog-genomics.org/static/bin/plink170815/plink_linux_x86_64.zip 
        unzip -j plink_linux_x86_64.zip plink
        rm plink_linux_x86_64.zip
    ;;
    * )
    ;;
esac
./plink --vcf allmerged.vcf.gz --make-bed --out allmerged

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
outpath=$echo$(echo $DIR | cut -d '/' -f 1-8)/Results/MergeGeno
files=$(ls allmerged.*)
cp $files $outpath

echo "Now I'll merge the 1000Genomes data with your samples"
cd $outpath
read -p "Do you need to download plink (y/n)?: " answer
case ${answer:0:1} in
    y|Y )
        wget https://www.cog-genomics.org/static/bin/plink170815/plink_linux_x86_64.zip 
        unzip -j plink_linux_x86_64.zip plink
        rm plink_linux_x86_64.zip
    ;;
    * )
    ;;
esac
./plink --bfile CleanMerged --bmerge allmerged --make-bed --out All1000G
#Remove two triallelic snps 
./plink --bfile CleanMerged --exclude All1000G-merge.missnp --make-bed --out CleanMerged
./plink --bfile allmerged --exclude All1000G-merge.missnp --make-bed --out allmerged
#Try again
./plink --bfile CleanMerged --bmerge allmerged --make-bed --out All1000G
