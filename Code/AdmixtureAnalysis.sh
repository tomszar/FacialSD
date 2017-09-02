cd ../Results/Admixture/
pwd

for i in {2..10}
do
    ./admixture --cv ../MergeGeno/Merge1000G/Merge1000Gsamples.bed $i | tee log${i}.out
done
