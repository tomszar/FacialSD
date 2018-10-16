%This script will run a Generalized Procrustes Analysis
%from the GPA function
%Adding code
addpath ('/home/tomas/Documents/Research/FacialSD/Code')
%Add pkg
pkg load statistics
pkg load io
pkg load geometry
%Get folders
folder.databases = '/home/tomas/Documents/Research/FacialSD/DataBases';
folder.save      = '/home/tomas/Documents/Research/FacialSD/Results/FacePCA';
cd(folder.databases);
load('landmark_matrix.mat');
load('landmark_ids.mat');
load('landmark_facets.mat');
%Loading IDs and groups from pop_struct 
[ids, groups] = textread ('mclust_class.csv', '%s %f', 'delimiter' , ',');

%Retain IDs from pop structure analysis into shape analysis
%First, remove the string 'PSU' from the landmark_ids file, and remove the starting 0
landmark_ids = erase(landmark_ids, 'PSU');
landmark_ids = regexprep(landmark_ids,'^0*','');
%Then, set the intersection
[~ , keep] = intersect(landmark_ids, ids);

%Keep those only in the landmark_matrix
landmark_matrix = landmark_matrix(keep,:);
landmark_ids = landmark_ids(keep,:);

[shape_matrix, cs] = GPA(landmark_matrix, 1);
clear landmark_matrix;

%PCA (Run this with random equal samples from each group)
%Count group members
[C,ia,ic] = unique(groups);
group_counts  = accumarray(ic,1);
group_counts

%We'll get a random sample of 160 for each group to run the PCA
rand_ids = cell();
for i = 1:7
  t = randsample(ids(groups==i), 160);
  rand_ids = {rand_ids{:} t{:}};
endfor
[~ , keep] = intersect(landmark_ids, rand_ids);
sample_shape_matrix = shape_matrix(keep,:);
[V,S,~,mean_eigenvals,index] = fastPCA(sample_shape_matrix, 100, 1);
#Looking at the number of relevant PCs
plot(diag(S),'bo-');
hold on;
plot(mean_eigenvals,'ro-');
hold off;
%Getting PCA scores for whole sample
mu = mean(shape_matrix);
Xm = bsxfun(@minus, shape_matrix, mu);
score = Xm*V(:,1:index); #Keeping only relevant PCs
%Eigenvalue is (diag(S).^2)/(n-1)

%Testing accuracy of PCA
%Calculating euclidean distance between original and PCA transformed faces
distances = distancePoints(shape_matrix, fromPCA, 'diag');
distances_rand = distancePoints(shape_matrix, fromPCA(randperm(size(fromPCA,1)), :), 'diag');
hist(distances,'bo-');
hold on;
hist(distances_rand,'ro-');
hold off;

%Saving files
cd(folder.save)
cell2csv("landmark_ids.csv", landmark_ids)
csvwrite("means.csv", mu') %Perhaps the exported mu should be the one used in fastPCA?
csvwrite("cs.csv", cs') 
csvwrite("scores.csv", score) 
csvwrite("eigenvalues.csv", diag(S) ) 
csvwrite("eigenvectors.csv", V(:,1:index) ) 
csvwrite("facets.csv", landmark_facets ) 
