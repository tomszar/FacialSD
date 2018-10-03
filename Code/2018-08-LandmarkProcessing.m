%This script will run a Generalized Procrustes Analysis
%from the GPA function
%Adding code
addpath ('/home/tomas/Documents/Research/FacialSD/Code')
folder.databases = '/home/tomas/Documents/Research/FacialSD/DataBases';
folder.save      = '/home/tomas/Documents/Research/FacialSD/Results/ShapeCoordinates';
cd(folder.databases);
load('landmark_matrix.mat');
load('landmark_ids.mat');
load('landmark_facets.mat');
%Loading IDs from pop_struct 
idstoretain = textread ('IDsDensePheno.txt', '%s');

%Retain IDs from pop structure analysis into shape analysis
%First, remove the string 'PSU' from the landmark_ids file, and remove the starting 0
landmark_ids = erase(landmark_ids, 'PSU');
landmark_ids = regexprep(landmark_ids,'^0*','');
%Then, set the intersection
[~ , keep] = intersect(landmark_ids, idstoretain);

%Keep those only in the landmark_matrix
landmark_matrix = landmark_matrix(keep,:);

[shape_matrix, cs] = GPA(landmark_matrix, 1);
clear landmark_matrix;

%PCA (Run this with random equal samples from each group)
%mu = mean(shape_matrix);
%Xm = bsxfun(@minus, shape_matrix, mu);
%C = cov(Xm);
%clear mu Xm;
%[u, s, v] = svd(C, 'econ'); 

cd(folder.save)
save shape_matrix.mat shape_matrix
save cs.mat cs
