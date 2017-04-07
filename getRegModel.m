function [ output_args ] = getRegModel( A, C, Q, R, INITX, INITV, trainRawProg)
%GETREGMODEL Summary of this function goes here
%   Detailed explanation goes here
global os;
[FILTEREDX]=kalman_filterpatients(trainRawProg(2:end,:), A, C, Q, R, INITX, INITV);
trainFiltProg=trainRawProg;
trainFiltProg(2:end,3)=FILTEREDX(:,1);
numPat=size(trainFiltProg,1)-1;
x=[];
y=[];
for i=2:numPat+1
    x=[x; [(i-1)*ones(size(trainFiltProg{i,3},2),1)  trainFiltProg{i,3}' trainFiltProg{i,4}' trainFiltProg{i,5}' trainFiltProg{i,6}' trainFiltProg{i,7}'] ];
    y=[y; trainFiltProg{i,8}'];
end

csvwrite(fullfile([pwd '/kfregression'] ,'forReg.csv'),[x y]);

try
    system(['Rscript ' pwd '/kfregression/reg_caa.R']); %for MAC OSX
catch
    
end


end

