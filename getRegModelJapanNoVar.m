function [ output_args ] = getRegModelJapanNoVar( A, C, Q, R, INITX, INITV, trainRawProg)
%GETREGMODEL Summary of this function goes here
%   Detailed explanation goes here
[FILTEREDX]=kalman_filterpatients(trainRawProg(2:end,:), A, C, Q, R, INITX, INITV);
trainFiltProg=trainRawProg;
trainFiltProg(2:end,3)=FILTEREDX(:,1);
numPat=size(trainFiltProg,1)-1;
x=[];
y=[];
for i=2:numPat+1
    x=[x; [trainFiltProg{i,3}' trainFiltProg{i,4}' trainFiltProg{i,5}' trainFiltProg{i,6}' trainFiltProg{i,7}'  trainFiltProg{i,8}' trainFiltProg{i,9}' trainFiltProg{i,10}' trainFiltProg{i,11}'] ];
    y=[y; trainFiltProg{i,12}'];
end

    csvwrite([pwd '/kfregression/forReg.csv'],[x y]);

try
    system('/Library/Frameworks/R.framework/Versions/3.2/Resources/bin/Rscript regJPnovar.R'); %for MAC OSX
catch
    
end

try
    system('"C:\Users\liuxiang\Documents\R\R-3.3.2\bin\x64\Rscript" regJPnovar.R');
catch
end
try
    system('C:\VApps\R\R-3.3.1\bin\x64\Rscript regJP.R');
catch
end


end

