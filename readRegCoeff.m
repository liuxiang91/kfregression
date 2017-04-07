function o= readRegCoeff()


    out=textread(fullfile([pwd '/kfregression'] ,'regCoeff.csv'), '%s', 'whitespace',',');

nums = out(4:2:end);
o=[];
for i = nums
    o=[o str2double(i)];
end
end