function o= readRegCoeff()


    out=textread([pwd '/regCoeff.csv'], '%s', 'whitespace',',');

nums = out(4:2:end);
o=[];
for i = nums
    o=[o str2double(i)];
end
end