%clear workspace and set paths
RC = 0;
clearvars -except RC bakeoffSettings bakeoff_ssSettings s dsz wdpath nsets MCMC2;
RC = RC + 7;
if MCMC2
    RC = RC + 2;
end
rng(RC);
warning('off','all');

hmscPath=fullfile(wdpath,'MODELS','HMSC class');

%sampling settings
niters=100;
thin1=10;
nadaptrounds=3;
nrounds=50;
thin2=10;
MCMCcut=41;

MCMCsaveRam=true;
MCMCsaveFile=false;

%data & prediction settings
Sets = {'birds','butterfly','plant','trees','vegetation'};
dSizes=[150 300 600];
nsets=size(Sets,2);
predN = 100;
