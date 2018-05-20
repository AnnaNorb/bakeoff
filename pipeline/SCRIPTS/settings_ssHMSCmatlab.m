%clear workspace and set paths
RC = 0;
clearvars -except RC bakeoffSettings bakeoff_ssSettings s dsz wdpath nsets MCMC2;
RC = RC + 7;
rng(RC);
warning('off','all');

hmscPath=fullfile(wdpath,'MODELS','HMSC class');

%sampling settings
niters=100;
thin1=10;
nadaptrounds=3;
nrounds=9;
thin2=30;
MCMCcut=6;

if MCMC2
    niters=niters*2;
    thin1=thin1*2;
    nadaptrounds=3;
    nrounds=nrounds*2;
    thin2=thin2*2;
    MCMCcut=MCMCcut*2;
end

MCMCsaveRam=true;
MCMCsaveFile=false;

%data & prediction settings
Sets = {'birds','butterfly','plant','trees','vegetation'};
dSizes=[150 300 600];
nsets=size(Sets,2);
predN = 100;


