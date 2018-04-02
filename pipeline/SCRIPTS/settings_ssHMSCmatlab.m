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
nrounds=6;
thin2=20;
MCMCcut=3;

if MCMC2
    niters=200;
    thin1=20;
    nadaptrounds=3;
    nrounds=12;
    thin2=40;
    MCMCcut=6;
end

MCMCsaveRam=true;
MCMCsaveFile=false;

%data & prediction settings
Sets = {'birds','butterfly','plant','trees','vegetation'};
dSizes=[150 300 600];
nsets=size(Sets,2);
predN = 100;


