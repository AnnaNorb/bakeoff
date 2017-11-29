%clear workspace and set paths
RC = 0;
clearvars -except RC bakeoffSettings s dsz;
RC = RC + 7;
rng(RC);
warning('off','all');

hmscPath=fullfile(wdpath,'MODELS','HMSC class');

%sampling settings
niters=100;
thin1=10;
nadaptrounds=3;
MCMCsaveRam=true;
MCMCsaveFile=false;
nrounds=30;
thin2=10;
MCMCcut=21;

