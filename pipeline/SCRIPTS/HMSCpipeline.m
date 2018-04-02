clc;
clearvars;

% cd('D:\HY-data\NORBERG\OneDrive - University of Helsinki\bakeoff\pipeline\MODELS')
% jobPlant=batch('jobHMSCplant')
% jobBird=batch('jobHMSCbirds')
% jobButter=batch('jobHMSCbutter')
% jobTrees=batch('jobHMSCtree')

% pipeine directory
%wdpath=fullfile('...','bakeoff','pipeline');
wdpath=fullfile('D:\HY-data\NORBERG\OneDrive - University of Helsinki\bakeoff','pipeline');
%wdpath=fullfile('/Users/anorberg/OneDrive - University of Helsinki/bakeoff/pipeline')

% run twice longer MCMC chains?
MCMC2=true;

% run settings
bakeoffSettings=fullfile(wdpath,'SCRIPTS','settingsHMSCmatlab.m');
bakeoff_ssSettings=fullfile(wdpath,'SCRIPTS','settings_ssHMSCmatlab.m');
run(bakeoffSettings)

% data size
dsz=1;  %1or2  
% data set
s=1 %1:nsets

run(bakeoffSettings)
run(fullfile(wdpath,'MODELS','fit_predict_hmsc.m'))    

run(bakeoff_ssSettings)
run(fullfile(wdpath,'MODELS','fit_predict_hmsc_ss.m'))

