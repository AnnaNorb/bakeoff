clc;
clearvars;

% pipeine directory
wdpath=fullfile('...','bakeoff','pipeline');

% run twice longer MCMC chains?
MCMC2=false;

% run settings
bakeoffSettings=fullfile(wdpath,'SCRIPTS','settingsHMSCmatlab.m');
bakeoff_ssSettings=fullfile(wdpath,'SCRIPTS','settings_ssHMSCmatlab.m');
run(bakeoffSettings)

% data size
for dsz=1:2  
    % data set
    for s=1:nsets

        run(bakeoffSettings)
        run(fullfile(wdpath,'MODELS','fit_predict_hmsc.m'))    

        run(bakeoff_ssSettings)
        run(fullfile(wdpath,'MODELS','fit_predict_hmsc_ss.m'))

    end
end