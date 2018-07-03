clc;
clearvars;

% pipeine directory
wdpath=fullfile('...','bakeoff','pipeline');

% run settings
bakeoffSettings=fullfile(wdpath,'SCRIPTS','settingsHMSCmatlab.m');
bakeoff_ssSettings=fullfile(wdpath,'SCRIPTS','settings_ssHMSCmatlab.m');
run(bakeoffSettings)

% run twice longer MCMC chains?
MCMC2=false;

for dsz=1:3     % data size (1, 2 or 3)
    for s=1:5       % data set

        run(bakeoffSettings)
        run(fullfile(wdpath,'MODELS','fit_predict_hmsc.m'))    

        run(bakeoff_ssSettings)
        run(fullfile(wdpath,'MODELS','fit_predict_hmsc_ss.m'))

    end
end
