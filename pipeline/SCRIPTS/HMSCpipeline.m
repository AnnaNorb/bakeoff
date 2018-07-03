clc;
clearvars;

% run twice longer MCMC chains?
MCMC2=false;

cd('D:\HY-data\NORBERG\OneDrive - University of Helsinki\bakeoff\pipeline\MODELS')
%cd('/Users/anorberg/OneDrive - University of Helsinki/bakeoff/pipeline/MODELS')

dsz=3;
jobBird=batch('jobHMSCbirds')
jobButter=batch('jobHMSCbutter')
jobPlant=batch('jobHMSCplant')
jobVegt=batch('jobHMSCveg')
jobTrees=batch('jobHMSCtree')

% pipeine directory
%wdpath=fullfile('...','bakeoff','pipeline');

% run settings
bakeoffSettings=fullfile(wdpath,'SCRIPTS','settingsHMSCmatlab.m');
bakeoff_ssSettings=fullfile(wdpath,'SCRIPTS','settings_ssHMSCmatlab.m');
run(bakeoffSettings)

for dsz=1:3     % data size (1, 2 or 3)
    for s=1:5       % data set

        run(bakeoffSettings)
        run(fullfile(wdpath,'MODELS','fit_predict_hmsc.m'))    

        run(bakeoff_ssSettings)
        run(fullfile(wdpath,'MODELS','fit_predict_hmsc_ss.m'))

    end
end
