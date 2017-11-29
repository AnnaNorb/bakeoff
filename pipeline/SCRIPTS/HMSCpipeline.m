
clearvars;

% pipeine directory
wdpath=fullfile('...','bakeoff','pipeline');

% run settings
bakeoffSettings=fullfile(wdpath,'SCRIPTS','settingsHMSCmatlab.m');

% run the models
for s=1:nsets
    
    for dsz=1:2

        run(fullfile(wdpath,'MODELS','fits_preds_spat.m'))
        run(bakeoffSettings)
        run(fullfile(wdpath,'MODELS','fits_preds_spat_ss.m'))
        run(bakeoffSettings)
        run(fullfile(wdpath,'PREDICT','predmods.m'))
        run(bakeoffSettings)
        run(fullfile(wdpath,'PREDICT','predmods_ss.m'))

    end
end
