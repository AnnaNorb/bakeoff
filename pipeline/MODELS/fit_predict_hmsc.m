% Bakeoff
% Fit community models for data with explicit spatial structure

cd(hmscPath);

set_no=Sets{s}      % data set
dSz=dSizes(dsz);    % data size
            
for dTyp=1:3                % interpol, extrapol1, extrapol2

    for typ=1:3         % 1=without LF, 2==with LF; 3=with spatial structure as LF

        folder = fullfile(wdpath,'FITS',set_no);
        folderData = fullfile(wdpath,'DATA');
        folderPred = fullfile(wdpath,'PREDICTIONS',set_no);

        %train
        file=fullfile(folderData,strcat('Yt_', num2str(dTyp), '_', num2str(set_no),'.csv'));
        Y_t=importdata(file);
        %file=fullfile(folderData,strcat('Yy_', num2str(set_no),'_',num2str(dTyp),'_',num2str(dSz),'_train.csv'));
        %Y_t=importdata(file);
        %valid
        file=fullfile(folderData,strcat('Yv_', num2str(dTyp), '_', num2str(set_no),'.csv'));
        Y_v=importdata(file);

        %train
        file=fullfile(folderData,strcat('Xt_', num2str(dTyp), '_', num2str(set_no),'.csv'));
        X_t=importdata(file);
        X_t=[ones(size(X_t,1),1),X_t,X_t.^2];
        %valid
        file=fullfile(folderData,strcat('Xv_', num2str(dTyp), '_', num2str(set_no),'.csv'));
        X_v=importdata(file);
        X_v=[ones(size(X_v,1),1),X_v,X_v.^2];

        %train
        file=fullfile(folderData,strcat('St_', num2str(dTyp), '_', num2str(set_no),'.csv'));
        S_t=importdata(file);
        %valid
        file=fullfile(folderData,strcat('Sv_', num2str(dTyp), '_', num2str(set_no),'.csv'));
        S_v=importdata(file);

        Xs_t=[X_t,S_t];
        Xs_v=[X_v,S_v];

        % subsamples of data and species
        file=fullfile(folderData,strcat('siteSamps_',num2str(set_no),'.mat'));
        siteSamps=importdata(file);
        siteSamps=struct2cell(siteSamps);
        samp=siteSamps{dsz}';
        file=fullfile(folderData,strcat('spSel_',num2str(set_no),'.csv'));
        spSel=importdata(file);

        Y_t=Y_t(samp,spSel);
        X_t=X_t(samp,:);
        S_t=S_t(samp,:);
        Xs_t=Xs_t(samp,:);

        nsp=size(Y_t,2);
        nsites=size(Y_t,1);

        if typ==1
            m = Hmsc(folder, false, false, []);
            m.setData(Y_t,'probit',X_t,[],[],[],[]);
        end
        if typ==2
            piCell = cellfun(@num2str, num2cell((1:size(Y_t,1))'), 'UniformOutput', false);
            m = Hmsc(folder, false, false, [false]);
            m.setData(Y_t,'probit',X_t,piCell,[],[],[]);
        end
        if typ==3
            piCell = cellfun(@num2str, num2cell((1:size(Y_t,1))'), 'UniformOutput', false);
            xyCell = [piCell, num2cell(S_t)];
            m = Hmsc(folder, false, false, [true]);
            m.setData(Y_t,'probit',X_t,piCell,{xyCell},[],[]);
        end

    m.setPriorsDefault();
    covScaleFlag=ones(1,m.nc);
    covScaleFlag(1)=2;
    m.setCovScaling(covScaleFlag);

    m.setMCMCOptions(niters, thin1);
    m.setMCMCAdapt([nadaptrounds,0], true);
    m.setMCMCSaveOptions(MCMCsaveRam, MCMCsaveFile);

    tic;
    m.sampleMCMC(nrounds, false, [], 3);
    compTime=toc;

    m.setPostThinning(MCMCcut:m.repN, thin2);
    
    filebody=strcat('hmsc_',num2str(set_no),'_',num2str(typ),'_d',num2str(dTyp),'_',num2str(dSz));
    filebodyTime=strcat('compTime_hmsc_',num2str(set_no),'_',num2str(typ),'_',num2str(dSz));

    if MCMC2
        filebody=strcat(filebody,'_MCMC2');
        filebodyTime=strcat(filebodyTime,'_MCMC2');
    end

    try
        save(fullfile(folder,strcat(filebody,'.mat')),'m');
    catch
        fprintf('File too large, using flag -v7.3')
        save(fullfile(folder,strcat(filebody,'.mat')),'m','-v7.3');
    end
    if dTyp==1
        save(fullfile(folder,strcat(filebodyTime,'.mat')),'compTime');
    end

    m.postRamClear();

    load(fullfile(folder,strcat(filebody,'.mat')));

    if typ==1
        predList = m.predict(predN, X_v, [], [], false);
    end
    if typ==2
        piCell = cellfun(@num2str, num2cell((1:(m.ny+size(Y_v,1)))'), 'UniformOutput', false);
        predList = m.predict(predN, [m.X;X_v], piCell, [], false);
    end
    if typ==3
        piCell = cellfun(@num2str, num2cell((1:(m.ny+size(Y_v,1)))'), 'UniformOutput', false);
        xyCell = [piCell, num2cell([S_t;S_v])];
        predList = m.predict(predN, [m.X;X_v], piCell, {xyCell}, false);
    end

    filebodyPred=strcat('pred_hmsc_',num2str(set_no),'_',num2str(typ),'_d',num2str(dTyp),'_',num2str(dSz));
    filebodyPredCsv=strcat('preds_',num2str(set_no),'_hmsc',num2str(typ),'_d',num2str(dTyp),'_',num2str(dSz));

    if MCMC2
        filebodyPred=strcat(filebodyPred,'_MCMC2');
        filebodyPredCsv=strcat(filebodyPredCsv,'_MCMC2');
    end
    save(fullfile(folderPred,strcat(filebodyPred,'.mat')),'predList');
    predsM=cell2mat(predList);
    csvwrite(fullfile(folderPred,strcat(filebodyPredCsv,'.cvs')),predsM);

    end
end
