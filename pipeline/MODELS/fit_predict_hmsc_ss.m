% Bakeoff
% Fit single species models for data with explicit spatial structure

cd(hmscPath);

dSz=dSizes(dsz)
set_no=Sets{s}
    
for dTyp=1:3                % interpol, extrapol1, extrapol2

    for typ=1:2             % 1=without LF, 2=with spatial structure as LF

        folder = fullfile(wdpath,'FITS',set_no,'ssHMSC');
        folderData = fullfile(wdpath,'DATA');
        folderPred = fullfile(wdpath,'PREDICTIONS',set_no,'ssHMSC');

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

        compTime=0;

        for sp=1:nsp

            if typ==1
                m = Hmsc(folder, false, false, []);
                m.setData(Y_t(:,sp),'probit',X_t,[],[],[],[]);
            end
            if typ==2
                piCell = cellfun(@num2str, num2cell((1:size(Y_t,1))'), 'UniformOutput', false);
                xyCell = [piCell, num2cell(S_t)];
                m = Hmsc(folder, false, false, [true]);
                m.setData(Y_t(:,sp),'probit',X_t,piCell,{xyCell},[],[]);
            end

            m.setPriorsDefault();
            covScaleFlag=ones(1,m.nc);
            covScaleFlag(1)=2;
            m.setCovScaling(covScaleFlag);

            m.setMCMCOptions(niters, thin1);
            m.setMCMCAdapt([nadaptrounds,0], true);
            m.setMCMCSaveOptions(MCMCsaveRam, MCMCsaveFile);

            filebody=strcat('sp',num2str(sp),'_',num2str(set_no),'_hmsc',num2str(typ),'_d',num2str(dTyp),'_',num2str(dSz));
            filebodyPred=strcat('sp',num2str(sp),'_',num2str(set_no),'_pred_hmsc',num2str(typ),'_d',num2str(dTyp),'_',num2str(dSz));
            filebodyPredCsv=strcat('preds_ss_',num2str(set_no),'_hmsc',num2str(typ),'_d',num2str(dTyp),'_',num2str(dSz));
            filebodyTime=strcat('compTime_',num2str(set_no),'_hmsc',num2str(typ),'_',num2str(dSz));
            if MCMC2
                filebody=strcat(filebody,'_MCMC2');
                filebodyPred=strcat(filebodyPred,'_MCMC2');
                filebodyPredCsv=strcat(filebodyPredCsv,'_MCMC2');
                filebodyTime=strcat(filebodyTime,'_MCMC2');
            end

            try
                tic;
                m.sampleMCMC(nrounds, false, [], 3);
                compT=toc;
            catch
                %fprintf(strcat('MCMC sampling failed for sp', num2str(sp)));
                %save(fullfile(folder,strcat('sp',num2str(sp),'model',num2str(typ),'_d',num2str(dTyp),'_',num2str(dSz),'_NULL','.mat')),'m');

                % sovitetaan pelkka intercept malli
                m.setData(m.Y,'probit',m.X(:,1),m.piCell,m.xyCell,[],[]);
                m.setPriorsDefault();
                covScaleFlag=ones(1,m.nc);
                covScaleFlag(1)=2;
                m.setCovScaling(covScaleFlag);
                m.setMCMCOptions(niters, thin1);
                m.setMCMCAdapt([nadaptrounds,0], true);
                m.setMCMCSaveOptions(MCMCsaveRam, MCMCsaveFile);
                m.sampleMCMC(nrounds, false, [], 3);

                m.setPostThinning(MCMCcut:m.repN, thin2);
                save(fullfile(folder,strcat(filebody,'.mat')),'m');
                m.postRamClear();
                load(fullfile(folder,strcat(filebody,'.mat')));

                if typ==1
                    predList = m.predict(predN, X_v(:,1), [], [], false);
                end
                if typ==2
                    piCell = cellfun(@num2str, num2cell((1:(m.ny+size(Y_v,1)))'), 'UniformOutput', false);
                    xyCell = [piCell, num2cell([S_t;S_v])];
                    predList = m.predict(predN, [m.X;X_v(:,1)], piCell, {xyCell}, false);
                end
                save(fullfile(folderPred,strcat(filebodyPred,'.mat')),'predList');
                compTime=compTime+compT;

                continue;
            end

            m.setPostThinning(MCMCcut:m.repN, thin2);

            save(fullfile(folder,strcat(filebody,'.mat')),'m');

            m.postRamClear();
            load(fullfile(folder,strcat(filebody,'.mat')));

            if typ==1
                predList = m.predict(predN, X_v, [], [], false);
            end
            if typ==2
                piCell = cellfun(@num2str, num2cell((1:(m.ny+size(Y_v,1)))'), 'UniformOutput', false);
                xyCell = [piCell, num2cell([S_t;S_v])];
                predList = m.predict(predN, [m.X;X_v], piCell, {xyCell}, false);
            end

            save(fullfile(folderPred,strcat(filebodyPred,'.mat')),'predList');
            compTime=compTime+compT;
        end

        if dTyp==1
            save(fullfile(folder,strcat(filebodyTime,'.mat')),'compTime');
        end
    end
end

% modify predictions to .csv
cd(hmscPath);
dSz=dSizes(dsz)
set_no=Sets{s}
    
        for dTyp=1:3
            
            for typ=1:2

                folder = fullfile(wdpath,'FITS',set_no,'ssHMSC');
                folderData = fullfile(wdpath,'DATA');
                folderPred = fullfile(wdpath,'PREDICTIONS',set_no,'ssHMSC');

                file=fullfile(folderData,strcat('Yv_', num2str(dTyp), '_', num2str(set_no),'.csv'));
                Y_v=importdata(file);
                
                % subsample of species
                file=fullfile(folderData,strcat('spSel_',num2str(set_no),'.csv'));
                spSel=importdata(file);

                Y_v=Y_v(:,spSel);

                nsp=size(Y_v,2);

                predsM=[];

                filebodyPredCsv=strcat('preds_ss_',num2str(set_no),'_hmsc',num2str(typ),'_d',num2str(dTyp),'_',num2str(dSz));

                if MCMC2
                    filebodyPredCsv=strcat(filebodyPredCsv,'_MCMC2');
                end

                for sp=1:nsp
                    filebodyPred=strcat('sp',num2str(sp),'_',num2str(set_no),'_pred_hmsc',num2str(typ),'_d',num2str(dTyp),'_',num2str(dSz));
                    if MCMC2
                        filebodyPred=strcat(filebodyPred,'_MCMC2');
                    end
                    load(fullfile(folderPred,strcat(filebodyPred,'.mat')),'predList');
                    predsM=[predsM,cell2mat(predList)];
                end

                csvwrite(fullfile(folderPred, strcat(filebodyPredCsv,'.csv')),predsM);

            end
        end