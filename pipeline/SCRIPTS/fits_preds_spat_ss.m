% Bakeoff
% Fit single species models for data with explicit spatial structure

cd(hmscPath);

Sets = {'birds','butterfly','plant','trees','vegetation'};
dSizes=[300 600];
nsets=size(Sets,2);
predN = 100;

dSz=dSizes(dsz)
set_no=Sets{s}
    
        for dTyp=1:3                % interpol, extrapol1, extrapol2
            
            for typ=1:4             % 1=without LF, 2=wihtout LF, with space as covariates, 3=with LF; 4=with spatial structure as LF

                folder = fullfile(wdpath,'FITS/');
                folderData = fullfile(wdpath,'DATA/');
                folderPred = fullfile(wdpath,'PREDICTIONS/');

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
                
                Y_t=Y_t(1:dSz,:);
                Y_v=Y_v(1:dSz,:);
                X_t=X_t(1:dSz,:);
                X_v=X_v(1:dSz,:);
                S_t=S_t(1:dSz,:);
                S_v=S_v(1:dSz,:);
                Xs_t=Xs_t(1:dSz,:);
                Xs_v=Xs_v(1:dSz,:);
                
                nsp=size(Y_t,2);
                nsites=size(Y_t,1);

                compTime=0;

                for sp=1:nsp

                    if typ==1
                        m = Hmsc(folder, false, false, []);
                        m.setData(Y_t(:,sp),'probit',X_t,[],[],[],[]);
                    end
                    if typ==2
                        m = Hmsc(folder, false, false, []);
                        m.setData(Y_t(:,sp),'probit',Xs_t,[],[],[],[]);
                    end
                    if typ==3
                        piCell = cellfun(@num2str, num2cell((1:size(Y_t,1))'), 'UniformOutput', false);
                        m = Hmsc(folder, false, false, [false]);
                        m.setData(Y_t(:,sp),'probit',X_t,piCell,[],[],[]);
                    end
                    if typ==4
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
                        save(fullfile(folder,strcat('sp',num2str(sp),'_',num2str(set_no),'_model',num2str(typ),'_d',num2str(dTyp),'_',num2str(dSz),'.mat')),'m');
                        m.postRamClear();
                        load(fullfile(folder,strcat('sp',num2str(sp),'_',num2str(set_no),'_model',num2str(typ),'_d',num2str(dTyp),'_',num2str(dSz),'.mat')));
                        if typ==1 | typ==2
                            predList = m.predict(predN, X_v(:,1), [], [], false);
                        end
                        if typ==3
                            piCell = cellfun(@num2str, num2cell((m.ny+1:m.ny+size(Y_v,1))'), 'UniformOutput', false);
                            predList = m.predict(predN, X_v(:,1), piCell, [], false);
                        end
                        if typ==4
                            piCell = cellfun(@num2str, num2cell((1:m.ny+size(Y_v,1))'), 'UniformOutput', false);
                            xyCell = [piCell, num2cell([S_t;S_v])];
                            predList = m.predict(predN, [m.X(:,1); X_v(:,1)], piCell, {xyCell}, false);
                        end
                        save(fullfile(folderPred,strcat('sp',num2str(sp),'_',num2str(set_no),'_pred_m',num2str(typ),'_d',num2str(dTyp),'_',num2str(dSz),'.mat')),'predList');
                        compTime=compTime+compT;
                        
                        continue;
                    end
                    
                    m.setPostThinning(MCMCcut:m.repN, thin2);

                    save(fullfile(folder,strcat('sp',num2str(sp),'_',num2str(set_no),'_model',num2str(typ),'_d',num2str(dTyp),'_',num2str(dSz),'.mat')),'m');

                    m.postRamClear();
                    load(fullfile(folder,strcat('sp',num2str(sp),'_',num2str(set_no),'_model',num2str(typ),'_d',num2str(dTyp),'_',num2str(dSz),'.mat')));

                    predN = 100;
                    if typ==1
                        predList = m.predict(predN, X_v, [], [], false);
                    end
                    if typ==2
                        predList = m.predict(predN, Xs_v, [], [], false);
                    end
                    if typ==3
                        piCell = cellfun(@num2str, num2cell((m.ny+1:m.ny+size(Y_v,1))'), 'UniformOutput', false);
                        predList = m.predict(predN, X_v, piCell, [], false);
                    end
                    if typ==4
                        piCell = cellfun(@num2str, num2cell((1:m.ny+size(Y_v,1))'), 'UniformOutput', false);
                        xyCell = [piCell, num2cell([S_t;S_v])];
                        predList = m.predict(predN, [m.X; X_v], piCell, {xyCell}, false);
                    end

                    save(fullfile(folderPred,strcat('sp',num2str(sp),'_',num2str(set_no),'_pred_m',num2str(typ),'_d',num2str(dTyp),'_',num2str(dSz),'.mat')),'predList');
                    compTime=compTime+compT;
                end
              
                if dTyp==1
                    save(fullfile(folder,strcat('compTime_',num2str(set_no),'_m',num2str(typ),'_',num2str(dSz),'.mat')),'compTime');
                end
            end
        end
