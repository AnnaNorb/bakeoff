function [ Ypred ] = crossValidate(m,part,mcmcRepN,postRep,postThin,predN)
partUn = sort(unique(part));
partN = length(partUn);
Ypred = nan(size(m.Y));
for pN = 1:partN
   fprintf('Crossvalidation - partition %d out of %d\n', pN, partN);
   indVal = (part==partUn(pN));
   indTrain = ~indVal;
   %    mT = Hmsc(m.folder, m.traits, m.speciesX, m.phylogeny, m.includeXs, m.includeXv, m.outlierSpecies, m.spatial, m.factorCov);
   mT = m.copy();
   
   YT = m.Y(indTrain,:);
   piCellT = m.piCell(indTrain,:);
   piCellV = m.piCell(indVal,:);
   
   XT = m.X(indTrain,:);
   XV = m.X(indVal,:);
   if m.traits
      TT = m.T;
   else
      TT = [];
   end
   
   mT.setData(YT,m.dist(:,1:2),XT,piCellT,m.xyCell,TT,m.C);
   mT.sampleMCMC(mcmcRepN, false, [], 2);
   mT.setPostThinning(postRep, postThin);
   
   predTestCell = mT.predict(predN, XV, piCellV, m.xyCell, true);
   Ypred(indVal,:) = mean(cat(3, predTestCell{:}),3);
end

end
