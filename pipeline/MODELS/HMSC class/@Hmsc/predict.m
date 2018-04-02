function Y = predict(m, n, X, piCell, xyCell, expected)

res = cell(1, n);
postSamInd = randi([1, m.postSamN],1,n);

piN = nan(size(piCell));
piKeyN = cell(1, m.nr);
piMapN = cell(1, m.nr);
xy = cell(1, m.nr);
for r=1:m.nr
	piKey = m.piKey{r};
	keySet = unique(piCell(:,r));
	exInd = m.piMap{r}.isKey(keySet);
	if any(~exInd)
		keySetN = keySet(~exInd);
		piKeyN{r} = [piKey; keySetN];
		newMap = containers.Map(keySetN, (length(piKey)+1):length(piKeyN{r}) );
		piMapN{r} = [m.piMap{r}; newMap];
	else
		piKeyN{r} = piKey;
		piMapN{r} =  m.piMap{r};
	end
	piN(:,r) = cell2mat(piMapN{r}.values( piCell(:,r)) );
	if m.spatial(r)
		piKey = piKeyN{r};
		xyKey = xyCell{r}(:,1);
		xyMap = containers.Map(xyKey,1:length(xyKey));
		if ~all(xyMap.isKey(piKey))
			error('HMSC: some units, defined at level %d were not given spatial coordinates\n', r);
		end
		ind = cell2mat( xyMap.values(piKey) );
		xy1 = xyCell{r}(ind, 2:size(xyCell{r}, 2));
		xy1 = cell2mat(xy1);
		xy{r} = xy1;
	end
end
pi = piN;

dis = cell(1, m.nr);
for i = 1:m.nr
	newPiInd = ~ismember(pi(:,i), m.pi(:,i));
	pi1 = pi(newPiInd, i);
	newPiN = length( unique(pi1) );
	di = [];
	if m.spatial(i) && newPiN > 0
		di = zeros(m.np(i)+newPiN);
		xy1 = xy{i};
		for j = 1:m.spatDim(i)
			xx = repmat(xy1(:,j), 1, m.np(i)+newPiN);
			dx = xx-xx';
			di = di+dx.^2;
		end
	end
	di = sqrt(di);
	dis{i} = di;
end

for rN = 1:n
	%fprintf('Calculating prediction %d\n', rN);
	p = m.postSamVec(postSamInd(rN));
	if m.speciesX
		ny = size(X{1}, 1);
		Ez = zeros(ny, m.ns);
		for i = 1:m.ns
			Ez(:,i) = X{i}*p.beta(:,i);
		end
	else
		ny = size(X, 1);
		Ez = X*p.beta;
	end
	
	for i = 1:m.nr
		etaM = p.eta{i};
		lambda1 = p.lambda{i};
		newPiInd = ~ismember(pi(:,i), m.pi(:,i));
		pi1 = pi(newPiInd, i);
		newPiN = length( unique(pi1) );
		if m.spatial(i) && newPiN > 0
			di = dis{i};
			alphaInd = p.alpha{i};
			alphapw = m.alphapw{i};
			etaN = zeros(newPiN, p.nf(i));
			for j = 1:p.nf(i)
				alpha = alphapw(alphaInd(j), 1);
				if alpha > 0
					di12 = di(1:m.np(i), m.np(i)+(1:newPiN));
					di22 = di(m.np(i)+(1:newPiN), m.np(i)+(1:newPiN));
					W12 = exp(-di12/alpha);
					W22 = exp(-di22/alpha);
					iW11 = m.iWg{i}(:,:,alphaInd(j));
					muN = W12' * iW11 * etaM(:,j);
					WN = W22 - W12' * iW11 * W12;
					WN = (WN+WN')/2;
					etaN(:, j) = mvnrnd(muN, WN);
				else
					etaN(:, j) = normrnd(0,1,[newPiN,1]);
				end
			end
		else
			etaN = normrnd(0,1,[newPiN,p.nf(i)]);
		end
		eta = [etaM; etaN];
		if m.factorCov(i)
			for k = 1:m.ncr(i)
				Xreta = diag(Xr{i}(:,k))*eta;
				Ez = Ez + Xreta(pi(:,i),:)*lambda1(:,:,k);
			end
		else
			Ez = Ez + eta(pi(:,i),:)*lambda1;
		end
	end
	
	if m.includeXs
		Xel = Xs*p.etas*p.lambdas;
		Ez = Ez+Xel;
	end
	
	z = Ez;
	if expected == false
		eps = zeros(ny, m.ns);
		for i = 1:ny
			eps(i,:) = normrnd(zeros(1,m.ns), diag(p.sigma)' );
		end
		mult=ones(ny, m.ns);
		for i = 1:m.ns
			if m.dist(i,3) == 1
				mult(:,i) = max(Ez(:,i),1).^m.dist(i,4);
			end
			if m.dist(i,3) == 2
				mult(:,i) = exp(Ez(:,i)).^m.dist(i,4);
			end
		end
		z = z + mult.*eps;
	end
	Y = z;
	
	for j = 1:m.ns
		if(m.dist(j,1) == 2)
			if expected
				Y(:,j) = normcdf(z(:,j));
			else
				Y(:,j) = z(:,j)>0;
			end
		end
		if(m.dist(j,1) == 3)
			if expected
				Y(:,j) = exp(z(:,j));
			else
				Y(:,j) = floor(exp(z(:,j)));
			end
		end
		if(m.dist(j,1) == 4)
			if expected
				Y(:,j) = max(0,z(:,j));
			else
				Y(:,j) = max(0,floor(z(:,j)));
			end
			Y(:,j) = max(0,floor(z(:,j)));
		end
	end
	res{rN} = Y;
end
Y = res;

end