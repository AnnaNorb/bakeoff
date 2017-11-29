function genSpatialLocations(m, spatDim)
if length(spatDim) ~= m.nr
	error('HMSC: Length of vector with spatial dimentions for random facors must be eaual to number of random factors');
end
m.spatDim = spatDim;

xy = cell(1, m.nr);
alphap = cell(1, m.nr);
alpha2 = cell(1, m.nr);

for i=1:m.nr
	alphap1 = [];
	alphaw1 = [];
	if m.spatial(i)
		xy1=rand(m.np(i), spatDim(i));
		xy{i} = xy1;
		alphap1 = 0:0.01:1;
		alphaw1 = ones(1,length(alphap1));
		alphaw1 = 0.5*alphaw1/(sum(alphaw1)-1);
		alphaw(1) = 0.5;
	end
	alphap{i} = alphap1;
	alphaw{i} = alphaw1;
end

m.setSpatialLocations(xy)
m.setPriorsAlpha(alphap, alphaw)

end