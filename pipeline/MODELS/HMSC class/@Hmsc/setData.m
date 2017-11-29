function setData(m, Y, dist, X, pi, xy, T, C)

[ny, ns] = size(Y);
if m.speciesX
	nc = size(X{1}, 2);
else
	nc = size(X, 2);
end
setDim(m, ny, ns, nc);
m.setY(Y);
m.setX(X);
m.setDist(dist);
m.setPi(pi);
if ~isempty(T)
	setTraits(m, T);
end
if ~isempty(C)
	setPhylogeny(m, C);
end
if ~isempty(xy)
	setSpatialLocations(m, xy)
end

end