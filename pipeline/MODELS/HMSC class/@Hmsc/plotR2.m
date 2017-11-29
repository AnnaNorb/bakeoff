function plotR2(m,R2,showToScreen, saveToFile)

if ~showToScreen
	set(0,'DefaultFigureVisible','off');
end
z=max(m.nr,1);
for level=1:z
	xx='sampling unit';
	if m.nr>0
		xx=m.levelNames{level};
	end
	lR2=R2(:,level);
	exl=isnan(lR2);
	lR2(exl)=[];
	figure;
	prev=mean(m.Y);
	prev(exl)=[];
	scatter(prev,lR2,'black','s');
	% xlim([0 1]);
	xlabel('occurrence');
	ylabel('R2');
	ylim([0 1]);
	title(strcat(xx,': mean R2 over the species=',num2str(mean(lR2),2)));
	if saveToFile
		folder = strcat(m.folder,filesep,'results',filesep);
		if (~isequal(exist(folder, 'dir'),7))
			mkdir(folder);
		end
		print(strcat(folder,'R2-',xx,'.tiff'),'-dtiff');
	end
end

if ~showToScreen
	set(0,'DefaultFigureVisible','on');
end