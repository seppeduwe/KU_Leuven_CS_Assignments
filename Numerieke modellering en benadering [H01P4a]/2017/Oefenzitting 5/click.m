function [x,y] = click

	% initialiseer figuur

	figure;
	axis([0 1 0 1]);
	axis manual
	title('Click left to draw polyline, click right to terminate')
	hold on;

	% herhaal tot andere dan linkermuisknop ingedrukt
	x = []; y = [];
	while(1)
		[px,py,button] = ginput(1);
		if( button ~= 1 )
			break;
		else
			x = [x px] ; y = [y py];
			if( length(x) > 1 )
				plot(x([end-1 end]),y([end-1 end]),'b-');
			end
			plot(px,py,'+');
		end
	end

	hold off;
