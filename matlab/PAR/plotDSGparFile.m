% cd ~/ABOS/git/java-ocean-data-delivery/ABOS/

%file = '../data/IMOS_ABOS-SOTS_F_20090928_SOFS_FV01_SOFS-1-2010-PAR-DiscreteGeometries_END-20160413_C-20180423.nc';
file = 'data/IMOS_ABOS-SOTS_F_20100318_SOFS_FV01_SOFS-1-2010-SW-DiscreteGeometries_END-20171101_C-20180604.nc';

varName= 'SW';
station = ncread(file, 'stationIndex');
par = ncread(file, varName);
par_unit = ncreadatt(file, varName, 'units');
par_name = ncreadatt(file, varName, 'name');
parQC = ncread(file, [varName '_quality_code']);

sr = ncread(file, 'cSR');

%time = datetime(ncread(file, 'TIME') + datenum(1950,1,1), 'ConvertFrom', 'datenum');
time = ncread(file, 'TIME') + datenum(1950,1,1);

stationStr = ncread(file, 'station_name');
stationNames = stationStr';
stationNamesStr = string(stationNames);
stationid = split(stationNamesStr,':');

depth = ncread(file, 'NOMINAL_DEPTH');

%figure(1); clf; hold on
%figure(2); clf; hold on

depths = unique(round(depth/10)*10);
keys =  round(depth/10)+1;

colours = 'rgbymcwk';
figI = 0;
deployments = struct;
titles = {};

for i = min(station):max(station)
%     figure(1);
%     plot(time(station==i & parQC < 2), par(station==i & parQC < 2), '.')
%     
%     figure(2);
%     avg = accumarray(round(time(station==i & parQC < 2)), par(station==i & parQC < 2), [] , @mean);
%     tavg = accumarray(round(time(station==i & parQC < 2)), time(station==i & parQC < 2), [] , @mean);
%     %key = find(depth(i+1)>depths,1);
%     key = keys(i+1);
%     disp(key)
% 
%     plot(tavg, log(avg), ['.-' colours(key)]);


    stationfield = matlab.lang.makeValidName(stationid(i+1, 1));
    disp(stationfield);
    
    figure(1); hold on; grid on
    set(gca, 'YScale', 'log')
    
%     if (~isfield(deployments, char(stationfield)))
%         figI = figI + 1;
%         deployments.(stationfield) = figI;
%         figure(figI); clf; hold on; grid on
%         title(stationid(i+1,1)); legend
%         titles{i+1} = stationid(i+1,1);
%         ylim([0.1 4000]);
%         set(gca, 'YScale', 'log')
%     else
%         figI = deployments.(stationfield);
%         figure(figI);
%     end
    disp(figI);
    %plot(time(station==i & parQC < 2), par(station==i & parQC < 2), '.')
    %plot(time(station==i), par(station==i), '.', 'DisplayName', stationid(i+1,2)+":"+num2str(depth(i+1)))
    avg = accumarray(round(time(station==i & parQC < 2)/5), par(station==i & parQC < 2), [] , @mean);
    
    tavg = accumarray(round(time(station==i & parQC < 2)/5), time(station==i & parQC < 2), [] , @mean);

    plot(mod(tavg(tavg>0)-datenum(2010,1,1), 365), avg(tavg>0), '.', 'DisplayName', stationid(i+1,2)+":"+num2str(depth(i+1)));
    %datetick('x', 'keeplimits');
end

t = time(sr > 100 & parQC < 2);
p = par(sr > 100 & parQC < 2);
s = sr(sr > 100 & parQC < 2);
ps = p ./ s;

avg = accumarray(round(t/5), ps, [], @mean);
tavg = accumarray(round(t/5), t, [], @mean);

figure(2)
plot(tavg(tavg>0),avg(tavg>0))
figure(4)
histogram(avg(tavg>0), 0.1:0.02:1, 'Normalization', 'cdf', 'DisplayStyle', 'stairs')
grid on

xlabel('swr/celestial');
ylabel('cumlative prob');
title('SOFS short wave radiation / incoming celestial, 5 day means');

% figure(1);
% grid on
% ylim([0 10000]);
% ylabel([par_name ' (' par_unit ')'], 'Interpreter', 'none')
% datetick('x', 'keeplimits');
% legend(stationStr')
% 
% figure(2);
% grid on
% ylim([0 20]);
% xlim([datenum(2009,1,1)  datenum(2017,12,31)]);
% ylabel([par_name ' (' par_unit ')'], 'Interpreter', 'none')
% datetick('x', 'keeplimits');
% legend(stationStr')

figures = findall(0,'type','figure'); 
for f = 1:numel(figures)
      fig = figures(f);
      %filename = titles{f}+"-PAR.ps";
      print( fig, '-dpsc2', 'SOTS-PAR.ps', '-append');
end
