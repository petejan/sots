% plot PAR data

%file = 'data/IMOS_ABOS-SOTS_F_20090928_SOFS_FV01_SOFS-1-2010-PAR-DiscreteGeometries_END-20160413_C-20181121.nc';
%file = 'data/IMOS_ABOS-SOTS_F_20090928_SOFS_FV01_SOFS-1-2010-PAR-SW-SR-DiscreteGeometries_END-20160413_C-20181127.nc';
file = 'data/IMOS_ABOS-SOTS_F_20090928_SOFS_FV01_SOFS-1-2010-PAR-SR-SW-DiscreteGeometries_END-20160413_C-20181128.nc';

time = ncread(file, 'TIME') + datetime(1950,1,1);
par = ncread(file, 'PAR');
par_units = ncreadatt(file, 'PAR', 'units');
par_qc = ncread(file, 'PAR_quality_code');
sw = ncread(file, 'SW');

cSR = ncread(file, 'cSR');

station_name = ncread(file, 'station_name');
station_name_cell = cellstr(station_name');
stationIndex = ncread(file, 'stationIndex');
nom_depth = ncread(file, 'NOMINAL_DEPTH');
stn_surface = find(nom_depth < 0);

figure(1); clf
for i = min(stationIndex):max(stationIndex)
    plot(time(stationIndex == i), par(stationIndex == i), '.', 'DisplayName', ['PAR ' deblank(station_name(:, i+1)') ' @' num2str(nom_depth(i+1)) 'm']);
    hold on;
end
grid on; legend('show', 'Location','southoutside');
 
plot(time(cSR > 1), sw(cSR > 1), '.');
%plot(time(cSR > 1), cSR(cSR > 1), '.');
ylim([0 5000]);

figure(2); clf; hold on

stn_surface_data = ismember(stationIndex, stn_surface);

%figure(2)
%plot(time(cSR>1 & stn_surface_data), sw(cSR>1 & stn_surface_data)./par(cSR>1 & stn_surface_data), '.');

stn_select = regexp(station_name_cell, '.*2011:.*');
Index = find(not(cellfun('isempty',stn_select)));

%Index = nom_depth <= 0;

qc_level = 2;
stn = min(stationIndex):max(stationIndex);
n = 1;
uniqueDays = {};
uniqueTS = {};
dailyMeanPAR = {};
dailyMeanSW = {};
dailyMeanCSR = {};
station = {};
minTs = max(time);
maxTs = min(time);

for i = stn(Index)
%for i = min(stationIndex):max(stationIndex)
    msk = stationIndex == i & cSR > 1 & par_qc <= qc_level;
    t = datenum(time(msk));
    msk(msk) = msk(msk) & (t - round(t(1)-(10+12)/24)) > 3 & (t - round(t(end)-(10+12)/24) < -3);
% Get the unique dates, and their indices
    [uniqueDays{n},idxToUnique,idxFromUniqueBackToAll] = unique(round(datenum(time(msk)),-1));
    dailyMeanPAR{n} = accumarray(idxFromUniqueBackToAll, par(msk),[],@mean);
    
%    [uniqueDaysSW{n},idxToUniqueSW,idxFromUniqueBackToAllSW] = unique(round(datenum(time(msk & ~isnan(sw)))));
    dailyMeanSW{n} = accumarray(idxFromUniqueBackToAll, sw(msk),[],@nanmean);
    
    dailyMeanCSR{n} = accumarray(idxFromUniqueBackToAll, cSR(msk),[],@mean);
    uniqueTS{n} = datetime(uniqueDays{n}, 'ConvertFrom', 'datenum');
%    uniqueTSSW{n} = datetime(uniqueDaysSW{n}, 'ConvertFrom', 'datenum');
    station{n} = i;
    
    minTs = min([minTs min(uniqueTS{n})]);
    maxTs = max([maxTs max(uniqueTS{n})]);
    
    figure(2);
    plot(time(cSR>100 & msk), par(cSR>100 & msk)./sw(cSR>100 & msk), '.', 'DisplayName', ['SW/CSR ' deblank(station_name(:,station{n}+1)') ' @' num2str(nom_depth(station{n}+1)) 'm']);
    
    n = n + 1;
end

figure(2);
grid on
set(gca, 'YScale', 'log')
legend('show', 'Location','southoutside');
ylim([0.01 100]);
xlim([minTs maxTs]);

%sw2par = 4.57; % (McCree 1972)
sw2par = 2.114 ; 
%sw2par = 1.0;

figure(3); clf; hold on; grid on;
[uniqueDaysSW,idxToUniqueSW,idxFromUniqueBackToAllSW] = unique(round(datenum(time-10/24 ),-1));
dailyMeanSWAG = accumarray(idxFromUniqueBackToAllSW(cSR > 1), sw(cSR > 1),[],@nanmean);
plot(datetime(uniqueDaysSW, 'ConvertFrom', 'datenum'), dailyMeanSWAG * sw2par, '-.', 'DisplayName', 'SWR')

[uniqueDaysSR,idxToUniqueSR,idxFromUniqueBackToAllSR] = unique(round(datenum(time-10/24 ),-1));
dailyMeanSR = accumarray(idxFromUniqueBackToAllSR(cSR > 1), cSR(cSR > 1),[],@mean);
plot(datetime(uniqueDaysSR, 'ConvertFrom', 'datenum'), dailyMeanSR * sw2par, '-.', 'DisplayName', 'SR')

figure(5); clf; hold on; grid on;
figure(6); clf; hold on; grid on;
figure(6);
plot(time(cSR>100 & time > minTs), sw(cSR>100 & time > minTs)./cSR(cSR>100 & time > minTs), '.', 'DisplayName', 'SW/CSR', 'MarkerSize', 0.5);

for i = 1:n-1
    disp(['PAR ' deblank(station_name(:,station{i}+1)') ' @' num2str(nom_depth(station{i}+1)) 'm']);
    figure(3);
    plot(uniqueTS{i}, dailyMeanPAR{i}, 'DisplayName', ['PAR ' deblank(station_name(:,station{i}+1)') ' @' num2str(nom_depth(station{i}+1)) 'm']);
    %plot(uniqueTS{i}, dailyMeanCSR{i} * sw2par, '-.', 'DisplayName', ['CSR ' station_name(:,station{i}+1)']);
    %plot(uniqueTS{i}, dailyMeanSW{i} * sw2par, '-', 'DisplayName', ['SW ' station_name(:,station{i}+1)']);
    figure(5);
    plot(uniqueTS{i}, dailyMeanPAR{i}./(dailyMeanSW{i} * sw2par), 'DisplayName', ['PAR/SW ' deblank(station_name(:,station{i}+1)') ' @' num2str(nom_depth(station{i}+1)) 'm']);
    figure(6);
    plot(uniqueTS{i}, dailyMeanSW{i}./(dailyMeanCSR{i}), 'DisplayName', ['SW/CSR ' deblank(station_name(:,station{i}+1)') ' @' num2str(nom_depth(station{i}+1)) 'm'], 'LineWidth', 2);

end


figure(3);
title('10 day mean daylight par, and swr & sr scalled to umol/m^2/s (by 2.114)');
set(gca, 'YScale', 'log')
legend('show', 'Location','southoutside');
ylabel('radiation (umol/s/m^2)');
ylim([1 5000]);
xl = xlim();
xlim([minTs maxTs]);
%print('-dpsc2', 'par.ps', '-append');

figure(5); xlim(xl)
title('10 day mean daylight par/swr, swr scalled to umol/m^2/s (by 2.114)');
xlim([minTs maxTs]);
set(gca, 'YScale', 'log')
legend('show', 'Location','southoutside');
ylim([0.001 10]);
%print('-dpsc2', 'par-swr.ps', '-append');

figure(6); xlim(xl)

xlim([minTs maxTs]);
set(gca, 'YScale', 'log')
%legend('show', 'Location','southoutside');
ylim([0.01 2]);
title('swr/incoming (cloudiness ratio)');

figure(10); clf
msk_sofs2_licor = stationIndex == 15 & cSR > 1 & time > datetime(2011,11,26) & time < datetime(2012,7,21);
plot(sw(msk_sofs2_licor), par(msk_sofs2_licor), '.'); grid on
title('SOFS-2 : LiCor Par vs SWR');

xlabel('swr (W/m^2)');
ylabel('par (umol/m^2/s)');

xlim([0 1400]);
ylim([0 1600]);
hold on; plot([0 1400], [0 1400*sw2par]);
coefficients = polyfit(sw(msk_sofs2_licor), par(msk_sofs2_licor), 1);
yFit = polyval(coefficients , [0 1000]);
plot([0 1000], yFit);

figure(11); clf
d = duration(0, 0, 10);
msk_pulse8_alec0m = stationIndex == 14 & cSR > 1 & time > datetime(2011,8,4) & time < datetime(2012,7,18);
time_par = time(msk_pulse8_alec0m);
time_sw = time(msk_sofs2_licor);
time_par_min = dateshift(time_par+d, 'start', 'minute');
time_sw_min = dateshift(time_sw+d, 'start', 'minute');
p8 = par(msk_pulse8_alec0m);
s2 = sw(msk_sofs2_licor);

[sharedvals, iPar, iSw] = intersect(time_par_min, time_sw_min, 'stable');
plot(p8(iPar), s2(iSw), '.'); grid on
title('Pulse-8 Alec Par vs SOFS-2 LiCor Par');
ylabel('LiCor (umol/m^2/s)');
xlabel('Alec (umol/m^2/s)');
xlim([0 1400]);
ylim([0 1600]);
hold on; plot([0 1400], [0 1400]);
