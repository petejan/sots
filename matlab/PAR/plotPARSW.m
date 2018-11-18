% plot PAR data

file = '../../data/IMOS_ABOS-SOTS_F_20090928_SOFS_FV01_SOFS-1-2010-PAR-DiscreteGeometries_END-20160413_C-20181118.nc';
time = ncread(file, 'TIME') + datetime(1950,1,1);
par = ncread(file, 'PAR');
par_units = ncreadatt(file, 'PAR', 'units');
par_qc = ncread(file, 'PAR_quality_code');
cSR = ncread(file, 'cSR');
sw = ncread(file, 'SW');

file_sw = '../../data/IMOS_ABOS-ASFS_CFMST_20100318_SOFS_FV02_SOFS-Aggregate-SW_END-20171108_C-20181118.nc';
sw_ag = ncread(file, 'SW');
time_ag = ncread(file, 'TIME') + datetime(1950,1,1);

station_name = ncread(file, 'station_name');
station_name_cell = cellstr(station_name')
stationIndex = ncread(file, 'stationIndex');
nom_depth = ncread(file, 'NOMINAL_DEPTH');
stn_surface = find(nom_depth < 0);

figure(1); clf
plot(time(cSR > 1), par(cSR > 1), '.');
hold on; grid;
plot(time(cSR > 1), sw(cSR > 1), '.');
plot(time(cSR > 1), cSR(cSR > 1), '.');
ylim([0 5000]);

stn_surface_data = ismember(stationIndex, stn_surface);

figure(2)
plot(time(cSR>1 & stn_surface_data), sw(cSR>1 & stn_surface_data)./par(cSR>1 & stn_surface_data), '.');

%stn_select = regexp(station_name_cell, '.*2009:.*');
%Index = find(not(cellfun('isempty',stn_select)));

Index = nom_depth <= 0;

qc_level = 10;
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
    disp(station_name(:,n)');
% Get the unique dates, and their indices
    [uniqueDays{n},idxToUnique,idxFromUniqueBackToAll] = unique(round(datenum(time(stationIndex == i & cSR > 1 & par_qc <= qc_level))-10/24 ));

    dailyMeanPAR{n} = accumarray(idxFromUniqueBackToAll, par(stationIndex == i & cSR > 1 & par_qc <= qc_level),[],@mean);
    %dailyMeanSW{n} = accumarray(idxFromUniqueBackToAll, sw(stationIndex == i & cSR > 1 & par_qc <= qc_level),[],@mean);
    %dailyMeanCSR{n} = accumarray(idxFromUniqueBackToAll, cSR(stationIndex == i & cSR > 1 & par_qc <= qc_level),[],@mean);
    uniqueTS{n} = datetime(uniqueDays{n}, 'ConvertFrom', 'datenum');
    station{n} = i;
    
    minTs = min([minTs min(uniqueTS{n})]);
    maxTs = max([maxTs max(uniqueTS{n})]);
    
    n = n + 1;
end

figure(3); clf; hold on; grid on;
sw2par = 4.57;
%sw2par = 2.114 ; % (McCree 1972). was 2.114

[uniqueDaysSW,idxToUniqueSW,idxFromUniqueBackToAllSW] = unique(round(datenum(time_ag-10/24 )));
dailyMeanSWAG = accumarray(idxFromUniqueBackToAllSW(~isnan(sw_ag)), sw_ag(~isnan(sw_ag)),[],@mean);
plot(datetime(uniqueDaysSW, 'ConvertFrom', 'datenum'), dailyMeanSWAG * sw2par, ':', 'DisplayName', 'SWR')

[uniqueDaysSR,idxToUniqueSR,idxFromUniqueBackToAllSR] = unique(round(datenum(time-10/24 )));
dailyMeanSR = accumarray(idxFromUniqueBackToAllSR, cSR,[],@mean);
plot(datetime(uniqueDaysSR, 'ConvertFrom', 'datenum'), dailyMeanSR * sw2par, '-.', 'DisplayName', 'SR')


for i = 1:n-1
    plot(uniqueTS{i}, dailyMeanPAR{i}, 'DisplayName', ['PAR ' deblank(station_name(:,station{i}+1)') ' @' num2str(nom_depth(station{i}+1)) 'm']);
    %plot(uniqueTS{i}, dailyMeanCSR{i} * 2.114, '-.', 'DisplayName', ['CSR ' station_name(:,station{i}+1)']);
    %plot(uniqueTS{i}, dailyMeanSW{i} * 2.114, '-', 'DisplayName', ['SW ' station_name(:,station{i}+1)']);
end
%set(gca, 'YScale', 'log')
%legend('show', 'Location','southoutside');
ylabel('radiation (umol/s/m^2)');
ylim([10 5000]);
xlim([minTs maxTs]);
