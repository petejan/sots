file = 'IMOS_ABOS-SOTS_F_20090928_SOFS_FV01_SOFS-1-2010-PAR-SWR-cSR-DiscreteGeometries_END-20160413_C-20200215.nc'

time = ncread(file, 'TIME') + datetime(1950,1,1);
par = ncread(file, 'PAR');
sensor = ncread(file, 'stationIndex');
sensorName = ncread(file, 'station_name');

nom_depth = ncread(file, 'NOMINAL_DEPTH');

sn = 9;

figure(3); clf;
plot(time(sensor==sn), par(sensor==sn), 'DisplayName', sensorName(:,sn+1)'); hold on
plot(time(sensor==(sn-2)), par(sensor==(sn-2)), 'DisplayName', sensorName(:,sn+1-2)'); hold on
plot(time(sensor==(sn-1)), par(sensor==(sn-1)), 'DisplayName', sensorName(:,sn+1-1)'); hold on
plot(time(sensor==(sn-3)), par(sensor==(sn-3)), 'DisplayName', sensorName(:,sn+1-3)'); hold on
ylim([0 4500]);
