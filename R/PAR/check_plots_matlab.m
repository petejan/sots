
file = 'par-data-qc.nc'

time = ncread(file, 'TIME') + datetime(1950,1,1);
par = ncread(file, 'PAR');
%par_qc = ncread(file, 'PAR_quality_code');
par_qc_gr = ncread(file, 'PAR_quality_code_gr');
par_qc_cl = ncread(file, 'PAR_quality_code_cl');
par_qc_fl = ncread(file, 'PAR_quality_code_fl');
par_qc_nn = ncread(file, 'PAR_quality_code_nn');
par_qc_man = ncread(file, 'PAR_quality_code_man');

sensor = ncread(file, 'sensor');
epar = ncread(file, 'ePAR');
depth = ncread(file, 'depth');

sn = 9;

figure(1); clf;
sh(1) = subplot(2,1,1);
plot(time(sensor==sn), par(sensor==sn)); hold on
plot(time(sensor==sn), epar(sensor==sn) * 3); 
plot(time(sensor==(sn-1)), par(sensor==(sn-1))); hold on

ylim([0 4500]);

sh(2) = subplot(2,1,2);
plot(time(sensor==sn), par_qc_nn(sensor==sn));
ylim([0 9])

linkaxes(sh, 'x')

figure(2);
si(1)=subplot(3,1,1);
plot(time, par);
si(2)=subplot(3,1,2);
plot(time, par_qc_nn);
ylim([0 9]);
si(3)=subplot(3,1,3);
plot(time, sensor, '.');

linkaxes(si, 'x')
