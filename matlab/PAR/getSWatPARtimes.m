
file_par = '../../data/IMOS_ABOS-SOTS_F_20090928_SOFS_FV01_SOFS-1-2010-PAR-DiscreteGeometries_END-20160413_C-20180604.nc';
%file_sw = '../../data/IMOS_ABOS-SOTS_F_20100318_SOFS_FV01_SOFS-1-2010-SW-DiscreteGeometries_END-20171101_C-20180604.nc';
file_sw = '../../data/IMOS_ABOS-ASFS_CFMST_20100318_SOFS_FV02_SOFS-Aggregate-SW_END-20171108_C-20181118.nc';

time_par = ncread(file_par, 'TIME') + datetime(1950,1,1);
time_sw = ncread(file_sw, 'TIME') + datetime(1950,1,1);

time_par_min = dateshift(time_par, 'start', 'minute');
time_sw_min = dateshift(time_sw, 'start', 'minute');

[sharedvals, iPar, iSw] = intersect(time_par_min, time_sw_min, 'stable');

sw = ncread(file_sw, 'SW');

sw_at_par = zeros(size(time_par)) * NaN;
sw_at_par(iPar) = sw(iSw);

par = ncread(file_par, 'PAR');

figure(1);  clf; hold on;grid on
plot(time_par, par, '.')
plot(time_sw, sw)
plot(time_par(iPar), sw(iSw), '.')

infile = '../../data/IMOS_ABOS-SOTS_F_20090928_SOFS_FV01_SOFS-1-2010-PAR-DiscreteGeometries_END-20160413_C-20180604.nc';

file = [infile(1:size(infile,2)-11) datestr(datetime(), 'yyyymmdd') '.nc'];

copyfile(infile, file);

ncid = netcdf.open(file, 'NC_WRITE');
varid = netcdf.defVar(ncid, 'SW', 'double', 0);

[dimname, dimlen] = netcdf.inqDim(ncid,0);

varid = netcdf.inqVarID(ncid, 'SW');

netcdf.putAtt(ncid, varid,'name','surface_downwelling_shortwave_flux_in_air');
netcdf.putAtt(ncid, varid,'standard_name','surface_downwelling_shortwave_flux_in_air');
netcdf.putAtt(ncid, varid,'long_name','surface_downwelling_shortwave_flux_in_air');
netcdf.putAtt(ncid, varid,'units','W/m2');
netcdf.putAtt(ncid, varid,'comment',['from ' file_sw]);

netcdf.putVar(ncid, varid, 0, numel(time_par), sw_at_par);

netcdf.close(ncid);