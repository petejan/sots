
file_par = 'data/IMOS_ABOS-SOTS_F_20090928_SOFS_FV01_SOFS-1-2010-PAR-DiscreteGeometries_END-20160413_C-20181127.nc';
%file_sw = 'data/IMOS_ABOS-SOTS_F_20100318_SOFS_FV01_SOFS-1-2010-SW-DiscreteGeometries_END-20171101_C-20180604.nc';
file_sw = 'data/IMOS_ABOS-ASFS_CFMST_20100318_SOFS_FV02_SOFS-Aggregate-SW_END-20171108_C-20181118.nc';

time_par = ncread(file_par, 'TIME') + datetime(1950,1,1);
time_sw = ncread(file_sw, 'TIME') + datetime(1950,1,1);
d = duration(0, 0, 10);
time_par_min = dateshift(time_par+d, 'start', 'minute');
time_sw_min = dateshift(time_sw+d, 'start', 'minute');
station_name = ncread(file_par, 'station_name');
station_name_cell = cellstr(station_name');

stationIndex = ncread(file_par, 'stationIndex');
stn = min(stationIndex):max(stationIndex);
sw = ncread(file_sw, 'SW');

sw_at_par = zeros(size(time_par)) * NaN;

for i = stn
    disp(station_name_cell{find(stn == i, 1)});
    msk = stationIndex == i;
    t_inst = time_par_min; % make a copy
    t_inst(~msk) = datetime(1900,0,0); % remove unwated ones
    [sharedvals, iPar, iSw] = intersect(t_inst, time_sw_min, 'stable');
    
    disp(size(iPar))
    sw_at_par(iPar) = sw(iSw);
end

par = ncread(file_par, 'PAR');

figure(1);  clf; hold on;grid on
plot(time_par, par, '.')
plot(time_sw, sw)
plot(time_par, sw_at_par, '.')

infile = 'data/IMOS_ABOS-SOTS_F_20090928_SOFS_FV01_SOFS-1-2010-PAR-DiscreteGeometries_END-20160413_C-20181127.nc';

file = [infile(1:size(infile,2)-11) datestr(datetime(), 'yyyymmdd') '.nc'];

file = strrep(file, 'PAR', 'PAR-SW');
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