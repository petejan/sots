% add a incomming radiation to PAR file

%infile = '../../data/IMOS_ABOS-SOTS_F_20090928_SOFS_FV01_SOFS-1-2010-PAR-DiscreteGeometries_END-20160413_C-20180423.nc';
infile = 'data/IMOS_ABOS-SOTS_F_20090928_SOFS_FV01_SOFS-1-2010-PAR-SW-DiscreteGeometries_END-20160413_C-20181128.nc';

file = [infile(1:size(infile,2)-11) datestr(datetime(), 'yyyymmdd') '.nc'];

file = strrep(file, 'PAR', 'PAR-SR');
copyfile(infile, file);

ncid = netcdf.open(file,'NC_WRITE');
varid = netcdf.defVar(ncid,'cSR','double', 0);

[dimname, dimlen] = netcdf.inqDim(ncid,0);

time1 = ncread(file, 'TIME') + datetime(1950,1,1);

lat = ncreadatt(file, '/', 'geospatial_lat_max');
lon = ncreadatt(file, '/', 'geospatial_lon_max');

y = year(time1);
jd = time1 - datenum(y,1,1);
[DEC,JD,AZM,RAD] = soradna(lat,lon,jd,y);

varid = netcdf.inqVarID(ncid,'cSR');

netcdf.putAtt(ncid,varid,'name','celestial incoming solar radiation ');
netcdf.putAtt(ncid,varid,'long_name','incoming_solar_radiation');
netcdf.putAtt(ncid,varid,'units','W/m2');
netcdf.putAtt(ncid,varid,'comment','using http://mooring.ucsd.edu/software/matlab/doc/toolbox/geo/suncycle.html');

netcdf.putVar(ncid,varid,0,numel(RAD),RAD);

netcdf.close(ncid);