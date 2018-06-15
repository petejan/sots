% load all the flntus data

% load \Users\jan079\Desktop\mooring_data.mat

fields = fieldnames(cleandat_level1);
nel = numel(fields);

tmax = 0;
tmin = datenum(2020,1,1);

for i=1:nel
    t = cleandat_level1.(fields{i}).time(cleandat_level1.(fields{i}).bb_qc<2);
    dv = datevec(t);
    doy = t - datenum(dv(:,1),1,1) + 1;

    disp(horzcat(fields{i}, ' ', datestr(max(t)), ' ', datestr(min(t)), ' ',  allcalibs.(fields{i}).serial_no))

    cnts = cleandat_level1.(fields{i}).fl_cnts(cleandat_level1.(fields{i}).bb_qc<2);
    fntus = (cnts - allcalibs.(fields{i}).fl_dark_cnts) .* allcalibs.(fields{i}).fl_scale_factor;
    
    outname = horzcat((fields{i}), '-', regexprep(allcalibs.(fields{i}).serial_no, '[; ]', '-'), '-FLNTUSdata.txt');
    
    fid = fopen(outname, 'w+');
    for j=1:length(t)
        fprintf(fid,'%s,%s,CHL_UGL=%4.3f (%d),BB=%6.5f (%d)\n', datestr(t(j), 'yyyy-mm-dd HH:MM:SS'), strrep((fields{i}),'_','-'), ...
            cleandat_level1.(fields{i}).fl_chl_a(j), cleandat_level1.(fields{i}).fl_qc(j), ...
            cleandat_level1.(fields{i}).bb_bbp(j), cleandat_level1.(fields{i}).bb_qc(j));
    end
    fclose(fid);
    
end
