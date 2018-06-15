%---------------------------------------------------------------------------
%
%  this function finds the year, month, day, hour, minute and second
%    given the julian date. tu can be ut1, tdt, tdb, etc.
%
%  author        : david vallado                  719-573-2600   27 may 2002
%
%  revisions
%                -
%
%  inputs          description                    range / units
%    jd          - julian date                    days from 4713 bc
%
%  outputs       :
%    year        - year                           1900 .. 2100
%    mon         - month                          1 .. 12
%    day         - day                            1 .. 28,29,30,31
%    hr          - hour                           0 .. 23
%    min         - minute                         0 .. 59
%    sec         - second                         0.0 .. 59.999
%
%---------------------------------------------------------------------------
function [year,mon,day,hr,min,sec] = invjday (jd)

% ----------------- find year and days of the year ---------------
temp   = jd-2415019.5;
tu     = temp / 365.25;
year   = 1900 + floor( tu );
leapyrs= floor( ( year-1901 )*0.25 );
days   = temp - ((year-1900)*365.0 + leapyrs );

% ------------ check for case of beginning of a year -------------
if days < 1.0
    year   = year - 1;
    leapyrs= floor( ( year-1901 )*0.25 );
    days   = temp - ((year-1900)*365.0 + leapyrs );
end

% ------------------- find remaining data  -----------------------
[mon,day,hr,min,sec] = days2mdh( year,days );
% sec= sec - 0.00000086400;

