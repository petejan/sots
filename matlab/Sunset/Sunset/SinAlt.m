%------------------------------------------------------------------------------
%
% SinAlt: Sine of the altitude of Sun or Moon
%
% Inputs:
%   Event     Indicates event to find
%   MJD0      0h at date to investigate (as Modified Julian Date)
%   Hour      Hour
%   lambda    Geographic east longitude in [rad]
%   Cphi      Cosine of geographic latitude
%   Sphi      Sine of geographic latitude
%
% Output:   
%   out       Sine of the altitude of Sun or Moon at instant of Event
%
% Last modified:   2015/08/12   M. Mahooti
%
%------------------------------------------------------------------------------
function out = SinAlt ( Event, MJD0, Hour, lambda, Cphi, Sphi )

global PC eopdata

Ast_Const

MJD = MJD0 + Hour/24.0;

[UT1_UTC, TAI_UTC, x_pole, y_pole] = IERS(eopdata, MJD);
[UT1_TAI, UTC_GPS, UT1_GPS, TT_UTC, GPS_UTC] = timediff(UT1_UTC, TAI_UTC);

Mjd_TT = MJD + TT_UTC/86400.0;

[r_Mercury,r_Venus,r_Earth,r_Mars,r_Jupiter,r_Saturn,r_Uranus, ...
 r_Neptune,r_Pluto,r_Moon,r_Sun] = JPL_Eph_DE405(Mjd_TT);

if ( Event == 1 )    
    % Moon's right ascension and declination
    ra  = atan2( r_Moon(2), r_Moon(1) );
    dec = atan2( r_Moon(3), sqrt( r_Moon(1)^2+r_Moon(2)^2 ) );
else
    % Sun's right ascension and declination
    ra  = atan2( r_Sun(2), r_Sun(1) );
    dec = atan2( r_Sun(3), sqrt( r_Sun(1)^2+r_Sun(2)^2 ) );
end

tau = gmst(MJD) + lambda - ra;

out =  ( Sphi*sin(dec)+Cphi*cos(dec)*cos(tau) );

