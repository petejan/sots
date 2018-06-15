%--------------------------------------------------------------------------
%
% IERS
%
% Purpose:
%   Management of IERS time and polar motion data
%  
% Last modified:   2015/08/12   M. Mahooti
% 
%--------------------------------------------------------------------------
function [UT1_UTC, TAI_UTC, x_pole, y_pole] = IERS (eop, Mjd_TT)

Arcs = 3600.0*180.0/pi;  % Arcseconds per radian

mj = (round(Mjd_TT));
nop = length(eop);

for i=1:nop
    if (mj==eop(4,i))
        eop = eop(:,i);
        break;
    end
end

% Setting of IERS Earth rotation parameters
% (UT1-UTC [s], TAI-UTC [s], x ["], y ["])
UT1_UTC = eop(7);      % UT1-UTC time difference [s]
TAI_UTC = eop(13);     % TAI-UTC time difference [s]
x_pole  = eop(5)/Arcs; % Pole coordinate [rad]
y_pole  = eop(6)/Arcs; % Pole coordinate [rad]

