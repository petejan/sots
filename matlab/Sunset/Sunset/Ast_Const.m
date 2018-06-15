%--------------------------------------------------------------------------
% 
%  Ast_Const.m
%  
%  Purpose:
%     Definition of constants used in APC library and applications
% 
% 
%--------------------------------------------------------------------------

pi        = 3.14159265358979324;
pi2       = 2.0*pi;
Rad       = pi / 180.0;
Deg       = 180.0 / pi;
Arcs      = 3600.0*180.0/pi;

R_Earth   =   6378.137;     % [km]
R_Sun     = 696000.0;       % [km]
R_Moon    =   1738.0;       % [km]

MJD_J2000 = 51544.5;        % MJD of Epoch J2000.0
T_J2000   =  0.0;           % Epoch J2000.0
T_B1950   = -0.500002108;   % Epoch B1950

kGauss    = 0.01720209895;  % gravitational constant
GM_Sun    = kGauss*kGauss;  % [AU^3/d^2]

AU        = 149597870.0;    % Astronomical unit [km]

c_light   = 173.14;         % speed of light [AU/d]

