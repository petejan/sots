%**************************************************************************
%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

function [DEC,JD,AZM,RAD] = soradna(LAT,LON,JD,y);

% SORADNA  computes no-sky solar radiation and solar altitude.
%
% [DEC,AZM,ALT,RAD] = SORADNA1(lat,lon,day,Year) 
%
% computes instantaneous values of solar declination, radiation and altitude
% from Position, yearday and Year.
%
%%Assumes yd is either a column or row vector, the other input variables are 
%%scalars, OR yd is a scalar, the other inputs matrices.
%
% It is put together from expressions taken from Appendix E in the
% 1978 edition of Almanac for Computers, Nautical Almanac Office, U.S.
% Naval Observatory. They are reduced accuracy expressions valid for the
% years 1800-2100. Solar declination computed from these expressions is
% accurate to at least 1'.
%
% The solar constant (1368.0 W/m^2) represents a mean of satellite measurements
% made over the last sunspot cycle (1979-1995)  taken from 
%  Coffey et al (1995), Earth System Monitor, 6, 6-10.  
%


  SC  = 1368.0;   % Solar Constant
  d2r = pi/180;   % deg --> rad

[m,n] = size(JD);

y = datenum(y,01,01);

JD = JD + y(:,ones(1,n));

JD = datevec(JD(:));

% compute Universal Time in hours
   UT = JD(:,4) + JD(:,5) / 60 + JD(:,6) / 3600;

% compute Julian ephemeris date in days (Day 1 is 1 Jan 4713 B.C.=-4712 Jan 1)
  JD = 367 * JD(:,1) - fix( 7 * ( JD(:,1) + fix( (JD(:,2)+9) / 12 ) ) / 4 ) + ...
        fix( 275 * JD(:,2) / 9 ) + JD(:,3) + 1721013 + UT/24;

% compute interval in Julian centuries since 1900
  JD = ( JD - 2415020 ) / 36525;

% compute mean anomaly of the sun
   G = 358.475833 + 35999.049750 * JD - 0.000150 * JD.^2;

% compute mean longitude of sun
   L = 279.696678 + 36000.768920 * JD + 0.000303 * JD.^2;

% compute mean anomaly of Jupiter: 225.444651 + 2880 * JD + 154.906654 * JD;
  JP = 225.444651 + 3034.906654 * JD;

% compute mean anomaly of Venus
  VN = 212.603219 + 58517.803875 * JD + 0.001286 * JD.^2;

% compute longitude of the ascending node of the moon's orbit
  NM = 259.183275 - 1934.142008 * JD + 0.002078 * JD.^2;

   G = (  G - 360 * fix(  G / 360 ) ) * d2r;
   L = (  L - 360 * fix(  L / 360 ) ) * d2r;
  JP = ( JP - 360 * fix( JP / 360 ) ) * d2r;
  VN = ( VN - 360 * fix( VN / 360 ) ) * d2r;
  NM = ( NM - 360 * fix( NM / 360 ) + 360 ) * d2r;

% compute sun theta (THETA)
  DEC = +0.397930 * sin(L)       - 0.000040 * cos(L)       ...
        +0.009999 * sin(G-L)     + 0.003334 * sin(G+L)     ...
        +0.000042 * sin(2*G+L)  - 0.000014 * sin(2*G-L)   ...
        -0.000030 * JD.*sin(G-L) - 0.000010 * JD.*sin(G+L) ...
        -0.000208 * JD.*sin(L)   - 0.000039 * sin(NM-L)    ...
        -0.000010 * cos(G-L-JP);

% compute sun rho
  RHO = 1.000421 - 0.033503 * cos(G) - 0.000140 * cos(2*G) + ...
        0.000084 * JD.*cos(G) - 0.000033 * sin(G-JP) + 0.000027 * sin(2*G-2*VN);

%%% RHO = 1 - 0.0335*sin( 2 * pi * (DayOfYear - 94)/365 )

% compute declination: DEC = asin( THETA ./ sqrt(RHO) );
   DEC = DEC ./ sqrt(RHO);

% compute equation of time (in seconds of time)

   JD = 276.697 + (0.98564734*36525) * JD;    % [deg]
   JD = ( JD - 360 * fix( JD / 360 ) ) * d2r;

   JD =   -97.8 * sin(  JD) - 431.3 * cos(  JD) ...
         +596.6 * sin(2*JD) -   1.9 * cos(2*JD) ...
           +4.0 * sin(3*JD) +  19.3 * cos(3*JD) - 12.7 * sin(4*JD);
   JD = JD / 3600;

% compute local hour angle (LHA)
   JD = JD + UT - 12;
   JD = 15 * JD + LON;

   JD =  JD * d2r;
  LAT = LAT * d2r;

  AZM = JD;  % Here: Local Hour Angle LHA

% compute radius vector: RV = sqrt(RHO);

% compute solar altitude: sin(ALT) = sin(LAT)*sin(DEC) + ...
%                                    cos(LAT)*cos(DEC)*cos(LHA)

   JD = sin(LAT) * DEC + cos(LAT) * sqrt(1-DEC.^2) .* cos(JD);

% compute solar radiation outside atmosphere

  RAD = ( SC ./ RHO ) .* JD .* ( JD > 0 );  % here: JD == sin(ALT)

  DEC = asin(DEC);
   JD = asin(JD);

  int = mod( floor( AZM / pi ) , 2 ); %  0: [ 0 .. 180 ); 1: [ 180 .. 360 )

  AZM = atan( sin(AZM) ./ ( sin(LAT) .* cos(AZM) - cos(LAT) .* tan(DEC) ) );

  DEC = DEC / d2r;
  AZM = AZM / d2r;
   JD = JD  / d2r;

  AZM = AZM + 180 * (1-int) + 180 * ( AZM < 0 );  % Correction to [ 0 .. 360 ]

DEC = reshape(DEC,m,n);
AZM = reshape(AZM,m,n);

JD  = reshape( JD,m,n);
RAD = reshape(RAD,m,n);
