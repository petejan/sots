%------------------------------------------------------------------------------
%
% FindEvents: Search for rise/set/twilight events of Sun or Moon
%
% Inputs:
%   Event     Indicates event to search for
%   MJD0h     0h at desired date as Modified Julian Date
%   lambda    Geographic east longitude of the observer in [rad]
%   phi       Geographic latitude of the observer in [rad]
%
% Outputs:
%   LT_Rise   Local time of rising or beginning of twilight
%   LT_Set    Local time of setting or end of twilight
%   rises     Event takes place
%   sets      Event takes place
%   above     Sun or Moon is circumpolar
%
% Last modified:   2015/08/12   M. Mahooti
%
%------------------------------------------------------------------------------
function [LT_Rise,LT_Set,rises,sets,above] = FindEvents(Event,MJD0h,lambda,phi)

Ast_Const

sinh0 = [...
  sin(Rad*( +8.0/60.0)), ... % Moonrise              at h= +8' 
  sin(Rad*(-50.0/60.0)), ... % Sunrise               at h=-50'
  sin(Rad*(   - 6.0  )), ... % Civil twilight        at h=-6 deg
  sin(Rad*(   -12.0  )), ... % Nautical twilight     at h=-12deg
  sin(Rad*(   -18.0  )), ... % Astronomical twilight at h=-18deg
];

Cphi = cos(phi);
Sphi = sin(phi);
hour = 1.0;

% Initialize for search
y_minus = SinAlt(Event, MJD0h, hour-1.0, lambda, Cphi, Sphi) - sinh0(Event);

LT_Rise = 0;
LT_Set = 0;
above = (y_minus>0.0);
rises = 0;
sets  = 0;

% loop over search intervals from [0h-2h] to [22h-24h]
while(1)
  y_0    = SinAlt( Event, MJD0h, hour    , lambda, Cphi, Sphi )-sinh0(Event);
  y_plus = SinAlt( Event, MJD0h, hour+1.0, lambda, Cphi, Sphi )-sinh0(Event);
  
  % find parabola through three values y_minus,y_0,y_plus
  [~, ye, root1, root2, nRoot] = Quad(y_minus,y_0,y_plus);
  
  if ( nRoot==1 )
    if ( y_minus < 0.0 ) 
      LT_Rise = hour+root1;
      rises = 1;
    else
      LT_Set  = hour+root1;
      sets  = 1;
    end
  end
  
  if ( nRoot == 2 )
    if ( ye < 0.0 )
      LT_Rise = hour+root2;
      LT_Set = hour+root1;
    else
      LT_Rise = hour+root1;
      LT_Set = hour+root2;
    end
    rises = 1;
    sets = 1;
  end          
  
  y_minus = y_plus;     % prepare for next interval
  hour = hour + 2.0;
  
if ( ( (hour == 25.0) | ( (rises == 1) & (sets == 1) ) ) )
    break
end
end

