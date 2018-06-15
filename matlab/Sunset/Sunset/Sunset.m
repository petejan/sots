%--------------------------------------------------------------------------
%
% Sunset
% 
% Inputs:
%   year
%   month
%   day
%   lambda    Geographic east longitude of the observer in [rad]
%   phi       Geographic latitude of the observer in [rad]
%   zone      Difference local time - universal time in [h]
%   twilight  Indicates civil, nautical or astronomical twilight
% 
% Output:
%             Rising and setting times of Sun and Moon and twilight times
%
% Last modified:   2015/08/12   M. Mahooti
% 
%--------------------------------------------------------------------------
clc
clear
format long g

global PC eopdata

Ast_Const
DE405

% Initialize UT1-UTC and TAI-UTC time difference
fid = fopen('eop19620101.txt','r');

%  ----------------------------------------------------------------------------------------------------
% |  Date    MJD      x         y       UT1-UTC      LOD       dPsi    dEpsilon     dX        dY    DAT
% |(0h UTC)           "         "          s          s          "        "          "         "     s 
%  ----------------------------------------------------------------------------------------------------

eopdata = fscanf(fid,'%i %d %d %i %f %f %f %f %f %f %f %f %i',[13 inf]);

fclose(fid);

% Events to search for
% 1 'Moon'     % indicates moonrise or moonset
% 2 'Sun'      % indicates sunrise or sunset
% 3 'CivilTwi' % indicates Civil twilight
% 4 'NautiTwi' % indicates nautical twilight
% 5 'AstroTwi' % indicates astronomical twilight

year = 2015;
month = 10;
day = 12;

% Observing site:  east longitude [deg], latitude [deg]
lambda = 120;
phi = -30;

% local time - UT [h]
zone = 3;

% Twilight definition (c, n or a)
cTwilight = 'c';

switch cTwilight
    case 'c'
        twilight = 3;
    case 'a'
        twilight = 4;
    case 'n'
    otherwise
        twilight = 5;
end

lambda = lambda * Rad;
phi = phi * Rad;
zone = zone/24.0;

MJD = Mjday(year,month,day) - zone;

start_date = MJD;

% Title
fprintf('      SUNSET: solar and lunar rising and setting times  \n');

% Header
fprintf('    Date');
fprintf('          Moon              Sun             Twilight \n');
fprintf('        ');
fprintf('        rise/set          rise/set        beginning/end\n\n');

% loop over 10 subsequent days
for day = 0:9
    % current date    
    date = start_date + day;
    [year,mon,d,hr,min,sec] = invjday (date+zone + 2400000.5);
    fprintf('%4d/%2.2d/%2.2d', year, mon, d);
    
    % loop over cases (Moon, Sun, Twilight)
    for iEvent = 0:2
        % After events for Moon and Sun: specify kind of twilight event
        if (iEvent<2)
            Event = iEvent+1;
        else
            Event = twilight;
        end
        
        % Now try to find times of events
        [LT_Rise,LT_Set,rise,sett,above] = FindEvents(Event,date,lambda,phi);
        
        % Output
        if ( (rise == 1) | (sett == 1) )
          if ( rise == 1 )
              fprintf('   ');
              [H, M, S] = DMS (LT_Rise);
              fprintf('%2.2d:%2.2d', H, M);
              fprintf(' ');
          else
              fprintf('   ----- ');
          end
          if ( sett == 1 )
            fprintf('   ');
            [H, M, S] = DMS (LT_Set);
            fprintf('%2.2d:%2.2d', H, M);
            fprintf(' ');
          else
              fprintf('   ----- ');
          end          
        else
          if ( above )
              if ( Event >= 3 )
                  fprintf('    always bright ');
              else
                  fprintf('   always visible ');
              end
          else
              if ( Event >= 3 )
                  fprintf('     always dark  ');
              else
                  fprintf('  always invisible');
              end
          end
        end
    end
    fprintf('\n');
end

% Trailer
fprintf('\n');

fprintf(' all times in local time (=UT+');
fprintf('%d', 24.0*zone);
fprintf('h)');
fprintf('\n');

