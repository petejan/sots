%--------------------------------------------------------------------------
%
% DMS: Finds degrees, minutes and seconds of arc for a given angle 
%
% Input:
%
%   Dd        Angle in degrees in decimal representation
%
% Outputs:
%   D         Angular degrees
%   M         Minutes of arc
%   S         Seconds of arc
%
% Last modified:   2015/08/12   M. Mahooti
%
%--------------------------------------------------------------------------
function [D, M, S] = DMS (Dd)

x = abs(Dd);    D = floor(x);
x = (x-D)*60.0; M = floor(x);  S = (x-M)*60.0;

if (Dd<0.0)
    if (D~=0)
        D = D*-1;
    else
        if (M~=0)
        M = M*-1;
        else
            S = S*-1.0;
        end
    end
end

