%--------------------------------------------------------------------------
%  inputs          
%    year        - year                           
%    mon         - month                          
%    day         - day                            
%    hr          - universal time hour            
%    min         - universal time min             
%    sec         - universal time sec             
%
%  outputs       
%    Mjd         - Modified julian date                    
%--------------------------------------------------------------------------
function Mjd = Mjday(yr, mon, day, hr, min, sec)

if (nargin < 4)
    hr = 0;
    min = 0;
    sec = 0;
end

jd = 367.0 * yr...
    - floor( (7 * (yr + floor( (mon + 9) / 12.0) ) ) * 0.25 )...
    + floor( 275 * mon / 9.0 )...
    + day + 1721013.5...
    + ( (sec/60.0 + min ) / 60.0 + hr ) / 24.0;

Mjd = jd - 2400000.5;

