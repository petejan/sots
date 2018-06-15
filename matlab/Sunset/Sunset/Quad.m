%------------------------------------------------------------------------------
%
% Quad: Quadratic interpolation
%
%       Performs root finding and search for extreme values based on three 
%       equidistant values of a function.
%
% Input:
%
%   y_minus   Value of function at x = -1
%   y_0       Value of function at x =  0
%   y_plus    Value of function at x =  1
%
% Output:
%
%   xe        Abscissa of extremum (may be outside [-1, 1])
%   ye        Value of function at xe
%   root1     First root found
%   root2     Second root found
%   n_root    Number of roots found in [-1, 1]
%
% Last modified:   2015/08/12   M. Mahooti
%
%------------------------------------------------------------------------------
function [xe, ye, root1, root2, n_root] = Quad (y_minus,y_0,y_plus)

% Coefficients of interpolating parabola y=a*x^2+b*x+c
a  = 0.5*(y_plus+y_minus) - y_0;
b  = 0.5*(y_plus-y_minus);
c  = y_0;

% Find extreme value
xe = -b/(2.0*a); 
ye = (a*xe+b) * xe + c;

dis = b*b - 4.0*a*c; % Discriminant of y=a*x^2+b*x+c

if (dis >= 0) % Parabola has roots
    dx = 0.5 * sqrt (dis) / abs (a);
    
    root1 = xe - dx; 
    root2 = xe + dx;
    
    n_root = 0;
    
    if (abs(root1) <= 1.0)
        n_root = n_root + 1;
    end
    if (abs(root2) <= 1.0)
        n_root = n_root + 1;
    end
    if (root1       < -1.0)
        root1 = root2;
    end
end

