Module: shared
Synopsis: Code shared between daily AOC libraries

// Convert a string containing only digit chars into a vector of integers, one for each
// digit.
define function get-int-vector-from-string (str :: <string>) => (ns :: <sequence>)
  // as is a typecast, so we need to subtract as(<integer>, '0')
  // to get the correct value.
  map-as(<vector>, method (char) as(<integer>, char) - as(<integer>, '0') end, str)
end function;

// Get the neighbors of a point in a matrix.
define function get-neighbors(matrix :: <sequence>, point :: <sequence>, #key include-diagonals :: <boolean> = #f)
 => (neighbors :: <sequence>)
  let row = point[0];
  let column = point[1];
  let neighbors = make(<stretchy-vector>);
  for (i in #[-1, 0, 1])
    for (j in #[-1, 0, 1])
      if (i = 0 & j = 0)
        // You can't be your own neighbor.
      elseif ( 0 <= row + i & row + i < size(matrix) &
               0 <= column + j & column + j < size(matrix) )
          if (abs(i) = abs(j))
          when (include-diagonals)
              add!(neighbors, vector(row + i, column + j));
            end;
          else
            add!(neighbors, vector(row + i, column + j));
          end;
      end;
    end;
  end;
  neighbors;
end;
