Module: shared
Synopsis: Code shared between daily AOC libraries

// Convert a string containing only digit chars into a vector of integers, one for each
// digit.
define function get-int-vector-from-string (str :: <string>) => (ns :: <sequence>)
  // as is a typecast, so we need to subtract as(<integer>, '0')
  // to get the correct value.
  map-as(<vector>, method (char) as(<integer>, char) - as(<integer>, '0') end, str)
end function;
