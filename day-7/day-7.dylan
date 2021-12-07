Module: day-7
Synopsis:
Author:
Copyright:

define function get-int-vector-from-string(str :: <string>) => (ns :: <sequence>)
  map(\string-to-integer, split(str, ","))
end;

define function triangular(int :: <integer>) => (ns :: <integer>)
  floor/((int * (int + 1)), 2);
end;


let crabs = 0;
with-open-file(file-stream = "example-list.txt")
  crabs := get-int-vector-from-string(read-line(file-stream));
end;


let max-crab = reduce1(max,crabs);
let smallest-distance = max-crab * size(crabs);
for (i from 0 below max-crab)
  let distance = method (point)  abs(point - i) end;
  let total-distance = reduce1(\+, map(distance, crabs));
  when (smallest-distance > total-distance)
    smallest-distance := total-distance
  end;
end;
format-out("Part 1: smallest-distance: %d \n", smallest-distance);

smallest-distance := max-crab * size(crabs) * 100;
for (i from 0 below max-crab)
  let distance = method (point)  abs(point - i) end;
  let total-distance = reduce1(\+, map(triangular, map(distance, crabs)));
  when (smallest-distance > total-distance)
    smallest-distance := total-distance
  end;
end;
format-out("Part 2: smallest-distance: %d \n", smallest-distance)
