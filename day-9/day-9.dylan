Module: day-9
Synopsis:
Author:
Copyright:

define function get-int-vector-from-string(str :: <string>) => (ns :: <sequence>)
  map(\string-to-integer, split(str, ",", remove-if-empty?: #t))
end;

define function get-cave-map () => (map :: <sequence>)
  let cave-map = make(<stretchy-vector>);
  with-open-file(file-stream = "example-list.txt")
    while (~stream-at-end?(file-stream))
      let line = read-line(file-stream);
      add!(cave-map, get-int-vector-from-string(line));
    end;
  end;
  cave-map;
end;

define function find-lowest-points
    (cave-map :: <sequence>) => (coords :: <sequence>)
  let cave-map-depth = size(cave-map);
  let cave-map-length = size(cave-map[0]);
  let lowest-points = make(<stretchy-vector>);
  let directions = #[#[-1, 0], #[1, 0], #[0,-1], #[0,1]];
  for (i from 0 below cave-map-depth)
    for (j from 0 below cave-map-length)
      block (continue)
        for (k in directions)
          if (i + k[0] < cave-map-length &
                i + k[0] > -1 &
                j + k[1] > cave-map-depth &
                j + k[1] > -1 &
                cave-map[i][j] > cave-map[i + k[0]][j + k[1]])
            continue();
          end; // end if
        end; // end for
        add!(lowest-points, [i,j]);
      end; // end block
    end;
  end;
  lowest-points;
end;

define function sum-lowest-points(cave-map :: <sequence>, coords :: <sequence>)
 => (int :: <integer>)
  let coords-val = map(method (a) cave-map[a[0],a[1]] end, coords);
  reduce1(\+, coords-val);
end;

let cave-map = get-cave-map();
let lowest-points = find-lowest-points(cave-map);
let lowest-points-sum = sum-lowest-points(cave-map, lowest-points);
format-out("Lowest Points Sum: %d\n", lowest-points-sum)
