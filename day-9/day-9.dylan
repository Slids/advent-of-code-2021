Module: day-9
Synopsis:
Author:
Copyright:

define function get-cave-map () => (map :: <sequence>)
  let cave-map = make(<stretchy-vector>);
  with-open-file(file-stream = "list.txt")
    while (~stream-at-end?(file-stream))
      let line = read-line(file-stream);
      add!(cave-map, get-int-vector-from-string(line));
    end;
  end;
  cave-map;
end;

define function adjacent-points(x :: <integer>, y :: <integer>, cave-map :: <sequence>) => (points :: <sequence>)
  let directions = #[ #[-1, 0], #[1, 0], #[0,-1], #[0,1]];
  let cave-map-depth = size(cave-map);
  let cave-map-length = size(cave-map[0]);
  let points = make(<stretchy-vector>);
  for (k in directions)
    let new-x = x + k[0];
    let new-y = y + k[1];
    if ((new-x < cave-map-depth) &
          (new-x > -1) &
          (new-y < cave-map-length) &
          (new-y > -1))
      add!(points, vector(new-x, new-y));
    end;
  end;
  points;
end;

define function find-lowest-points
    (cave-map :: <sequence>) => (coords :: <sequence>)
  let cave-map-depth = size(cave-map);
  let cave-map-length = size(cave-map[0]);
  let lowest-points = make(<stretchy-vector>);
  let directions = #[ #[-1, 0], #[1, 0], #[0,-1], #[0,1]];
  for (i from 0 below cave-map-depth)
    for (j from 0 below cave-map-length)
      block (break)
        for (k in adjacent-points(i, j, cave-map))
          if (cave-map[i][j] >= cave-map[k[0]][k[1]])
            break();
          end; // end if
        end; // end for
        add!(lowest-points, vector(i,j));
      end; // end block
    end;
  end;
  lowest-points;
end;

define function sum-lowest-points(cave-map :: <sequence>, coords :: <sequence>)
 => (int :: <integer>)
  let coords-val = map(method (a) cave-map[a[0]][a[1]] end, coords);
  reduce1(\+, coords-val);
end;

let cave-map = get-cave-map();
let lowest-points = find-lowest-points(cave-map);
let lowest-points-sum = sum-lowest-points(cave-map, lowest-points);
format-out("Lowest Points Sum + 1: %d\n", lowest-points-sum + size(lowest-points));

define function make-bool-map(cave-map :: <sequence>) => (bool-map :: <sequence>)
  let bool-map = make(<vector>, size: size(cave-map));
  for (i from 0 below size(cave-map))
    bool-map[i] := make(<vector>, size: size(cave-map[i]));
  end;
  bool-map;
end;

define function basin-size(lowest-point :: <sequence>, cave-map :: <sequence>, bool-map :: <sequence>) => (int :: <integer>)
  let point-list = list(lowest-point);
  let basin-size = 0;
  while (size(point-list) > 0)
    let cur-point = head(point-list);
    point-list := remove!(point-list, cur-point, test: method(a,b) (a[0] = b[0] & a[1] = b[1]) end);
    bool-map[cur-point[0]][cur-point[1]] := #t;
    basin-size := basin-size + 1;
    let adjacent = adjacent-points(cur-point[0], cur-point[1], cave-map);
    for (a in adjacent)
      if (~bool-map[a[0]][a[1]] & cave-map[a[0]][a[1]] > cave-map[cur-point[0]][cur-point[1]]
            & cave-map[a[0]][a[1]] ~= 9)
        point-list := add!(point-list, a);
      end;
    end;
  end;
  basin-size;
end;

let bool-map = make-bool-map(cave-map);
let bs-vec = map(method(a) basin-size(a,cave-map,bool-map) end, lowest-points);
bs-vec := sort(bs-vec, test: \>);
format-out("Largest Three Basin Size mult: %d\n", bs-vec[0] * bs-vec[1] * bs-vec[2])
