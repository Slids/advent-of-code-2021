Module: day-17
Synopsis:
Author:
Copyright:


define function get-input-bounds (file :: <string>) => (x-bounds :: <sequence>, y-bounds :: <sequence>)
  let str = "";
  with-open-file(file-stream = file)
    while (~stream-at-end?(file-stream))
      str := read-line(file-stream);
    end;
  end;
  let split-1 = split(str, "=");
  let x-str-split = split(split(split-1[1], ",")[0], "..");
  let y-str-split = split(split-1[2], "..");
  values( map(string-to-integer, x-str-split), map(string-to-integer, y-str-split));
end;

// These are truly awful bounds...
define function calc-min-bounds(x-bounds :: <sequence>, y-bounds :: <sequence>) =>
    (x-shot-bounds :: <sequence>, y-shot-bounds :: <sequence>)
  let x-shot-bounds = vector(1,1);
  let tri-x = floor/(x-shot-bounds[0] * (x-shot-bounds[0] + 1), 2);
  x-shot-bounds[0] := x-shot-bounds[0] + 1;
  while (tri-x < x-bounds[0])
    x-shot-bounds[0] := x-shot-bounds[0] + 1;
    tri-x := floor/(x-shot-bounds[0] * (x-shot-bounds[0] + 1), 2);
  end;
  x-shot-bounds[1] := x-bounds[1];
  let y-shot-bounds = vector(1, 1);
  y-shot-bounds[0] := y-bounds[0];
  y-shot-bounds[1] := 2 * abs(y-bounds[0]);
  values(x-shot-bounds, y-shot-bounds);
end;

define function calculate-path (x :: <integer>, y :: <integer>, x-bound :: <sequence>, y-bound :: <sequence>) => (arc :: <sequence>)
  let path = make(<stretchy-vector>);
  let last-position = vector(x, y);
  let x-velocity = x;
  let y-velocity = y;
  while (last-position[0] <= x-bound[1] & last-position[1] >= y-bound[0])
    add!(path, last-position);
    x-velocity := max(0, x-velocity - 1);
    y-velocity := y-velocity - 1;
    let new-x =  last-position[0] + x-velocity;
    let new-y =  last-position[1] + y-velocity;
    last-position := vector(new-x, new-y);
  end;
  path;
end;

define function calculate-containment(x :: <integer>, y :: <integer>, x-bounds :: <sequence>, y-bounds :: <sequence>)
 => (p :: <boolean>)
  let x-contains = (x >= x-bounds[0]) & (x <= x-bounds[1]);
  let y-contains = (y >= y-bounds[0]) & (y <= y-bounds[1]);
  x-contains & y-contains;
end;

define function calculate-max-hieight (paths :: <sequence>)
 => (val :: <integer>)
  let calculate-max-high-path = method (path) reduce(max, $minimum-integer, map(second, path)) end;
  reduce(max, $minimum-integer, map(calculate-max-high-path, paths));
end;

define function calculate-paths (x-bounds :: <sequence>, y-bounds :: <sequence>)
 => (val :: <sequence>)
  let (x-shot-bounds, y-shot-bounds) = calc-min-bounds(x-bounds, y-bounds);
  let paths = make(<stretchy-vector>);
  for (x from x-shot-bounds[0] to x-shot-bounds[1])
    for (y from y-shot-bounds[0] to y-shot-bounds[1])
      let path = calculate-path(x, y, x-bounds, y-bounds);
      let last-el = last(path);
      if (calculate-containment(last-el[0], last-el[1], x-bounds, y-bounds))
        add!(paths, path);
      end;
    end;
  end;
  paths;
end;

define function main
    (name :: <string>, arguments :: <vector>)
  let (x-bounds, y-bounds) = get-input-bounds(arguments[0]);
  let paths = calculate-paths(x-bounds, y-bounds);
  format-out("Max-height %= \n", calculate-max-hieight(paths));
  format-out("Num-initial-velocities  %= \n", size(paths));
  exit-application(0);
end function main;

main(application-name(), application-arguments());
