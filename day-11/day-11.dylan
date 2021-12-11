Module: day-11
Synopsis:
Author:
Copyright:

define function increment-neighbors(oct-map :: <sequence>, point :: <sequence>) => ()
  for (i in #[-1, 0, 1])
    for (j in #[-1, 0, 1])
      unless (i = 0 & j = 0)
        let new-x = point[0] + i;
        let new-y = point[1] + j;
        if (0 <= new-x & new-x < size(oct-map) &
              0 <= new-y & new-y < size(oct-map[0]))
          oct-map[new-x][new-y] := oct-map[new-x][new-y] + 1;
        end;
      end;
    end;
  end
end;

define function flashing-points (oct-map :: <sequence>, flash-map :: <sequence>) => (points :: <sequence>)
  let flashed = make(<stretchy-vector>);
  for (i from 0 below size(oct-map))
    for (j from 0 below size(oct-map[0]))
      // If we should flash and yet to flash
      if  (oct-map[i][j] > 9 & ~flash-map[i][j])
        increment-neighbors(oct-map, vector(i, j));
        add!(flashed, vector(i, j));
        flash-map[i][j] := #t;
      end;
    end;
  end;
  flashed;
end;

define function increment-all (oct-map :: <sequence>) => ()
  for (i from 0 below size(oct-map))
    for (j from 0 below size(oct-map[0]))
      oct-map[i][j] := oct-map[i][j] + 1;
    end;
  end;
end;

define function make-flash-map (oct-map :: <sequence>) => (flash-map :: <sequence>)
  let flash-map = make(<vector>, size: size(oct-map));
  for (i from 0 below size(oct-map))
    flash-map[i] := make(<vector>, size: size(oct-map[i]));
  end;
  flash-map;
end;

define function zero-flashed (oct-map :: <sequence>) => ()
  for (i from 0 below size(oct-map))
    for (j from 0 below size(oct-map[0]))
      if ( oct-map[i][j] > 9)
        oct-map[i][j] := 0;
      end;
    end;
  end;
end;

define function step(oct-map :: <sequence>) => (num-flashes :: <integer>)
  let flash-map = make-flash-map(oct-map);
  increment-all(oct-map);
  let flashed = flashing-points(oct-map, flash-map);
  let num-flashed = size(flashed);
  while (size(flashed) > 0)
    flashed := flashing-points(oct-map, flash-map);
    num-flashed := num-flashed + size(flashed);
  end;
  zero-flashed(oct-map);
  num-flashed;
end;

define function get-int-vector-from-string(str :: <string>) => (ns :: <sequence>)
  let vec = make(<stretchy-vector>);
  for (i in str)
    add!(vec, string-to-integer(format-to-string("%c", i)));
  end;
  vec;
end;

define function get-oct-map () => (oct-map :: <sequence>)
  let oct-map = make(<stretchy-vector>);
  with-open-file(file-stream = "list.txt")
    while (~stream-at-end?(file-stream))
      let line = read-line(file-stream);
      add!(oct-map, get-int-vector-from-string(line));
    end;
  end;
  oct-map;
end;

let oct-map = get-oct-map();
let num-flashed = 0;
for (i from 0 below 100)
  num-flashed := num-flashed + step(oct-map);
end;
format-out("Num-Flash: %d\n", num-flashed);

oct-map := get-oct-map();
let step-number = 0;
num-flashed := 0;
while (num-flashed ~= 100)
  num-flashed := step(oct-map);
  step-number := step-number + 1;
end;
format-out("All flashed at: %d\n", step-number);
