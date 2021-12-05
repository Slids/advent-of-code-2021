Module: day-5
Synopsis:
Author:
Copyright:

define function get-int-vector-from-string(str :: <string>) => (ns :: <sequence>)
  map(\string-to-integer, split(str, ",", remove-if-empty?: #t))
end;

define function sort-coords(c1 :: <sequence>, c2 :: <sequence>) => (c1 :: <sequence>, c2 :: <sequence>)
  // c1 should be to the left
  if (c1[0] > c2[0])
    let ct = c1;
    c1 := c2;
    c2 := ct;
  elseif (c1[0] = c2[0]) // if equal then c1 is lower
    when (c1[1] > c2[1])
      let ct = c1;
      c1 := c2;
      c2 := ct;
    end;
  end;
  values(c1,c2);
end;

define function get-coords(line :: <string>)
 => (first-coord :: <sequence>, second-coord :: <sequence>)
  let first-split = split(line, " ");
  let first-coord = get-int-vector-from-string(first-split[0]);
  let second-coord = get-int-vector-from-string(first-split[2]);
  sort-coords(first-coord, second-coord);
end;

define function coord-to-string(c1 :: <integer>, c2 :: <integer>) => (s :: <String>)
  concatenate(integer-to-string(c1), ",", integer-to-string(c2));
end;

define function add-line-points-to-table!(c1 :: <sequence>, c2 :: <sequence>, table :: <table>,
                                          allow-diagonals :: <boolean>) => ()
  let rise :: <integer> = c2[1] - c1[1];
  let run :: <integer> = c2[0] - c1[0];
  if (run = 0)
    for (i from c1[1] to c2[1])
      let vec-string = coord-to-string(c1[0], i);
      table[vec-string] := element(table, vec-string, default: 0) + 1;
    end;
  elseif (rise = 0 | allow-diagonals)
    let slope = floor/(rise, run);
    //format-out("\n\n");
    let step = 0;
    for (i from c1[0] to c2[0])
      let vec-string = coord-to-string(i, c1[1] + (step * slope));
      //format-out("%= %d\n", vec-string, slope);
      table[vec-string] := element(table, vec-string, default: 0) + 1;
      step := step + 1;
    end;
  end;
end;

// Part 1
let position-map = make(<string-table>);
with-open-file(file-stream = "list.txt")
  while (~stream-at-end?(file-stream))
    let line = read-line(file-stream);
    let (c1, c2) = get-coords(line);
    add-line-points-to-table!(c1, c2, position-map, #f);
  end;
end;
let num-over-1 = 0;
for (value keyed-by key in position-map)
  if (value >= 2)
    num-over-1 := num-over-1 + 1;
  end;
end;
format-out("Positions at least 2 %d \n", num-over-1);

// Part 2
remove-all-keys!(position-map);
with-open-file(file-stream = "list.txt")
  while (~stream-at-end?(file-stream))
    let line = read-line(file-stream);
    let (c1, c2) = get-coords(line);
    add-line-points-to-table!(c1, c2, position-map, #t);
  end;
end;
num-over-1 := 0;
for (value keyed-by key in position-map)
  //format-out("key %= \n", key);
  if (value >= 2)
    num-over-1 := num-over-1 + 1;
  end;
end;
format-out("Positions at least 2 %d \n", num-over-1);
