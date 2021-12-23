Module: day-22
Synopsis:
Author:
Copyright:

define class <cuboid> (<object>)
  slot x :: <sequence>;
  slot y :: <sequence>;
  slot z :: <sequence>;
end;

define class <operation> (<cuboid>)
  slot on? :: <boolean>;
end;

define function kinda-hash-3d(x :: <integer>, y :: <integer>, z :: <integer>) => (_ :: <integer>)
    x + (y * 102) + (z * 102^2);
end;

define function read-ops(file :: <string>) => (players :: <sequence>)
  let op-vec = make(<stretchy-vector>);
  with-open-file(file-stream = file)
    while (~stream-at-end?(file-stream))
      let line = read-line(file-stream);
      let op = make(<operation>);

      op.on? := (line[1] == 'n');

      // x
      let split-line = split(line, "=");
      let xes = split(split-line[1], "..");
      op.x := vector(string-to-integer(xes[0]), string-to-integer(xes[1], end: size(xes[1]) - 2));
      // y
      let yes = split(split-line[2], "..");
      op.y := vector(string-to-integer(yes[0]), string-to-integer(yes[1], end: size(yes[1]) - 2));
      // z
      let zes = split(split-line[3], "..");
      op.z := vector(string-to-integer(zes[0]), string-to-integer(zes[1]));
      add!(op-vec, op);
    end;
  end;
  op-vec;
end;

define function turn-on/off(t :: <table>, op :: <operation>, min-max :: <integer>)
  let my-min = -1 * min-max;
  let my-max = min-max;
  for (x from max(my-min, op.x[0]) to min(my-max, op.x[1]))
    for (y from max(my-min, op.y[0]) to min(my-max, op.y[1]))
      for (z from max(my-min, op.z[0]) to min(my-max, op.z[1]))
        let hash-val = kinda-hash-3d(x, y, z);
        t[hash-val] := op.on?;
      end;
    end;
  end;
end;

define function naive-compute (ops :: <sequence>, min-max :: <integer>) => (_ :: <integer>)
  let on-table = make(<table>);
  for (op in ops)
    turn-on/off(on-table, op, min-max);
  end;
  let num-on = 0;
  for (value keyed-by key in on-table)
    if (value)
      num-on := num-on + 1;
    end;
  end;
  num-on;
end;

define function compute-1d-intersection(x1 :: <sequence>, x2 :: <sequence>) => (i :: <sequence>)
    if (x1[0] > x2[0])
      let t = x1;
      x1 := x2;
      x2 := t;
    end;
  let left-bound = max(x1[0] , x2[0]);
  let right-bound = min(x1[1] , x2[1]);
  if (left-bound > right-bound)
    #[];
  else
    vector(left-bound, right-bound);
  end;
end;

define constant <maybe-cuboid> = type-union(<boolean>, <cuboid>);

define function cuboid-intersection (c1 :: <cuboid>, c2 :: <cuboid>) => (res :: <maybe-cuboid>)
  let x-int = compute-1d-intersection(c1.x, c2.x);
  let y-int = compute-1d-intersection(c1.y, c2.y);
  let z-int = compute-1d-intersection(c1.z, c2.z);

  if (size(x-int) > 0 & size(y-int) > 0 & size(z-int) > 0)
    let c = make(<cuboid>);
    c.x := x-int;
    c.y := y-int;
    c.z := z-int;
    c;
  else
    #f;
  end
end;

// We assumes c2 is non-empty and contained in c1
define function cuboid-minus(c1 :: <cuboid>, c2 :: <cuboid>) => (res :: <sequence>)
  // The z portion
  let new-cuboids = make(<stretchy-vector>);
  if (c2.z[1] < c1.z[1])
    let new-cube = make(<cuboid>);
    new-cube.x := c1.x;
    new-cube.y := c1.y;
    new-cube.z := vector(c2.z[1] + 1, c1.z[1]);
    add!(new-cuboids, new-cube);
  end;
  if (c1.z[0] < c2.z[0])
    let new-cube = make(<cuboid>);
    new-cube.x := c1.x;
    new-cube.y := c1.y;
    new-cube.z := vector(c1.z[0], c2.z[0] - 1);
    add!(new-cuboids, new-cube);
  end;
  let z-vec = vector(c2.z[0], c2.z[1]);
  // At this point we don't consider any part
  // of c1 above/below c2 in the z-axis

  if (c2.x[1] < c1.x[1])
    let new-cube = make(<cuboid>);
    new-cube.x := vector(c2.x[1] + 1, c1.x[1]);
    new-cube.y := c1.y;
    new-cube.z := z-vec;
    add!(new-cuboids, new-cube);
  end;
  if (c1.x[0] < c2.x[0])
    let new-cube = make(<cuboid>);
    new-cube.x := vector(c1.x[0], c2.x[0] - 1);
    new-cube.y := c1.y;
    new-cube.z := z-vec;
    add!(new-cuboids, new-cube);
  end;
  let x-vec = vector(c2.x[0], c2.x[1]);
  // At this point we don't consider any part
  // of c1 above/below c2 in the x-axis

  if (c1.y[1] > c2.y[1])
    let new-cube = make(<cuboid>);
    new-cube.x := x-vec;
    new-cube.y := vector(c2.y[1] + 1, c1.y[1]);
    new-cube.z := z-vec;
    add!(new-cuboids, new-cube);
  end;
  if (c1.y[0] < c2.y[0])
    let new-cube = make(<cuboid>);
    new-cube.x := x-vec;
    new-cube.y := vector(c1.y[0], c2.y[0] - 1);
    new-cube.z := z-vec;
    add!(new-cuboids, new-cube);
  end;
  new-cuboids;
end;

define function check-distinct (cuboids :: <sequence>, str :: <string>) => ()
  for (c in cuboids)
    for (d in cuboids)
      unless (c = d)
        let intersect = cuboid-intersection(c, d);
        if (intersect)
          format-out("Error in %s: \n", str);
          print-cuboid(c);
          print-cuboid(d);
          format-out("Done Error: \n");
          error("");
        end;
      end
    end;
  end;
end;


// We assume current cuboids are distinct and non-intersecting
define function on-cuboid(new-cuboid :: <cuboid>, current-cuboids :: <sequence>) => (_ :: <sequence>)
  // check-distinct(current-cuboids, "on");
  let remaining-new-cuboid-set = make(<stretchy-vector>);
  add!(remaining-new-cuboid-set, new-cuboid);
  block (break)
    for (current-cuboid in current-cuboids)
      let temp-remaining-cuboid-set = make(<stretchy-vector>);
      for(new-cuboid in remaining-new-cuboid-set)
        let intersection = cuboid-intersection(new-cuboid, current-cuboid);
        if (intersection)
          let remaining = cuboid-minus(new-cuboid, intersection);

          if (size(remaining) > 0)
            temp-remaining-cuboid-set := concatenate(temp-remaining-cuboid-set, remaining);
          end;
        else
          add!(temp-remaining-cuboid-set, new-cuboid);
        end;
      end;
      remaining-new-cuboid-set := temp-remaining-cuboid-set;
      if (size(temp-remaining-cuboid-set) = 0)
        break();
      end;
    end;
  end;
  concatenate(remaining-new-cuboid-set, current-cuboids);
end;

// We assume current cuboids are distinct and non-intersecting
define function off-cuboid(new-cuboid :: <cuboid>, current-cuboids :: <sequence>) => (still-on-cuboids :: <sequence>)
  let still-on-cuboids = make(<stretchy-vector>);
  for (current-cuboid in current-cuboids)
    let intersection = cuboid-intersection(current-cuboid, new-cuboid);
    if (intersection)
      let remaining = cuboid-minus(current-cuboid, intersection);
      if (size(remaining) > 0)
        still-on-cuboids := concatenate(still-on-cuboids, remaining);
      end;
    else
      add!(still-on-cuboids, current-cuboid);
    end;
  end;
  still-on-cuboids;
end;

define function print-cuboid(c :: <cuboid>) => ()
  force-out();
  let sum-cuboid = method (c) (1 + c.x[1] - c.x[0]) * (1 + c.y[1] - c.y[0]) * (1 + c.z[1] - c.z[0]) end;
  format-out( "Cuboid: \n x: %= y: %= z: %= sum %d \n", c.x, c.y, c.z, sum-cuboid(c));
  force-out();
end;

define function on-sum(cs :: <sequence>) => (i :: <integer>)
  let sum-cuboid = method (c) (1 + c.x[1] - c.x[0]) * (1 + c.y[1] - c.y[0]) * (1 + c.z[1] - c.z[0]) end;
  reduce1(\+, map(sum-cuboid, cs))
end;

define function part-2 (ops :: <sequence>) => ()
  let cuboid-set = make(<stretchy-vector>);
  for (op in ops)
    if (op.on?)
      cuboid-set := on-cuboid(op, cuboid-set);
    else
      cuboid-set := off-cuboid(op, cuboid-set);
    end;
  end;
  format-out("Total on: %=\n", on-sum(cuboid-set));
end;

define function main
    (name :: <string>, arguments :: <vector>)
  let ops = read-ops(arguments[0]);
  let num-on = naive-compute(ops, 50);
  format-out("Part 1 Number on %=\n", num-on);
  // let num-on = naive-compute(ops, $maximum-integer);
  ops := read-ops(arguments[0]);
  part-2(ops);
  exit-application(0);
end function main;

main(application-name(), application-arguments());
