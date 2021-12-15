Module: day-15
Synopsis:
Author:
Copyright:

define function get-cave-map (file :: <string>) => (map :: <sequence>)
  let cave-map = make(<stretchy-vector>);
  with-open-file(file-stream = file)
    while (~stream-at-end?(file-stream))
      let line = read-line(file-stream);
      add!(cave-map, get-int-vector-from-string(line));
    end;
  end;
  cave-map;
end;

define function make-big-cave-map (cave-map :: <sequence>) => (big-cave-map :: <sequence>)
  let big-cave-map = make(<stretchy-vector>);
  for (i from 0 below 5 * size(cave-map))
    add!(big-cave-map, make(<stretchy-vector>, size: size(cave-map[0]) * 5));
  end;

  for (j from 0 below 5)
    for (l from 0 below 5)
      for (i from 0 below size(cave-map))
        for (k from 0 below size(cave-map[i]))
          let mapped-i = i + (size(cave-map) * j);
          let mapped-k = k + (size(cave-map[i]) * l);
          let unwrapped = (cave-map[i][k] + j + l);
          let wrapped-v1 = modulo(unwrapped, 10);
          let wrapped-value = if (unwrapped > 9) wrapped-v1 + 1 else wrapped-v1 end;
          big-cave-map[mapped-i][mapped-k] := wrapped-value;
        end;
      end;
    end;
  end;
  big-cave-map;
end;

define function get-smallest(possible-paths :: <sequence>) => (pos-q-element :: <sequence>)
  let min-value = method (a, b) if (a[1] < b[1]) a else b end; end;
  reduce1(min-value, possible-paths);
end;

define function copy-map(cave-map :: <sequence>, default-value) => (new-map :: <sequence>)
  let new-map = make(<vector>, size: size(cave-map));
  for (i from 0 below size(cave-map))
    new-map[i] := make(<vector>, size: size(cave-map[i]), fill: default-value);
  end;
  new-map;
end;

define function find-path(cave-map :: <sequence>) => (path-cost :: <integer>)
  let pos-q-element = #[#[0,0], 0];
  let visited = copy-map(cave-map, #f);
  let possible-paths = make(<stretchy-vector>);
  let pos-equal = method (a, b) a[0] == b[0] end;
  add!(possible-paths, pos-q-element);

  while (pos-q-element[0] ~= vector(size(cave-map[0]) - 1, size(cave-map[1]) - 1)
           & size(possible-paths) > 0)
    let pos = pos-q-element[0];
    unless ( visited[pos[0]][pos[1]])
      for (new-pos in get-neighbors(cave-map, pos-q-element[0]))
        let new-path-cost = pos-q-element[1] + cave-map[new-pos[0]][new-pos[1]];

        if ( ~visited[new-pos[0]][new-pos[1]] )
          let new-pos-q-element = vector(new-pos, new-path-cost);
          add!(possible-paths, new-pos-q-element);
        end;
      end;
    end;
    visited[pos[0]][pos[1]] := #t;
    pos-q-element := get-smallest(possible-paths);
    remove!(possible-paths, pos-q-element, test: pos-equal);
  end;
  pos-q-element[1];
end;

define function main
    (name :: <string>, arguments :: <vector>)
  let cave-map = get-cave-map(arguments[0]);
  let min-cost = find-path(cave-map);
  format-out("min-cost: %= \n", min-cost);

  let bcm = make-big-cave-map(cave-map);
  min-cost := find-path(bcm);
  format-out("min-cost: %= \n", min-cost);
  exit-application(0);
end function main;

main(application-name(), application-arguments());
