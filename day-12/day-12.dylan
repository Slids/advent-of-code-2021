Module: day-12
Synopsis:
Author:
Copyright:

define function add-to-cave-map(cave-map :: <string-table>, start :: <string>, finish :: <string>) => ()
  unless ( element(cave-map, start, default: #f) )
    element-setter(make(<stretchy-vector>), cave-map, start);
  end;
  add!(cave-map[start], finish);
end;

define function get-cave-map(file :: <string>) => (cave-map :: <string-table>)
  let cave-map = make(<string-table>);
  with-open-file(file-stream = file)
    while (~stream-at-end?(file-stream))
      let line = read-line(file-stream);
      let path = split(line, "-");
      add-to-cave-map(cave-map, path[1], path[0]);
      add-to-cave-map(cave-map, path[0], path[1]);
    end;
  end;
  cave-map;
end;

define function find-num-paths(cave-map :: <string-table>, place :: <string>, visited-places :: <sequence>, small-twice :: <boolean>)
 => (num-paths :: <integer>)
  let num-paths = 0;
  if (lowercase?(place))
    add!(visited-places, place);
  end;
  if (string-equal?(place, "end"))
    num-paths := 1;
  else
    for (next-place in cave-map[place])
      if (member?(next-place, visited-places, test: string-equal?) & ~small-twice)
        num-paths := num-paths + find-num-paths(cave-map, next-place, visited-places, #t);
      elseif (~member?(next-place, visited-places, test: string-equal?))
        num-paths := num-paths + find-num-paths(cave-map, next-place, visited-places, small-twice);
      end;
    end;
  end;
  if (lowercase?(place))
    remove!(visited-places, place, count: 1);
  end;
  num-paths;
end;

define function main
    (name :: <string>, arguments :: <vector>)
  let cave-map = get-cave-map(arguments[0]);
  format-out("P1 Num paths: %= \n", find-num-paths(cave-map, "start", make(<stretchy-vector>), #t));
  for (place in cave-map["start"])
    remove!(cave-map[place], "start", test: string-equal?);
  end;
  remove-key!(cave-map, "end");
  format-out("P2 Num paths: %= \n", find-num-paths(cave-map, "start", make(<stretchy-vector>), #f));
  exit-application(0);
end function main;

main(application-name(), application-arguments());
