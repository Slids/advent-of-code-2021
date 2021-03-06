Module: day-13
Synopsis:
Author:
Copyright:

define function get-intro(file :: <string>) => (directions :: <sequence>, points :: <sequence>)
  let points = make(<stretchy-vector>);
  let fold-directions = make(<stretchy-vector>);
  let before-line-found = #t;
  with-open-file(file-stream = file)
    while (~stream-at-end?(file-stream))
      let line = read-line(file-stream);
      if (line = "")
        before-line-found := #f;
      elseif (before-line-found)
        add!(points, map(string-to-integer, split(line, ",")))
      else
        let line-split = split(line, "=");
        let x-or-y = line-split[0][size(line-split[0]) - 1];
        let line-to-fold = string-to-integer(line-split[1]);
        add!(fold-directions, vector(x-or-y, line-to-fold));
      end
    end;
  end;
  values(fold-directions, points);
end;

define function get-max-x-y(points :: <sequence>)
  let max-x = reduce1(max, map(first, points));
  let max-y = reduce1(max, map(second, points));
  vector(max-x, max-y)
end;

define function fold-paper(x-or-y :: <character>, number :: <integer>, points :: <sequence>)
 => (new-points :: <sequence>)
  let ind = if (x-or-y = 'x') 0 else 1 end;
  local method fold (value)
          if (value[ind] < number)
            value;
          else
            value[ind] := (2 * number) - value[ind];
            value;
          end;
        end;
  map-into(points, fold, points);
  remove-duplicates!(points, test: \=);
end;

define function print-points(points :: <sequence>) => ()
  let max-x-y = get-max-x-y(points);
  for (y from 0 to max-x-y[1])
    for (x from 0 to max-x-y[0])
      if (member?(vector(x, y), points, test: \=))
        format-out("#");
      else
        format-out(" ");
      end;
    end;
    format-out("\n");
  end;
end;

define function main
    (name :: <string>, arguments :: <vector>)
  let (d, p) = get-intro(arguments[0]);
  p := fold-paper(d[0][0], d[0][1], p);
  format-out("Num Points Remaining %=\n", size(p), p);
  for (instruction in d)
    p := fold-paper(instruction[0], instruction[1], p);
  end;
  format-out("Num Points Remaining %=\n", size(p), p);
  print-points(p);
  exit-application(0);
end function main;

main(application-name(), application-arguments());
