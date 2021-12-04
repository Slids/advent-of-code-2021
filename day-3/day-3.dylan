Module: day-3
Synopsis:
Author:
Copyright:

define function set-bit (int :: <integer>, place :: <integer>) => (new-int :: <integer>)
  logior(int, ash(1, place));
end;

// Part 1
let int-size = 0;
let vec-total = make(<vector>, size: int-size, fill: 0);
let num-ints :: <integer> = 0;
let first-time = #t;
with-open-file(file-stream = "list.txt")
  while (~stream-at-end?(file-stream))
    let line = read-line(file-stream);
    when (first-time)
      first-time := #f;
      int-size := size(line);
      vec-total := make(<vector>, size: int-size, fill: 0);
    end;
    num-ints := num-ints + 1;
    for (i from 0 below int-size)
      when (line[i] =  '1')
        vec-total[i] := vec-total[i] + 1;
      end;
    end;
  end;
end;

let gamma = 0;
let epsilon = 0;
for (i from 0 below int-size)
  if (num-ints - vec-total[i] < vec-total[i]) // more 1's
    gamma := set-bit(gamma, int-size - i - 1);
  else // more 0's
    epsilon := set-bit(epsilon, int-size - i - 1)
  end;
end;

format-out("Part 1: Gamma: %b Epsilon: %b Mult: %= \n",
           gamma, epsilon, gamma * epsilon);

define function get-list () => (string-list :: <list>)
  let string-list = make(<list>);
  let first-time = #t;
  with-open-file(file-stream = "list.txt")
    while (~stream-at-end?(file-stream))
      let line = read-line(file-stream);
      string-list := add!(string-list, line);
    end;
  end;
  string-list;
end;

define function most-common-bit(string-list :: <list>, index :: <integer>) => (c :: <character>)
  let mapped = map(method (a) string-to-integer(a, start: index, end: index + 1); end, string-list);
  let int = reduce(\+, 0, mapped);
  if (int >= size(string-list) - int)
    '1';
  else
    '0';
  end;
end;

let oxygen-generator-rating = 0;
let string-list = get-list();
block(break)
  for ( i from 0 below size(string-list[0]))
    let common-bit = most-common-bit(string-list, i);
    string-list := choose ( method (a) a[i] = common-bit end, string-list);
    if (size(string-list) = 1)
      oxygen-generator-rating := string-to-integer(string-list[0], base: 2);
      break();
    end;
  end;
end;

let c02-scrubbing = 0;
string-list := get-list();
block(break)
  for ( i from 0 below size(string-list[0]))
    let common-bit = most-common-bit(string-list, i);
    string-list := choose ( method (a) a[i] ~= common-bit end, string-list);
    if (size(string-list) = 1)
      c02-scrubbing := string-to-integer(string-list[0], base: 2);
      break();
    end;
  end;
end;

format-out("Part 2: oxygen-generator-rating: %= c02-scrubbing: %= mult: %=\n",
           oxygen-generator-rating, c02-scrubbing, c02-scrubbing * oxygen-generator-rating);
