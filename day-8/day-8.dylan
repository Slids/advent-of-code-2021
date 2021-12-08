Module: day-8
Synopsis:
Author:
Copyright:

let *num-int-segments* = #[6,2,5,5,4,5,6,3,7,6];

define function count-match(str :: <string>) => (int :: <integer>)
  let size-str = size(str);
  if (size-str = *num-int-segments*[1] | size-str = *num-int-segments*[4] |
        size-str = *num-int-segments*[7] | size-str = *num-int-segments*[8])
    1;
  else
    0;
  end;
end;

let total-with-unique-seg = 0;
with-open-file(file-stream = "list.txt")
  while (~stream-at-end?(file-stream))
    let line = read-line(file-stream);
    let output = split(line, "|")[1];
    let output-sequence = split(output, " ", remove-if-empty?: #t);
    total-with-unique-seg := total-with-unique-seg +
      reduce1(\+, map(count-match, output-sequence));
  end;
end;

// define function print-table (t) => ()
//   for (value keyed-by key in t)
//     format-out("key %= value %= \n", key,value);
//   end;
// end;

define function intersection-to-string(s1 :: <string>, s2 :: <string>) => (s :: <string>)
  let my-str = "";
  for (i in intersection(s1,s2))
    my-str := format-to-string("%s%c", my-str, i);
  end;
  sort(my-str)
end;


define function determine-string-map(input-str :: <sequence>) => (str-table :: <string-table>)
  let string-table = make(<string-table>);
  // the easy part
  let one = find-element(input-str, method (a :: <string>) size(a) = 2; end);
  input-str := remove(input-str, one);
  string-table[sort(one)] := 1;
  let seven = find-element(input-str, method (a :: <string>) size(a) = 3; end);
  input-str := remove(input-str, seven);
  string-table[sort(seven)] := 7;
  let four = find-element(input-str, method (a :: <string>) size(a) = 4; end);
  input-str := remove(input-str, four);
  string-table[sort(four)] := 4;
  let eight = find-element(input-str, method (a :: <string>) size(a) = 7; end);
  input-str := remove(input-str, eight);
  string-table[sort(eight)] := 8;
  // six element ones
  let nine = "";
  block (e)
    for (i in input-str)
      if (size(i) = 6 & intersection-to-string(four, i) = sort(four))
        nine := i;
        input-str := remove(input-str, nine);
        string-table[sort(nine)] := 9;
        e();
      end;
    end;
  end;
  let zero = "";
  block (e)
    for (i in input-str)
      if (size(i) = 6 & intersection-to-string(one, i) = sort(one))
        zero := i;
        input-str := remove(input-str, zero);
        string-table[sort(zero)] := 0;
        e();
      end;
    end;
  end;
  let six = find-element(input-str, method (a :: <string>) size(a) = 6; end);
  input-str := remove(input-str, six);
  string-table[sort(six)] := 6;
  let three = "";
  block (e)
    for (i in input-str)
      if (size(intersection(one, i)) = 2)
        three := i;
        input-str := remove(input-str, three);
        string-table[sort(three)] := 3;
        e()
      end;
    end;
  end;
  let five = "";
  block (e)
    for (i in input-str)
      if (intersection-to-string(six, i) = sort(i))
        five := i;
        input-str := remove(input-str, five);
        string-table[sort(five)] := 5;
        e();
      end;
    end;
  end;
  string-table[sort(input-str[0])] := 2;
  // print-table(string-table);
  // format-out("------------------\n");
  string-table;
end;

define function int-seq-to-int (int-seq :: <sequence>, t :: <string-table>) => (int :: <integer>)
  let desired-int = 0;
  for (i from 0 below size(int-seq))
    let my-int = t[sort(int-seq[i])];
    desired-int := desired-int * 10;
    desired-int := desired-int + my-int;
  end;
  desired-int;
end;

let total = 0;
with-open-file(file-stream = "list.txt")
  while (~stream-at-end?(file-stream))
    let line = read-line(file-stream);
    let output = split(line, "|");
    let input-sequence = split(output[0], " ", remove-if-empty?: #t);
    let t = determine-string-map(input-sequence);
    let output-sequence = split(output[1], " ", remove-if-empty?: #t);
    total := total + int-seq-to-int(output-sequence, t)
  end;
end;


format-out("Part 1 number of outputs: %d \n", total-with-unique-seg);
format-out("Part 2 total: %d \n", total)
