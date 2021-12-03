Module: day-1
Synopsis:
Author:
Copyright:

let file-stream = make(<file-stream>, locator: "list.txt");
let first-time = #t;
let previous-int = 0;
let num-increased = 0;
while (~stream-at-end?(file-stream))
  let line = read-line(file-stream);
  let int = string-to-integer(line);
  if (first-time)
    first-time := #f;
  elseif (previous-int < int)
    num-increased := num-increased + 1;
  end;
  previous-int := int;
end;

format-out("%=\n", num-increased);


file-stream := make(<file-stream>, locator: "list.txt");
let times = 0;
let int1 = 0;
let int2 = 0;
let int3 = 0;
let previous-sum = 0;
num-increased := 0;
while (~stream-at-end?(file-stream))
  int3 := int2;
  int2 := int1;
  let line = read-line(file-stream);
  int1 := string-to-integer(line);
  let cur-sum = int1 + int2 + int3;
  if (times < 3)
    times := times + 1;
  elseif (previous-sum < cur-sum)
    num-increased := num-increased + 1;
  end;
  previous-sum := cur-sum;
end;

format-out("%=\n", num-increased);
