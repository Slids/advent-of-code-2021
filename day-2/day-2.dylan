Module: day-2
Synopsis:
Author:
Copyright:

let vertical = 0;
let horizontal = 0;
with-open-file(file-stream = "list.txt")
  while (~stream-at-end?(file-stream))
    let line = read-line(file-stream);
    let elements = split(line, " ");
    let int = string-to-integer(elements[1]);
    select (elements[0] by string-equal?)
      "forward"  => horizontal := horizontal + int;
      "down" => vertical := vertical + int;
      "up" => vertical := vertical - int;
      otherwise =>
        error("Invalid movement")
    end;
  end;
end;

format-out("Part 1: Verical: %= Horizontal: %= Mult: %= \n",
           vertical, horizontal, vertical * horizontal);


vertical := 0;
horizontal := 0;
let aim = 0;
with-open-file(file-stream = "list.txt")
  while (~stream-at-end?(file-stream))
    let line = read-line(file-stream);
    let elements = split(line, " ");
    let int = string-to-integer(elements[1]);

    select (elements[0] by string-equal?)
      "forward"  =>
        horizontal := horizontal + int;
        vertical := vertical + aim * int;
      "down" => aim := aim + int;
      "up" => aim := aim - int;
      otherwise =>
        error("Invalid movement")
    end;
  end;
end;

format-out("Part 2: Verical: %= Horizontal: %= Mult: %= \n",
           vertical, horizontal, vertical * horizontal);
