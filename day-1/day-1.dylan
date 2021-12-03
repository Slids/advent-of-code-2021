Module: day-1

define function main ()
  with-open-file (stream = "list.txt")
    let previous-int = #f;
    let num-increased = 0;
    while (~stream-at-end?(stream))
      let line = read-line(stream);
      let int = string-to-integer(line);
      if (previous-int & previous-int < int)
        num-increased := num-increased + 1;
      end;
      previous-int := int;
    end;
    format-out("%=\n", num-increased);
  end;
end function;

main();
