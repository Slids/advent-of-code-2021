Module: day-20
Synopsis:
Author:
Copyright:

define function make-binary (str :: <string>) => (_ :: <string>)
  for (i from 0 below size(str))
    if (str[i] == '.')
      str[i] := '0';
    else
      str[i] := '1';
    end
  end;
  str;
end;

define function print-image (image :: <sequence>) => ()
  format-out("image:\n");
  for (i in image)
    format-out("  %=\n", i);
  end;
end;

define function read-input (file :: <string>) => (str :: <string>, image :: <sequence>)
  with-open-file(file-stream = file)
    let image-enhancement-algorithm = make-binary(read-line(file-stream));
    // Blank
    read-line(file-stream);
    let image = make(<stretchy-vector>);
    while (~stream-at-end?(file-stream))
      add!(image, make-binary(read-line(file-stream)));
    end;
    values(image-enhancement-algorithm, image);
  end;
end;

define function add-fill-around-twice (image :: <sequence>, fill :: <character>) => (larger-image :: <sequence>)
  let col-size = size(image);
  let row-size = size(image[0]);
  let new-image = make(<stretchy-vector>);
  add!(new-image, make(<string>, size: row-size + 4, fill: fill));
  add!(new-image, make(<string>, size: row-size + 4, fill: fill));
  for (i in image)
    let new-i = format-to-string("%c%c%s%c%c", fill, fill, i, fill, fill);
    add!(new-image, new-i);
  end;
  add!(new-image, make(<string>, size: row-size + 4, fill: fill));
  add!(new-image, make(<string>, size: row-size + 4, fill: fill));
  new-image;
end;

define function get-3-3-box-int(image :: <sequence>, row :: <integer>, col :: <integer>) => (int :: <integer>)
  let rows = make(<stretchy-vector>);
  for (i from row - 1 to row + 1)
    add!(rows, copy-sequence(image[i], start: col - 1, end: col + 2));
  end;
  // print-image(rows);
  let int-string = format-to-string("%s%s%s", rows[0], rows[1], rows[2]);
  string-to-integer(int-string, base: 2);
end;

define function increase-level(iea :: <string>, image :: <sequence>, #key first? = #f) => (new-image :: <sequence>)
  let fill = image[0][0];
  // format-out("fill: %c\n", fill);
  let larger-image = add-fill-around-twice(image, fill);
  let copy-larger-image = map(copy-sequence, larger-image);
  for (row from 0 below size(larger-image))
    for (col from 0 below size(larger-image[0]))
      let iea-index = 0;
      if (row = 0
            | col = 0
            | row = (size(larger-image) - 1)
            | col = (size(larger-image[0]) - 1))
        if (fill = '1')
          // format-out ("iea-index 511 char: %c\n", iea[iea-index]);
          iea-index := 511;
        end;
      else
        iea-index := get-3-3-box-int(larger-image, row, col);
      end;
      let iea-char = iea[iea-index];
      copy-larger-image[row][col] := iea-char;
    end;
  end;
  //print-image(copy-larger-image);
  copy-larger-image;
end;

define function count-on(image :: <sequence>) => (count :: <integer>)
  //let int-image = map(get-int-vector-from-string, image);
  //let int-values = map(method (a) reduce1(\+, a) end, int-image);
  //reduce1(\+, int-values);
  let sum = 0;
  for (i in image)
    for (j in i)
      if (j = '1')
        sum := sum + 1;
      end;
    end;
  end;
  sum
end;

define function part-1 (filename) => ()
  let (iea, image) = read-input(filename);
  image := add-fill-around-twice(image, '0');
  image := increase-level(iea, image, first?: #t);
  for (i from 1 below 2)
    image := increase-level(iea, image);
  end;
  format-out("Part 1 On count: %d \n", count-on(image));
end;

define function part-2 (filename) => ()
  let (iea, image) = read-input(filename);
  image := add-fill-around-twice(image, '0');
  image := increase-level(iea, image, first?: #t);
  for (i from 1 below 50)
    image := increase-level(iea, image);
  end;
  format-out("Part 2 On count: %d \n", count-on(image));
end;

define function main
    (name :: <string>, arguments :: <vector>)
  part-1(arguments[0]);
  part-2(arguments[0]);
  exit-application(0);
end function main;

main(application-name(), application-arguments());
