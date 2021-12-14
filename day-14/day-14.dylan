Module: day-14
Synopsis:
Author:
Copyright:

define function get-intro(file :: <string>) => (directions :: <sequence>, points :: <string-table>)
  let template = make(<stretchy-vector>);
  let insertion-rules = make(<string-table>);
  let before-line-found = #t;
  with-open-file(file-stream = file)
    while (~stream-at-end?(file-stream))
      let line = read-line(file-stream);
      if (line = "")
        before-line-found := #f;
      elseif (before-line-found)
        for (i in line)
          add!(template, i);
        end;
      else
        let line-peices = split(line, " ");
        insertion-rules[line-peices[0]] := line-peices[2][0];
      end;
    end;
  end;
  values(template, insertion-rules);
end;

define function step(template :: <sequence>, rules :: <string-table>) => (template :: <stretchy-vector>)
  let new-template = make(<stretchy-vector>);
  let next-elements = vector(0, template[0]);
  for (i from 1 below size(template))
    add!(new-template, next-elements[1]);
    next-elements[0] := next-elements[1];
    next-elements[1] := template[i];
    add!(new-template, rules[map-as(<string>, identity, next-elements)]);
  end;
  add!(new-template, template[size(template) - 1]);
  new-template;
end;

define function print-max-min(template :: <sequence>) => ()
  let elements = remove-duplicates(template);
  let num-elements = make(<stretchy-vector>);
  for (i from 0 below size(elements))
    let method-id = method (el) if ( el == elements[i] ) 1 else 0 end; end;
    let element-bin = map(method-id, template);
    add!(num-elements, reduce1(\+, element-bin));
  end;
  sort!(num-elements);
  format-out("%= \n", num-elements);
  format-out("Max - Min %d \n", num-elements[size(num-elements) - 1] - num-elements[0]);
end;

define function make-tables(template :: <sequence>) => (pairs-int-table :: <string-table>, char-int-table :: <table>)
  let elements = remove-duplicates(template);
  let char-int-table = make(<table>);

  // Create char => int table
  for (e in elements)
    let method-id = method (el) if ( el == e ) 1 else 0 end; end;
    let element-bin = map(method-id, template);
    char-int-table[e] :=  reduce1(\+, element-bin);
  end;

  // Create the pair => int table
  // We could use one for loop, but it's really simpler this way
  let pairs-int-table = make(<string-table>);
  let next-elements = vector(0, template[0]);
  for (i from 1 below size(template))
    next-elements[0] := next-elements[1];
    next-elements[1] := template[i];
    let pair = map-as(<string>, identity, next-elements);
    pairs-int-table[pair] := element(pairs-int-table, pair, default: 0) + 1;
  end;

  values(pairs-int-table, char-int-table);
end;

define function better-step(pairs-int-table :: <string-table>, char-int-table :: <table>, rules :: <string-table>)
 => (pairs-int-table :: <string-table>, char-int-table :: <table>)
  let new-pairs-int-table = make(<string-table>);
  let new-char-int-table = make(<table>);

  // The number of elements is monotonically increasing
  for (value keyed-by key in char-int-table)
    new-char-int-table[key] := value;
  end;

  for (value keyed-by key in pairs-int-table)
    let new-char = rules[key];
    // format-out("\n new-char : %= new-amount %= \n", new-char, new-amount);

    // Add the new amount of elements
    new-char-int-table[new-char] := element(new-char-int-table, new-char, default: 0) + value;

    // Update the pairs
    let str = format-to-string("%c%c", key[0], new-char);
    new-pairs-int-table[str] := element(new-pairs-int-table, str, default: 0) + value;
    str := format-to-string("%c%c", new-char, key[1]);
    new-pairs-int-table[str] := element(new-pairs-int-table, str, default: 0) + value;

  end;
  // format-out("New char table: %=\n", new-char-int-table);
  values(new-pairs-int-table, new-char-int-table);
end;

define function print-max-min-2(char-int-table :: <table>) => ()
  let min = $maximum-integer;
  let max = $minimum-integer;
  for (value keyed-by key in char-int-table)
    if (value > max)
      max := value
    end;
    if (value < min)
      min := value
    end;
  end;
  format-out("Max - Min %d \n", max - min);
end;


define function main
    (name :: <string>, arguments :: <vector>)
  let (t, r) = get-intro(arguments[0]);
  for (i from 0 below 10)
    t := step(t, r);
  end;
  print-max-min(t);

  let (t, r) = get-intro(arguments[0]);
  let (pairs-table, char-table) = make-tables(t);
  for (i from 0 below 40)
    let (p, c) = better-step(pairs-table, char-table, r);
    pairs-table := p;
    char-table := c;
  end;
  print-max-min-2(char-table);

  exit-application(0);
end function main;

main(application-name(), application-arguments());
