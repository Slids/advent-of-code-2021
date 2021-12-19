Module: day-18
Synopsis:
Author:
Copyright:


define class <node> (<object>)
  slot value, init-value: #f;
  // These two will either be false or node
  slot left, init-value: #f;
  slot right, init-value: #f;
end;

define function make-number-tree (str :: <string>, pos :: <integer>, n :: <node>) => (pos :: <integer>)
  let break = #f;
  let left = #t;
  while (~break)
    select (str[pos])
      '[' =>
        let new-node = make(<node>);
        if (left)
          n.left := new-node;
        else
          n.right := new-node;
        end;
        pos := make-number-tree(str, pos + 1, new-node);
      ']' =>
        break := #t;
        pos := pos + 1;
      ',' =>
        pos := pos + 1;
        left := #f;
      otherwise =>
        let new-node = make(<node>);
        new-node.value := string-to-integer(str, start: pos, end: pos + 1);
        if (left)
          n.left := new-node;
        else
          n.right := new-node;
        end;
        pos := pos + 1;
    end;
  end;
  pos;
end;

define function read-numbers (file :: <string>) => (numbers :: <sequence>)
  let numbers = make(<stretchy-vector>);
  with-open-file(file-stream = file)
    while (~stream-at-end?(file-stream))
      let str = read-line(file-stream);
      let number = make(<node>);
      make-number-tree(str, 1, number);
      add!(numbers, number);
    end;
  end;
  numbers;
end;

define function read-strings  (file :: <string>) => (numbers :: <sequence>)
  let numbers = make(<stretchy-vector>);
  with-open-file(file-stream = file)
    while (~stream-at-end?(file-stream))
      let str = read-line(file-stream);
      add!(numbers, str);
    end;
  end;
  numbers;
end;

define function print-number(n :: <node>, level :: <integer>) => ()
  format-out("%s", make(<string>, size: 2 * level));
  if (n.value)
    format-out("value %= \n", n.value);
  else
    format-out("non-leaf\n");
    if (n.left)
      print-number(n.left, level + 1);
    end;
    if (n.right)
      print-number(n.right, level + 1);
    end;
    end;
end;

define function try-split-number(n :: <node>) => (split :: <boolean>)
  if (n.value & n.value > 9)
    let left = make(<node>);
    let right = make(<node>);
    left.value := floor/(n.value,2);
    right.value := ceiling/(n.value,2);
    n.value := #f;
    n.left := left;
    n.right := right;
    #t;
  elseif (n.value)
    #f;
  elseif (try-split-number(n.left) | try-split-number(n.right))
    #t;
  else
    #f;
  end;
end;

define function set-exploded-values(previous-value-node, current-node :: <node>, exploded-node :: <node>,
                                    left-value :: <integer>, right-value :: <integer>, looking-for-next :: <boolean>) =>
    (last-value-exploded :: <boolean>, last-value-node)
  let last-value-exploded = #f;
  let last-value-node = #f;
  let continue = #f;
  if (current-node == exploded-node)
    if (previous-value-node)
      previous-value-node.value := previous-value-node.value + left-value;
    end;
    last-value-exploded := #t;
    continue := #t;
  elseif (looking-for-next & current-node.value)
    current-node.value := current-node.value + right-value;
    last-value-exploded := #f;
    continue := #t;
  elseif (current-node.value)
    last-value-node := current-node;
    continue := #t;
  end;


  if (~continue)
    let (lvt, lvn) = set-exploded-values(previous-value-node, current-node.left, exploded-node, left-value, right-value, looking-for-next);
    last-value-exploded := lvt;
    last-value-node := lvn;
    let (lvt-2, lvn-2) = set-exploded-values(last-value-node, current-node.right, exploded-node, left-value, right-value, last-value-exploded := lvt);
    last-value-exploded := lvt-2;
    last-value-node := lvn-2;
  end;

  values(last-value-exploded, last-value-node);
end;

define function try-explode-child(n :: <node>, level :: <integer>, p :: <node>) => (exploded :: <boolean>)
  let ex-right = n.right.value;
  let ex-left = n.left.value;
  // I can only explode arrays.
  if (n.left.value & n.right.value & level > 3)
    n.left := #f;
    n.right := #f;
    n.value := 0;
    set-exploded-values(#f, p, n, ex-left, ex-right, #f);
    #t;
  elseif (n.left.value & n.right.value)
    #f;
  elseif (~n.left.value & try-explode-child(n.left, level + 1, p))
    #t;
  elseif (~n.right.value & try-explode-child(n.right, level + 1, p))
    #t;
  else
    #f;
  end;
end;

define function add-number(n1 :: <node>, n2 :: <node>) => (number :: <node>)
  let add-node = make(<node>);
  add-node.left := n1;
  add-node.right := n2;
  while (try-explode-child(add-node, 0, add-node) | try-split-number(add-node))
    // keep trying
  end;
  add-node;
end;

define function magnitude(n :: <node>) => (magnitude :: <integer>)
  if (n.value)
    n.value;
  else
    (3 * magnitude(n.left)) + (2 * magnitude(n.right))
  end;
end;

define function main
    (name :: <string>, arguments :: <vector>)
  let numbers = read-numbers(arguments[0]);
  let current-number = numbers[0];
  for (number-index from 1 below size(numbers))
    current-number := add-number(current-number, numbers[number-index]);
  end;
  format-out("Magnitude %d\n",  magnitude(current-number));
  let max-magnitude = $minimum-integer;
  let strings = read-strings(arguments[0]);
  for (i in strings)
    for (j in strings)
      unless ( i = j )
        let n1 = make(<node>);
        make-number-tree(i, 1, n1);
        let n2 = make(<node>);
        make-number-tree(j, 1, n2);

        let m = magnitude(add-number(n1, n2));
        max-magnitude := max(m, max-magnitude);
      end;
    end
  end;
  format-out("Max Magnitude %d\n",  max-magnitude);
  exit-application(0);
end function main;

main(application-name(), application-arguments());
