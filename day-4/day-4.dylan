Module: day-4
Synopsis:
Author:
Copyright:

define function get-int-vector-from-string(str :: <string>, sep :: <string>) => (ns :: <sequence>)
  map(string-to-integer, split(str, sep, remove-if-empty?: #t))
end;

define function get-board (stream :: <file-stream>) => (board :: <array>)
  let board = make(<stretchy-vector>);
  block(make-board)
    while (~stream-at-end?(stream))
      let line = read-line(stream);
      if (string-equal?(line, ""))
        make-board();
      end;
      add!(board, get-int-vector-from-string(line, " "));
    end;
  end;
  board;
end;

// useful for debugging
// define function print-board(board :: <sequence>) => ()
//   for (row in board)
//     format-out( "B: %=\n", row);
//   end;
// end;

define function check-win?(board :: <sequence>) => (win :: <boolean>)
  let all-true = method (el) el = #t end;
  // check row
  if (any?( method (seq) every?(all-true, seq) end, board))
    #t;
  else
    // check column
    let index-array = make(<array>, size: size(board[0]));
    for (i from 0 below size(board[0]))
      index-array[i] := i;
    end;
    any?(method (i) every?(method (row) row[i] = #t end, board) end, index-array);
  end;
end;

define function score-board(board :: <sequence>) => (score :: <integer>)
  local
    method value-or-0 (i) if (i = #t) 0 else i end; end,
    method reduce-row (row) reduce1(\+, map(value-or-0, row)) end;
  reduce1(\+, map(reduce-row, board));
end;

// Returns either the win-time and score (integers) or #f if no win.
define function find-win-and-score
    (drawn-numbers :: <sequence>, board :: <sequence>) => (win-time, score)
  let number-map = make(<table>);
  for (i from 0 below size(board))
    for (j from 0 below size(board))
      let entry = make(<vector>, size: 2);
      entry[0] := i;
      entry[1] := j;
      // Feels like this would be more straight-forward using a 3-dimensional array.
      number-map[board[i][j]] := entry;
    end;
  end;
  block(return-win)
    for (i from 0 below size(drawn-numbers))
      let number = drawn-numbers[i];
      let address = element(number-map, number, default: #f);
      when (address)
        board[address[0]][address[1]] := #t;
        when (check-win?(board))
          return-win(i, score-board(board) * number)
        end;
      end;
    end;
  end;
end;

define function get-solution(comparator :: <function>) => (final-win :: <sequence>)
  let final-win = #[-1, -1];
  with-open-file(file-stream = "list.txt")
    // get the vector of choices
    let line = read-line(file-stream);
    let drawn-numbers = get-int-vector-from-string(line, ",");
    read-line(file-stream);
    while (~stream-at-end?(file-stream))
      let board = get-board(file-stream);
      let (win-time, score) = find-win-and-score(drawn-numbers, board);
      if (final-win[0] = -1 | comparator(final-win[0], win-time))
        // I would change final-win to be two distinct values as well, and return them
        // from this function. Not going to do it now; it's late.
        final-win := vector(win-time, score)
      end;
    end;
  end;
  final-win
end;

define function part-1() => ()
  format-out("Part 1 final score %d \n",
             get-solution(\>)[1]);
end;

define function part-2() => ()
  format-out("Part 2 final score %d \n",
             get-solution(\<)[1]);
end;

define function main() => ()
  part-1();
  part-2();
end;

main();
