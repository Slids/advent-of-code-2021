Module: day-4
Synopsis:
Author:
Copyright:

define function get-int-vector-from-string(str :: <string>, sep :: <string>) => (ns :: <sequence>)
  map(\string-to-integer, split(str, sep, remove-if-empty?: #t))
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
  let sum = 0;
  for ( row in board)
    for (entry in row)
      if (entry ~= #t)
        sum := sum + entry
      end;
    end;
  end;
  sum;
end;

define function find-win-and-score(drawn-numbers :: <sequence>, board :: <sequence>) => (res :: <vector>)
  let number-map = make(<table>);
  for (i from 0 below size(board))
    for (j from 0 below size(board))
      let entry = make(<vector>, size: 2);
      entry[0] := i;
      entry[1] := j;
      number-map[board[i][j]] := entry;
    end;
  end;
  let win-time-and-score = make(<vector>, size: 2);
  block(return-win)
    for (i from 0 below size(drawn-numbers))
      let number = drawn-numbers[i];
      let address = element(number-map, number, default: #f);
      when (address)
        board[address[0]][address[1]] := #t;
        when (check-win?(board))
          win-time-and-score[0] := i;
          win-time-and-score[1] := score-board(board) * number;
          return-win();
        end;
      end;
    end;
  end;
  win-time-and-score;
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
      let win-and-score = find-win-and-score(drawn-numbers, board);
      if (final-win[0] = -1 | comparator(final-win[0], win-and-score[0]))
        final-win := win-and-score
      end;
    end;
  end;
  final-win
end;

define function part-1() => ()
  format-out("Part 1 final score %d \n",
             get-solution(method (current :: <integer>, new :: <integer>) current > new; end)[1]);
end;

define function part-2() => ()
  format-out("Part 2 final score %d \n",
             get-solution(method (current :: <integer>, new :: <integer>) current < new; end)[1]);
end;

define function main() => ()
  part-1();
  part-2();
end;

main();
