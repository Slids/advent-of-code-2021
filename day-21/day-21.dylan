Module: day-21
Synopsis:
Author:
Copyright:

define function get-starting-pos(file :: <string>) => (players :: <sequence>)
  let pos-vec = make(<stretchy-vector>);
  with-open-file(file-stream = file)
    while (~stream-at-end?(file-stream))
      let line = read-line(file-stream);
      let player = make(<player>);
      player.number := string-to-integer(split(line, " ")[1]);
      player.pos := string-to-integer(split(line, " ")[4]);
      add!(pos-vec, player);
    end;
  end;
  pos-vec;
end;

let *simple-roll-global* = 0;
define function simple-roll() => (value :: <integer>)
  *simple-roll-global* := *simple-roll-global* + 1;
  if (*simple-roll-global* > 100)
    *simple-roll-global* := modulo(*simple-roll-global*, 100);
  end;
  *simple-roll-global*;
end;

define function three-rolls(roll :: <function>) => (value :: <integer>)
  roll() + roll() + roll();
end;

define class <player> (<object>)
  slot number :: <integer>, init-value: 0;
  slot pos :: <integer>, init-value: 0;
  slot score :: <integer>, init-value: 0;
end;

// player data [player-pos, player-]
define function player-turn(player-data :: <player>, roll :: <function>) => ()
  let roll-sum = three-rolls(roll);
  let next-pos = player-data.pos + roll-sum;
  if (modulo(next-pos, 10) = 0)
    next-pos := 10;
  else
    next-pos := modulo(next-pos,10);
  end;
  player-data.pos := next-pos;
  player-data.score := player-data.score + next-pos;
end;

define function play-game(players :: <sequence>) => (num-rolls :: <integer>)
  let num-rolls = 0;
  block (end-play)
    let i = 0;
    while (max(players[0].score, players[1].score) < 1000)
      player-turn(players[modulo(i, size(players))], simple-roll);
      num-rolls := num-rolls + 3;
      i := i + 1;
    end;
  end;
  num-rolls;
end;

define class <game-state> (<object>)
  slot player-pos :: <sequence>;
  slot scores :: <sequence>;
  slot number-of-universes :: <integer>;
end;

define function copy-game-state (gs :: <game-state>) => (copy-gs :: <game-state>)
  let gs-copy = make(<game-state>);
  gs-copy.player-pos := copy-sequence(gs.player-pos);
  gs-copy.scores := copy-sequence(gs.scores);
  gs-copy.number-of-universes := gs.number-of-universes;
  gs-copy;
end;

define function update-game(gs :: <game-state>, player-number :: <integer>, roll :: <integer>, num-universes :: <integer>)
  => (win? :: <boolean>)
  // update position.
  let next-pos = gs.player-pos[player-number] + roll;
  if (modulo(next-pos, 10) = 0)
    next-pos := 10;
  else
    next-pos := modulo(next-pos,10);
  end;
  gs.player-pos[player-number] := next-pos;
  //update score
  gs.scores[player-number] := gs.scores[player-number] + next-pos;
  // Update number-of-universes
  gs.number-of-universes := gs.number-of-universes * num-universes;

  gs.scores[player-number] >= 21;
end;

// define function group-games(games :: <sequence>)
//   let possible-scores := map(scores, games);
//   let possible-player-pos := map(player-pos, games);
//   remove-duplicates
//   for (score in scores)
//     for

// end;

define function dirac-dice (players :: <sequence>)
  let possible-rolls = #[3,4,5,6,7,8,9];
  let number-for-each = #[1,3,6,7,6,3,1];

  //
  let games = make(<stretchy-vector>);
  let number-of-universe-win = vector(0,0);
  let player-number = 0;
  let turn-number = 1;

  // first turn
  for (i from 0 below size(possible-rolls))
    let gs = make(<game-state>);
    gs.player-pos := vector(players[0].pos, players[1].pos);
    gs.scores := vector(0, 0);
    gs.number-of-universes := 1;
    update-game(gs, 0, possible-rolls[i], number-for-each[i]);
    player-number := 1;
    add!(games, gs);
  end;

  while (size(games) > 0)
    turn-number := turn-number + 1;
    format-out("size of games: %= turn-number %=\n", size(games));
    force-out();
    let new-game-set = make(<stretchy-vector>);
    for (i from 0 below size(possible-rolls))
      for (game in games)
        let game-copy = copy-game-state(game);
        let win? = update-game(game-copy, player-number, possible-rolls[i], number-for-each[i]);
        if (win?)
          number-of-universe-win[player-number] := number-of-universe-win[player-number]
            + game-copy.number-of-universes;
        else
          add!(new-game-set, game-copy);
        end;
      end;
    end;
    player-number := modulo(player-number + 1, 2);
    games := new-game-set;
  end;
  format-out("number of universe wins: %=\n", number-of-universe-win);
end;

define function main
    (name :: <string>, arguments :: <vector>)
  let players = get-starting-pos(arguments[0]);
  let num-rolls =  play-game(players);
  let min-score = reduce1(min, map(method (a) a.score end, players));
  format-out("Min score * losing score: %=!\n", min-score *  num-rolls);

  let players = get-starting-pos(arguments[0]);
  dirac-dice(players);
  exit-application(0);
end function main;

main(application-name(), application-arguments());
