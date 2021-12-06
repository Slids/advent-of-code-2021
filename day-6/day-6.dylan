Module: day-6

define function get-int-vector-from-string(str :: <string>) => (ns :: <sequence>)
  map(\string-to-integer, split(str, ","))
end;

define function update-day(lantern-fish :: <sequence>) => (new-lantern-fish :: <vector>)
  let zero-fish :: <integer> = lantern-fish[0];
  for (i from 0 below 8)
    lantern-fish[i] := lantern-fish[i + 1];
  end;
  lantern-fish[8] := zero-fish;
  lantern-fish[6] := zero-fish + lantern-fish[6];
  lantern-fish
end;

let lantern-fish = make(<vector>, size: 9, fill: 0);
with-open-file(file-stream = "list.txt")
  while (~stream-at-end?(file-stream))
    let line = read-line(file-stream);
    let initial-lantern = get-int-vector-from-string(line);
    for ( i in initial-lantern)
      lantern-fish[i] := lantern-fish[i] + 1;
    end;
  end;
end;
for (i from 0 below 80)
  lantern-fish := update-day(lantern-fish);
end;
format-out("Part 1: num-fish: %d \n", reduce1(\+, lantern-fish));


lantern-fish := make(<vector>, size: 9, fill: 0);
with-open-file(file-stream = "list.txt")
  while (~stream-at-end?(file-stream))
    let line = read-line(file-stream);
    let initial-lantern = get-int-vector-from-string(line);
    for ( i in initial-lantern)
      lantern-fish[i] := lantern-fish[i] + 1;
    end;
  end;
end;
for (i from 0 below 256)
  lantern-fish := update-day(lantern-fish);
end;
format-out("Part 2: num-fish: %d \n", reduce1(\+, lantern-fish))
