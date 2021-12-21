Module: day-19
Synopsis:
Author:
Copyright:

define class <scanner> (<object>)
  slot number :: <integer>;
  slot beacons :: <sequence>;
  slot distances :: <sequence>;
  slot scanner-position-wrt0 :: <sequence>, init-value: #[];
end;

define function read-scanners  (file :: <string>) => (numbers :: <sequence>)
  let scanners = make(<stretchy-vector>);
  with-open-file(file-stream = file)
    let read-scanner-number = #t;
    let scanner = make(<scanner>);
    scanner.beacons := make(<stretchy-vector>);
    let i = 0;
    while (~stream-at-end?(file-stream))
      let str = read-line(file-stream);
      if (read-scanner-number)
        scanner.number := i;
        i := i + 1;
        read-scanner-number := #f;
      elseif (str = "")
        add!(scanners, scanner);
        scanner := make(<scanner>);
        scanner.beacons := make(<stretchy-vector>);
        read-scanner-number := #t;
      else
        let beacon-vector = split(str, ",");
        let beacon = map(string-to-integer, beacon-vector);
        add!(scanner.beacons, beacon);
      end;
    end;
    add!(scanners, scanner);
  end;
  scanners;
end;

define function squared-distance(v1 :: <sequence>, v2 :: <sequence>) => (int :: <integer>)
  reduce1(\+, map(method (a, b) (a - b)^2 end, v1, v2));
end;

define function manhattan-distance(v1 :: <sequence>, v2 :: <sequence>) => (int :: <integer>)
  reduce1(\+, map(method (a, b) abs(a - b) end, v1, v2));
end;

define function set-beacon-distances (scanner :: <scanner>) => ()
  scanner.distances := make(<stretchy-vector>);
  for (beacon in scanner.beacons)
    let d = map(curry(squared-distance, beacon), scanner.beacons);
    add!(scanner.distances, d);
  end;
end;

define function check-distances (s1 :: <scanner>, s2 :: <scanner>) =>
    (possible :: <boolean>, s1-match :: <integer>, s2-match :: <integer>)
  force-out();
  let match = #f;
  let s1-match = -1;
  let s2-match = -1;
  block (success)
    for (i from 0 below size(s1.distances))
      let d1 = s1.distances[i];
      for (j from 0 below size(s2.distances))
        let d2 = s2.distances[j];
        let d2-distances = copy-sequence(d2);

        for (d in d1)
          d2-distances := remove(d2-distances, d, count: 1);
        end;
        if (size(d2-distances) < size(d2) - 11)
          match := #t;
          s1-match := i;
          s2-match := j;
          success();
        end;
      end;
    end;
  end;
  values(match, s1-match, s2-match);
end;

define function translate(op :: <function>, v1 :: <sequence>, v2 :: <sequence>) => (res :: <sequence>)
  map(op, v1, v2);
end;

define function matrix-vector (m1 :: <sequence>, v :: <sequence>) => (v :: <sequence>)
  let vec = make(<stretchy-vector>);
  for (i from 0 below size(v))
    let value = 0;
    for (j from 0 below size(v))
      value := value + (m1[i][j] * v[j]);
    end;
    add!(vec, value);
  end;
  vec;
end;

define function translate-and-see-match (beacons1 :: <sequence>, beacons2 :: <sequence>,
                                         matching-v1-index :: <integer>, matching-v2-index :: <integer>)
 => (match :: <boolean>)
  // translate
  let v1-v2 = translate(\-, beacons1[matching-v1-index], beacons2[matching-v2-index]);
  let translated-beacons2 = map(curry(translate, \+, v1-v2), beacons2);

  // check match
  let beacons1-copy = copy-beacons(beacons1);

  for (i in translated-beacons2)
    beacons1-copy := remove(beacons1-copy, i,
                            test: method(a, b) a[0] == b[0] & a[1] == b[1] & a[2] == b[2] end);
  end;

  size(beacons1-copy) < (size(beacons1) - 11);
end;

define function copy-beacons(b :: <sequence>) => (_ :: <sequence>)
  map(copy-sequence, b);
end;

define function map-scanners (scanner-1 :: <scanner>, scanner-2 :: <scanner>, matching-v1-index :: <integer>, matching-v2-index :: <integer>)
 => (v1-v2 :: <sequence>)
  let b1 = scanner-1.beacons;
  let b2 = scanner-2.beacons;
  let temp-b2 = copy-beacons(b1);
  let v1-v2 = vector();

  block (success)
    for (x in #[#[1,0,0], #[-1,0,0], #[0,1,0], #[0,-1,0], #[0,0,1], #[0,0,-1]])
      for (y in #[#[1,0,0], #[-1,0,0], #[0,1,0], #[0,-1,0], #[0,0,1], #[0,0,-1]])
        for (z in #[#[1,0,0], #[-1,0,0], #[0,1,0], #[0,-1,0], #[0,0,1], #[0,0,-1]])
          let m = vector(x, y, z);
          let temp-b2 = copy-beacons(b2);
          temp-b2 := map(curry(matrix-vector, m), temp-b2);

          if (translate-and-see-match(b1, temp-b2, matching-v1-index, matching-v2-index))
            // update scanner2 to have the same oreintation as scanner1.
            scanner-2.beacons := temp-b2;
            v1-v2 := translate(\-, scanner-1.beacons[matching-v1-index], scanner-2.beacons[matching-v2-index]);
            success();
          end;
        end;
      end;
    end;
  end;
  v1-v2;
end;

define function map-the-scanners (scanner1 :: <scanner>, scanner2 :: <scanner>, v1 :: <integer>, v2 :: <integer>,
                                  needed-to-scan :: <stretchy-vector>, currently-mapped :: <stretchy-vector>) => ()
  let v1-v2 = map-scanners(scanner1, scanner2, v1, v2);
  if (size(v1-v2) > 0)
    scanner2.scanner-position-wrt0 :=
      translate(\+, scanner1.scanner-position-wrt0, v1-v2);
    add!(needed-to-scan, scanner2.number);
    add!(currently-mapped, scanner2.number);
  end;
end;

define function continue-to-map(current-scanner-number :: <integer>, currently-mapped :: <stretchy-vector>,
                                map-table :: <table>, scanners :: <sequence>)
 => ()
  let scanner1 = scanners[current-scanner-number];
  let needed-to-scan = make(<stretchy-vector>);
  for (related-scanner-v1-v2 in element(map-table, current-scanner-number, default: #[]))
    unless (member?(related-scanner-v1-v2[0], currently-mapped))
      let scanner2 = scanners[related-scanner-v1-v2[0]];
      map-the-scanners(scanner1, scanner2, related-scanner-v1-v2[1], related-scanner-v1-v2[2],
                       needed-to-scan, currently-mapped);
    end;
  end;
  for (i in needed-to-scan)
    continue-to-map(i, currently-mapped, map-table, scanners);
  end;
end;

define function find-beacon-list(scanners :: <sequence>, map-table :: <table>) => (scanner-list :: <sequence>)
  let first-scanner = scanners[0];
  first-scanner.scanner-position-wrt0 := #[0, 0, 0];
  let currently-mapped = make(<stretchy-vector>);
  continue-to-map(0, currently-mapped, map-table, scanners);

  currently-mapped := remove-duplicates(currently-mapped);
  while (size(currently-mapped) < size(scanners) - 1)
    for (i from 1 below size(scanners))
      if (~member?(i, currently-mapped) & element(map-table, i, default: #f))
        let nts = make(<stretchy-vector>);
        for (j in map-table[i])
          let scanner1 = scanners[j[0]];
          if (size(scanner1.scanner-position-wrt0) > 0)
            map-the-scanners(scanner1, scanners[i], j[2], j[1], nts, currently-mapped);
          end;
        end;
        for (j in nts)
          continue-to-map(j, currently-mapped, map-table, scanners);
        end;
      end;
    end;
    currently-mapped := remove-duplicates(currently-mapped);
  end;

  let all-beacons = make(<stretchy-vector>);
  for (scanner in scanners)
    let beacons = copy-beacons(scanner.beacons);
    let translation-vec = scanner.scanner-position-wrt0;
    beacons := map(curry(translate, \+, translation-vec), beacons);
    all-beacons := concatenate(all-beacons, beacons);
  end;
  all-beacons := remove-duplicates(all-beacons, test: \=);

  all-beacons;
end;

define function main
    (name :: <string>, arguments :: <vector>)
  force-out();
  let scanners = read-scanners(arguments[0]);
  for (scanner in scanners)
    set-beacon-distances(scanner);
  end;
  let match-table = make(<table>);
  // format-out("ss: %=", size(scanners));
  for (i from 0 below size(scanners))
    let first-match = #t;
    for (j from i + 1 below size(scanners))
      let (match, s1-match, s2-match) = check-distances(scanners[i], scanners[j]);
      if (match)
        if (first-match)
          first-match := #f;
          match-table[i] := make(<stretchy-vector>);
        end;
        add!(match-table[i], vector(j, s1-match, s2-match));
      end;
    end;
  end;

  let beacon-list = find-beacon-list(scanners, match-table);
  format-out("Number of beacons %=\n", size(beacon-list));

  let max-scanner-distance = 0;
  for (i in scanners)
    for (j in scanners)
      let distance = manhattan-distance(i.scanner-position-wrt0, j.scanner-position-wrt0);
      max-scanner-distance := max(max-scanner-distance, distance);
    end;
  end;
  format-out("Max Distance %=\n", max-scanner-distance);
  exit-application(0);
end function main;

main(application-name(), application-arguments());
