Module: day-16
Synopsis:
Author:
Copyright:

define function char-to-bin-string(char :: <character>) => (str :: <string>)
  select (char)
    '0' => "0000";
    '1' => "0001";
    '2' => "0010";
    '3' => "0011";
    '4' => "0100";
    '5' => "0101";
    '6' => "0110";
    '7' => "0111";
    '8' => "1000";
    '9' => "1001";
    'A' => "1010";
    'B' => "1011";
    'C' => "1100";
    'D' => "1101";
    'E' => "1110";
    'F' => "1111";
  end;
end;

define function get-input-string (file :: <string>) => (str :: <string>)
  let str = "";
  with-open-file(file-stream = file)
    while (~stream-at-end?(file-stream))
      str := read-line(file-stream);
    end;
  end;
  str;
end;

define class <packet> (<object>)
  slot version :: <integer>;
  slot type-id :: <integer>;
  slot value :: <integer>;
  slot sub-packets :: <sequence>,
    init-value: #[];
end;

define function parse-packet (str ::<string>, s :: <integer>) => (p :: <packet>, ns :: <integer>)
  let p = make(<packet>);
  p.version := string-to-integer(str, start: s, end: s + 3, base: 2);
  s := s + 3;
  p.type-id := string-to-integer(str, start: s, end: s + 3, base: 2);
  s := s + 3;
  if (p.type-id == 4)
    let (lit, ns) = parse-literal(str, s);
    p.value := lit;
    s := ns;
  else
    let (sub-packets, ns) = parse-operator(str, s);
    p.sub-packets := sub-packets;
    s := ns;
  end;
  values(p, s);
end;

define function parse-literal(str :: <string>, s :: <integer>) => (lit :: <integer>, ns :: <integer>)
  let continue = #t;
  let lit = make(<stretchy-vector>);
  // start off with 6 for the first 6 packet bits
  let agg-count = 6;
  while (continue)
    continue := (str[s] = '1');
    s := s + 1;
    for (i from s below s + 4)
      add!(lit, str[i]);
      s := s + 1;
    end;
    agg-count := agg-count + 5;
  end;
  let lit-value = string-to-integer(map-as(<string>, identity, lit), base: 2);
  // trailing zeros
  values(lit-value, s);
end;

define function parse-operator(str :: <string>, s :: <integer>) => (lit :: <sequence>, ns :: <integer>)
  force-out();
  s := s + 1;
  let subpacket-seq = make(<stretchy-vector>);
  if (str[s - 1] = '0')
    let total-length-subpackets = string-to-integer(str, start: s, end: s + 15, base: 2);
    s := s + 15;
    let cur-s = s;
    while (cur-s + total-length-subpackets > s)
      let (subpacket, ns) = parse-packet(str, s);
      s := ns;
      add!(subpacket-seq, subpacket);
    end;
  else
    let number-of-subpackets = string-to-integer(str, start: s, end: s + 11, base: 2);
    s := s + 11;
    for (i from 0 below number-of-subpackets)
      let (subpacket, ns) = parse-packet(str, s);
      s := ns;
      add!(subpacket-seq, subpacket);
    end;
  end;
  values(subpacket-seq, s);
end;

define function sum-version(p :: <packet>) => (i :: <integer>)
  let version-total = p.version;
  force-out();
  for (i in p.sub-packets)
    version-total := version-total + sum-version(i);
  end;
  version-total;
end;

define function evaluate-packet(p :: <packet>) => (int :: <integer>)
  select (p.type-id)
    0 =>
      let sum = 0;
      for (i in p.sub-packets)
        sum := sum + evaluate-packet(i);
      end;
      sum;
    1 =>
      let prod = 1;
      for (i in p.sub-packets)
        prod := prod * evaluate-packet(i);
      end;
      prod;
    2 =>
      let m = $maximum-integer;
      for (i in p.sub-packets)
        m := min(m, evaluate-packet(i));
      end;
      m;
    3 =>
      let m = $minimum-integer;
      for (i in p.sub-packets)
        m := max(m, evaluate-packet(i));
      end;
      m;
    4 => p.value;
    5 =>
      if (evaluate-packet(p.sub-packets[0]) > evaluate-packet(p.sub-packets[1]))
        1
      else
        0
      end;
    6 =>
      if (evaluate-packet(p.sub-packets[0]) < evaluate-packet(p.sub-packets[1]))
        1
      else
        0
      end;
    7 =>
      if (evaluate-packet(p.sub-packets[0]) = evaluate-packet(p.sub-packets[1]))
        1
      else
        0
      end;
  end;
end;

define function main
    (name :: <string>, arguments :: <vector>)
  force-out();
  let input-str = get-input-string(arguments[0]);
  let bin-string = reduce1(concatenate, map-as(<vector>, char-to-bin-string, input-str));

  let p = parse-packet(bin-string, 0);
  format-out("version-total: %d\n", sum-version(p));
  format-out("Packet eval %d\n", evaluate-packet(p));
  exit-application(0);
end function main;

main(application-name(), application-arguments());
