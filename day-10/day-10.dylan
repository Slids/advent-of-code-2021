Module: day-10
Synopsis:
Author:
Copyright:

define function element-error-value(c :: <character>) => (int :: <integer>)
  select (c)
    ')' => 3;
    ']' => 57;
    '}' => 1197;
    '>' => 25137;
  end;
end;

define function matching-close(c :: <character>) => (c :: <character>)
  select (c)
    '(' => ')';
    '[' => ']';
    '{' => '}';
    '<' => '>';
  end;
end;

define function syntax-error-value(str :: <string>) => (int :: <integer>)
  let d = make(<deque>);
  let error-value = 0;
  block (break)
    for (i in str)
      if ( i = '(' | i = '[' | i = '<' | i = '{' )
        push(d,i);
      else
        let last-element = pop(d);
        let last-element-close = matching-close(last-element);
        unless (last-element-close = i)
          error-value := element-error-value(i);
          break();
        end;
      end;
    end;
  end;
  error-value;
end;

define function unclosed-portion(str :: <string>) => (int :: <sequence>)
  let d = make(<deque>);
  let error-value = 0;
  block (break)
    for (i in str)
      if ( i = '(' | i = '[' | i = '<' | i = '{' )
        push(d,i);
      else
        let last-element = pop(d);
      end;
    end;
  end;
  d;
end;

let input = make(<stretchy-vector>);
with-open-file(file-stream = "list.txt")
  while (~stream-at-end?(file-stream))
    add!(input, read-line(file-stream));
  end;
end;

let syntax-error-value-vec = map(syntax-error-value, input);
format-out("Syntax-error-total %d\n", reduce1(\+, syntax-error-value-vec));

define function line-completion (input-string :: <string>) => (completion-string :: <string>)
  let up = unclosed-portion(input-string);
  map-as(<string>, matching-close, up);
end;

define function unclosed-value-map (c :: <character>) => (int :: <integer>)
  select (c)
    ')' => 1;
    ']' => 2;
    '}' => 3;
    '>' => 4;
  end;
end;

define function unclosed-string-value (str :: <string>) => (int :: <integer>)
  let score = 0;
  for ( i in str)
    score := score * 5;
    score := score + unclosed-value-map(i);
  end;
  score;
end;

let line-completions = make(<stretchy-vector>);
for (input-str in input)
  if (syntax-error-value(input-str) = 0)
    add!(line-completions, line-completion(input-str))
  end;
end;
let unclosed-values = map(unclosed-string-value, line-completions);
unclosed-values := sort(unclosed-values);
format-out("middle unclosed value: %=\n", unclosed-values[floor/(size(unclosed-values), 2)])
