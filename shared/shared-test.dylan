Module: shared-test

define test test-get-int-vector-from-string ()
  assert-equal(#[1, 2, 3], get-int-vector-from-string("123"));
end test;

run-test-application();
