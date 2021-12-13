Module: dylan-user

define library shared
  use common-dylan;
  use io;

  export shared;
end library shared;

define module shared
  use common-dylan;
  use format-out;

  export get-int-vector-from-string;
  export get-neighbors;
  export print-map;
end module shared;
