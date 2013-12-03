# 0 "_routes_generated_"
module Say_controller = struct
  module Hello = struct
    let path = "/say/hello"
    ;;
    end
  ;;
  module Goodbye = struct
    let path = "/say/goodbye"
    ;;
    end
  ;;
  end
;;
module Test_controller = struct
  module Test_action = struct
    let path ~int_arg ~string_arg = "/myint/" ^ string_of_int int_arg ^ "/mystring/" ^ string_arg
    ;;
    end
  ;;
  end
;;
