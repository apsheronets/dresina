let test_action ~int_arg ~string_arg () =
  respond & sprintf "Test_controller.test_action: int_arg=%i, string_arg=%S\n"
    int_arg string_arg
