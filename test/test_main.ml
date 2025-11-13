let () =
  Alcotest.run "Coeffect-Sandbox"
    [ ("Lexer tests", Test_lexer.suite); ("Parser tests", Test_parser.suite) ]
