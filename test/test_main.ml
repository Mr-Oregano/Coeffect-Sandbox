let () =
  Alcotest.run "Coeffect-Sandbox"
    [
      ("Lexer tests", Test_lexer.suite);
      ("Parser tests", Test_lambda_parser.suite);
      ("Interpreter tests", Test_lambda_interpreter.suite);
    ]
