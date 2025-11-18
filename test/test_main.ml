let () =
  Alcotest.run "Coeffect-Sandbox"
    [
      ("Lexer tests", Test_lexer.suite);
      ("Lambda Calculus Parser tests", Test_lambda_parser.suite);
      ("Lambda Calculus Interpreter tests", Test_lambda_interpreter.suite);
      ("Implicit Parameters Parser tests", Test_implicit_parser.suite);
    ]
