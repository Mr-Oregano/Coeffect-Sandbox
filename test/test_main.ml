let () =
  Alcotest.run "Coeffect-Sandbox"
    [
      ("Lexer tests", Test_lexer.suite);
      ("Lambda Calculus Parser tests", Test_lambda_parser.suite);
      ("Lambda Calculus Interpreter tests", Test_lambda_interpreter.suite);
      ("Implicit Parameters Parser tests", Test_implicit_parser.suite);
      ("Implicit Parameters Context tests", Test_implicit_context.suite);
      ("Implicit Parameters Type Checker tests", Test_implicit_typecheck.suite);
      ("Implicit Parameters Environment tests", Test_implicit_environment.suite);
      ("Implicit Parameters Interpreter tests", Test_implicit_interpreter.suite);
    ]
