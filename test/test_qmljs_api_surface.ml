let tests =
  Qmljs_test_runtime_context.tests
  @ Qmljs_test_values_objects.tests
  @ Qmljs_test_functions_classes.tests
  @ Qmljs_test_binary_numbers.tests
  @ Qmljs_test_modules_loaders.tests

let () = Eio_main.run @@ fun _env -> Qmljs_test_common.run tests
