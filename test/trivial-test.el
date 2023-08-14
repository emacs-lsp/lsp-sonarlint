(ert-deftest trivial-test-to-make-sure-testing-framework-works ()
  "Check whether the testing framework itself can accept a passing test."
  (should (equal (+ 1 1) 2)))
