(defmodule shrimp-pool-test
  (behaviour ltest-unit)
  (export all)
  (import
    (from ltest
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))
(include-lib "ltest/include/ltest-macros.lfe")

(deftest init
  (is 'true))
