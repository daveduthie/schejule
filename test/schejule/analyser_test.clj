(ns schejule.analyser-test
  (:require [clojure.test :refer :all]
            [schejule.analyser :refer :all]))

(deftest vec-subset?-test
  (is (= (vec-subset? [1 2 3] [1 2 3 4])
         true))
  (is (= true
         (vec-subset? [2 4] [1 2 3 4])))
  (is (= true
         (vec-subset? [1 2 3] [1 2 3])))
  (is (= false
         (vec-subset? [1 2 3] [1 3 2])))
  (is (= false
         (vec-subset? [1 3 2] [1 2 3]))))

