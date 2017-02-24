(ns schejule.core-test
  (:require [clojure.test :refer :all]
            [schejule.core :refer :all]))

(def sample-tasks-1
  [{:id 1 :machine 1 :job "yum" :duration 100}
   {:id 2 :machine 2 :job "yum" :duration 80}
   {:id 4 :machine 1 :job "yuck" :duration 130 :endby 250}])

(def sample-tasks-2
  [{:id 1 :job :client001 :duration 180 :machine 1}
   {:id 2 :job :client001 :duration 120 :machine 2}
   {:id 7 :job :stuff001 :duration 100 :machine 3}
   {:id 8 :job :stuff001 :duration 150 :machine 4}
   {:id 9 :job :job001 :duration 180 :machine 1}
   {:id 13 :job :thing001 :duration 240 :machine 3}
   {:id 17 :job :thing001 :duration 223 :machine 3}])

(def sample-tasks-3
  [{:id 1 :job :client001 :duration 180 :machine 1}
   {:id 2 :job :client001 :duration 120 :machine 2}
   {:id 3 :job :client001 :duration 100 :machine 3}
   {:id 4 :job :client001 :duration 150 :machine 4 :endby 431}
   {:id 5 :job :stuff001 :duration 180 :machine 1}
   {:id 6 :job :stuff001 :duration 120 :machine 2}
   {:id 7 :job :stuff001 :duration 100 :machine 3}
   {:id 8 :job :stuff001 :duration 150 :machine 4}
   {:id 9 :job :job001 :duration 180 :machine 1}
   {:id 10 :job :job001 :duration 120 :machine 2}
   {:id 11 :job :job001 :duration 100 :machine 3}
   {:id 12 :job :job001 :duration 120 :machine 3 :endby 640}
   {:id 13 :job :thing001 :duration 240 :machine 3}
   {:id 14 :job :thing001 :duration 140 :machine 2}
   {:id 15 :job :thing001 :duration 200 :machine 4}
   {:id 16 :job :thing001 :duration 136 :machine 1}
   {:id 17 :job :thing001 :duration 223 :machine 3 :endby 0}])

(deftest solvo*-test
  (is (= (first (solvo* (sort-tasks sample-tasks-3)))
       '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))))

(deftest constructo*-test
  (is (= (first (constructo* sample-tasks-3))
         '({:id 1, :job :client001, :duration 180, :machine 1, :ord 0}
           {:id 2, :job :client001, :duration 120, :machine 2, :ord 1}
           {:id 3, :job :client001, :duration 100, :machine 3, :ord 2}
           {:id 4, :job :client001, :duration 150, :machine 4, :endby 431, :ord 3}
           {:id 5, :job :stuff001, :duration 180, :machine 1, :ord 4}
           {:id 6, :job :stuff001, :duration 120, :machine 2, :ord 5}
           {:id 7, :job :stuff001, :duration 100, :machine 3, :ord 6}
           {:id 8, :job :stuff001, :duration 150, :machine 4, :ord 7}
           {:id 9, :job :job001, :duration 180, :machine 1, :ord 8}
           {:id 10, :job :job001, :duration 120, :machine 2, :ord 9}
           {:id 11, :job :job001, :duration 100, :machine 3, :ord 10}
           {:id 12, :job :job001, :duration 120, :machine 3, :endby 640, :ord 11}
           {:id 13, :job :thing001, :duration 240, :machine 3, :ord 12}
           {:id 14, :job :thing001, :duration 140, :machine 2, :ord 13}
           {:id 15, :job :thing001, :duration 200, :machine 4, :ord 14}
           {:id 16, :job :thing001, :duration 136, :machine 1, :ord 15}
           {:id 17, :job :thing001, :duration 223, :machine 3, :endby 0, :ord 16}))))

(deftest due-filter*-test
  (is (= (first (due-filter* sample-tasks-2))
         {:machines {1 360, 2 300, 3 563, 4 250},
          :jobs {:client001 300, :stuff001 250, :job001 360, :thing001 563},
          :tasks '({:id 17, :job :thing001, :duration 223, :machine 3, :ord 6, :end-time 563}
                  {:id 13, :job :thing001, :duration 240, :machine 3, :ord 5, :end-time 340}
                  {:id 9, :job :job001, :duration 180, :machine 1, :ord 4, :end-time 360}
                  {:id 8, :job :stuff001, :duration 150, :machine 4, :ord 3, :end-time 250}
                  {:id 7, :job :stuff001, :duration 100, :machine 3, :ord 2, :end-time 100}
                  {:id 2, :job :client001, :duration 120, :machine 2, :ord 1, :end-time 300}
                  {:id 1, :job :client001, :duration 180, :machine 1, :ord 0, :end-time 180})}))
  (is (= (count (due-filter* sample-tasks-2))
         630)))
;; Don't call (due-filter* sample-tasks-3)! It will block.

(deftest adequate-schedule-test
  (is (= (adequate-schedule 100 sample-tasks-3)
         nil))
  (is (= (adequate-schedule 100 sample-tasks-2)
         {:machines {1 360, 2 300, 3 563, 4 250},
          :jobs {:client001 300, :stuff001 250, :job001 360, :thing001 563},
          :tasks '({:id 17, :job :thing001, :duration 223, :machine 3, :ord 6, :end-time 563}
                   {:id 13, :job :thing001, :duration 240, :machine 3, :ord 5, :end-time 340}
                   {:id 9, :job :job001, :duration 180, :machine 1, :ord 4, :end-time 360}
                   {:id 8, :job :stuff001, :duration 150, :machine 4, :ord 3, :end-time 250}
                   {:id 7, :job :stuff001, :duration 100, :machine 3, :ord 2, :end-time 100}
                   {:id 2, :job :client001, :duration 120, :machine 2, :ord 1, :end-time 300}
                   {:id 1, :job :client001, :duration 180, :machine 1, :ord 0, :end-time 180})})))
