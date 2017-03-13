(ns schejule
  (:require [schejule.generator :as gen]
            [schejule.analyser :as z]
            [clojure.core.async :refer [chan go go-loop thread >! >!! <! <!!] :as a]))

;; ## Helper functions

(defn sort-tasks
  "Sort by `:job`. Within a job, sort by `:id`"
  [tasks]
  (sort (fn [a b] (if-not (= (a :job) (b :job))
                    (compare (a :job) (b :job))
                    (compare (a :id) (b :id))))
        tasks))

(defn clean-tasks
  "Makes sure tasks are sorted by `:job`, then by `:id` and have `:id` values from 0 to n-1."
  [tasks]
  (let [sorted (sort-tasks tasks)]
    (loop [clean         []
           [head & tail] sorted
           id            0]
      (if (empty? head)
        clean
        (recur (conj clean (assoc head :id id)) tail (inc id))))))

(defn by-job [tasks]
  (into {} (map (fn [task] [(:id task) (:job task)])
                tasks)))

;; ## Entrypoint

(defn adequate-schedule
  "Searches for a solution and returns the best found in `timeout` milliseconds.
  \"Best\" means the minimum workspan, given the `:endby` constraints encoded into the tasks."
  [tasks millis]
  (let [tasks      (clean-tasks tasks)
        job-lookup (by-job tasks)
        failures   (agent nil)
        sols       (agent nil)
        f
        (future (loop [rules        []
                       [s & others] (z/simulate-schedule* tasks [])]

                  (if (not (Thread/interrupted))
                    (if (s :failure)
                      (let [new-rules (z/accrete-rules job-lookup rules (z/distil-rule s))]
                        (prn (str "Failure: " (s :failure)))
                        (prn (str "New Rules: " new-rules))
                        (send failures (fn [old] (update old (s :failure) #(inc (or % 0)))))
                        (recur new-rules (z/simulate-schedule* tasks new-rules)))
                      (let [current (z/get-workspan s)]
                        (prn (str "Success: " current))
                        (send sols (fn [b] (if (or (nil? b)
                                                  (< current (:workspan b)))
                                            (assoc s :workspan current)
                                            b)))
                        (recur rules others)))
                    nil)))]

    (Thread/sleep millis)
    (future-cancel f)
    (assoc @sols :failures @failures)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use 'schejule-test)
;; (def failures (atom nil))
;; (add-watch failures :yo (fn [_ _ _ state] (prn state)))
;; (adequate-schedule schejule-test/sample-1 500)
;; (adequate-schedule schejule-test/sample-3 (* 60000 5))

;; (def a-big-failure
;;   {:machines {5 215, 11 311, 4 781, 7 946, 9 1099},
;;    :jobs {"brq890" 946, "jtj163" 927, "pke479" 1099},
;;    :tasks '({:id 7, :job "pke479", :machine 9, :duration 172, :endby 600, :ord 7, :end-time 1099}
;;             {:id 6, :job "jtj163", :machine 9, :duration 146, :ord 6, :end-time 927}
;;             {:id 5, :job "jtj163", :machine 4, :duration 96, :ord 5, :end-time 781}
;;             {:id 4, :job "brq890", :machine 7, :duration 261, :ord 4, :end-time 946}
;;             {:id 3, :job "brq890", :machine 4, :duration 84, :ord 3, :end-time 685}
;;             {:id 2, :job "brq890", :machine 4, :duration 290, :ord 2, :end-time 601}
;;             {:id 1, :job "brq890", :machine 11, :duration 96, :ord 1, :end-time 311}
;;             {:id 0, :job "brq890", :machine 5, :duration 215, :ord 0, :end-time 215}),
;;    :failure 7})

;; (distil-rule a-big-failure)
;; ((3 5) (6 7))

;; (def a-big-failure-2
;;   {:machines {5 215, 11 311, 4 781, 7 1042, 9 1015},
;;    :jobs {"brq890" 1042, "jtj163" 843, "pke479" 1015},
;;    :tasks '({:id 7, :job "pke479", :machine 9, :duration 172, :endby 600, :ord 7, :end-time 1015}
;;             {:id 6, :job "jtj163", :machine 9, :duration 146, :ord 6, :end-time 843}
;;             {:id 4, :job "brq890", :machine 7, :duration 261, :ord 5, :end-time 1042}
;;             {:id 3, :job "brq890", :machine 4, :duration 84, :ord 4, :end-time 781}
;;             {:id 5, :job "jtj163", :machine 4, :duration 96, :ord 3, :end-time 697}
;;             {:id 2, :job "brq890", :machine 4, :duration 290, :ord 2, :end-time 601}
;;             {:id 1, :job "brq890", :machine 11, :duration 96, :ord 1, :end-time 311}
;;             {:id 0, :job "brq890", :machine 5, :duration 215, :ord 0, :end-time 215}),
;;    :failure 7})

;; (distil-rule a-big-failure-2)
;; ((2 5) (6 7))

;; ;=>(0 1 2 5 6 7)

;; (accrete-rules (schejule/by-job (:tasks a-big-failure-2))
;;                '(((3 5) (6 7)))
;;                '((2 5) (6 7)))
;; ;=>((0 1 2 5 6 7))

;; (some #(and (% :failure) %) (simulate-schedule* sample-1-cleaned ['(0 1 2 5 6 7)]))

;; (def a-big-failure-3
;;   {:machines {5 1918, 11 311, 4 1983, 7 1212, 9 812, 0 114, 3 1429, 6 880},
;;    :jobs {"brq890" 946, "jtj163" 242, "pke479" 414, "uym843" 880, "wim276" 1983},
;;    :tasks '({:id 17, :job "wim276", :machine 4, :duration 65, :endby 1037, :ord 17, :end-time 1983}
;;             {:id 16, :job "wim276", :machine 5, :duration 234, :ord 16, :end-time 1918}
;;             {:id 15, :job "wim276", :machine 5, :duration 255, :ord 15, :end-time 1684}
;;             {:id 14, :job "wim276", :machine 3, :duration 217, :ord 14, :end-time 1429}
;;             {:id 13, :job "wim276", :machine 7, :duration 266, :ord 13, :end-time 1212}
;;             {:id 12, :job "uym843", :machine 6, :duration 68, :endby 6800, :ord 12, :end-time 880}
;;             {:id 11, :job "uym843", :machine 9, :duration 61, :ord 11, :end-time 812}
;;             {:id 10, :job "uym843", :machine 3, :duration 293, :ord 10, :end-time 751}
;;             {:id 9, :job "uym843", :machine 5, :duration 243, :ord 9, :end-time 458}
;;             {:id 8, :job "uym843", :machine 0, :duration 114, :ord 8, :end-time 114}
;;             {:id 7, :job "pke479", :machine 9, :duration 172, :endby 600, :ord 7, :end-time 414}
;;             {:id 6, :job "jtj163", :machine 9, :duration 146, :ord 6, :end-time 242}
;;             {:id 4, :job "brq890", :machine 7, :duration 261, :ord 5, :end-time 946}
;;             {:id 3, :job "brq890", :machine 4, :duration 84, :ord 4, :end-time 685}
;;             {:id 2, :job "brq890", :machine 4, :duration 290, :ord 3, :end-time 601}
;;             {:id 5, :job "jtj163", :machine 4, :duration 96, :ord 2, :end-time 96}
;;             {:id 1, :job "brq890", :machine 11, :duration 96, :ord 1, :end-time 311}
;;             {:id 0, :job "brq890", :machine 5, :duration 215, :ord 0, :end-time 215}),
;;    :failure 17})

;; (z/distil-rule a-big-failure-3)
;; ((4 13))

;; ;=>(0 1 2 3 4 13 14 15 16 17)

;; (z/accrete-rules (by-job (:tasks a-big-failure-3)) '(((2 5) (6 7))) '((4 13)))
;; (((4 13)) ((2 5) (6 7)))

;; ;=>((0 1 2 3 4 13 14 15 16 17) (0 1 2 5 6 7))

;; (some #(and (% :failure) %) (simulate-schedule* sample-1-cleaned '((0 1 2 3 4 13 14 15 16 17) (0 1 2 5 6 7))))

;; (def a-big-failure-4
;;   {:machines {5 1457, 11 311, 4 1522, 7 946, 9 812, 0 114, 3 968, 6 880},
;;    :jobs {"brq890" 946, "jtj163" 242, "wim276" 1522, "pke479" 414, "uym843" 880},
;;    :tasks '({:id 17, :job "wim276", :machine 4, :duration 65, :endby 1037, :ord 17, :end-time 1522}
;;             {:id 16, :job "wim276", :machine 5, :duration 234, :ord 16, :end-time 1457}
;;             {:id 15, :job "wim276", :machine 5, :duration 255, :ord 15, :end-time 1223}
;;             {:id 14, :job "wim276", :machine 3, :duration 217, :ord 14, :end-time 968}
;;             {:id 12, :job "uym843", :machine 6, :duration 68, :endby 6800, :ord 13, :end-time 880}
;;             {:id 11, :job "uym843", :machine 9, :duration 61, :ord 12, :end-time 812}
;;             {:id 10, :job "uym843", :machine 3, :duration 293, :ord 11, :end-time 751}
;;             {:id 9, :job "uym843", :machine 5, :duration 243, :ord 10, :end-time 458}
;;             {:id 8, :job "uym843", :machine 0, :duration 114, :ord 9, :end-time 114}
;;             {:id 7, :job "pke479", :machine 9, :duration 172, :endby 600, :ord 8, :end-time 414}
;;             {:id 6, :job "jtj163", :machine 9, :duration 146, :ord 7, :end-time 242}
;;             {:id 4, :job "brq890", :machine 7, :duration 261, :ord 6, :end-time 946}
;;             {:id 13, :job "wim276", :machine 7, :duration 266, :ord 5, :end-time 266}
;;             {:id 3, :job "brq890", :machine 4, :duration 84, :ord 4, :end-time 685}
;;             {:id 2, :job "brq890", :machine 4, :duration 290, :ord 3, :end-time 601}
;;             {:id 5, :job "jtj163", :machine 4, :duration 96, :ord 2, :end-time 96}
;;             {:id 1, :job "brq890", :machine 11, :duration 96, :ord 1, :end-time 311}
;;             {:id 0, :job "brq890", :machine 5, :duration 215, :ord 0, :end-time 215}),
;;    :failure 17})

;; (z/distil-rule a-big-failure-4)
;; ((0 9) (10 14))

;; (z/accrete-rules (by-job (:tasks a-big-failure-4))
;;                '(((4 13))
;;                  ((2 5) (6 7)))
;;                '((0 9) (10 14)))

;; (((0 9) (10 14)) ((4 13)) ((2 5) (6 7)))

;; (def a-big-failure-5
;;   {:machines {4 1421, 9 711, 0 114, 5 1356, 7 1303, 11 668, 3 867, 6 779},
;;    :jobs {"jtj163" 242, "pke479" 414, "uym843" 779, "wim276" 1421, "brq890" 1303},
;;    :tasks '({:id 17, :job "wim276", :machine 4, :duration 65, :endby 1037, :ord 17, :end-time 1421}
;;             {:id 16, :job "wim276", :machine 5, :duration 234, :ord 16, :end-time 1356}
;;             {:id 15, :job "wim276", :machine 5, :duration 255, :ord 15, :end-time 1122}
;;             {:id 14, :job "wim276", :machine 3, :duration 217, :ord 14, :end-time 867}
;;             {:id 12, :job "uym843", :machine 6, :duration 68, :endby 6800, :ord 13, :end-time 779}
;;             {:id 11, :job "uym843", :machine 9, :duration 61, :ord 12, :end-time 711}
;;             {:id 10, :job "uym843", :machine 3, :duration 293, :ord 11, :end-time 650}
;;             {:id 4, :job "brq890", :machine 7, :duration 261, :ord 10, :end-time 1303}
;;             {:id 3, :job "brq890", :machine 4, :duration 84, :ord 9, :end-time 1042}
;;             {:id 2, :job "brq890", :machine 4, :duration 290, :ord 8, :end-time 958}
;;             {:id 1, :job "brq890", :machine 11, :duration 96, :ord 7, :end-time 668}
;;             {:id 0, :job "brq890", :machine 5, :duration 215, :ord 6, :end-time 572}
;;             {:id 13, :job "wim276", :machine 7, :duration 266, :ord 5, :end-time 266}
;;             {:id 9, :job "uym843", :machine 5, :duration 243, :ord 4, :end-time 357}
;;             {:id 8, :job "uym843", :machine 0, :duration 114, :ord 3, :end-time 114}
;;             {:id 7, :job "pke479", :machine 9, :duration 172, :endby 600, :ord 2, :end-time 414}
;;             {:id 6, :job "jtj163", :machine 9, :duration 146, :ord 1, :end-time 242}
;;             {:id 5, :job "jtj163", :machine 4, :duration 96, :ord 0, :end-time 96}),
;;    :failure 17})

;; (z/distil-rule a-big-failure-5)
;; ((10 14))

;; (z/accrete-rules (by-job (:tasks a-big-failure-5))
;;                '(((0 9) (10 14)) ((4 13)) ((2 5) (6 7)))
;;                '((10 14)))
