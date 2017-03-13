(ns schejule.analyser
  (:require [schejule.generator :as gen]))

;; (defn vec-subset?
;;   "Returns true if `sub` contains a subset of `a-vector`'s elements in the same order as `a-vector`.
;;   Returns false otherwise."
;;   [sub a-vector]
;;   (loop [v1 sub
;;          v2 a-vector]
;;     (cond
;;       (nil? v1)                 true
;;       (nil? v2)                 false
;;       (= (first v1) (first v2)) (recur (next v1) (next v2))
;;       :else                     (recur v1 (next v2)))))

(defn get-workspan
  "Get the workspan of a solution.
  The workspan is the finish time of the last task to be completed."
  [solution]
  (if (empty? solution)
    nil
    (->> solution :machines (map second) (apply max))))

(defn simulate-schedule*
  "Simulates schedule execution, checking whether `:endby`-constrained jobs will be done on time."
  [tasks constraints]
  (map (fn [tasks]
         (reduce (fn [solution task]
                   (let [{:keys [machine duration job id endby]} task

                         end-time (+ duration (max
                                               (or (get-in solution [:machines machine]) 0)
                                               (or (get-in solution [:jobs job]) 0)))]

                      ;; Check if `:endby` constraint has been violated.
                     (let [new-sol (-> solution
                                       (assoc-in [:machines machine] end-time)
                                       (assoc-in [:jobs job] end-time)
                                       (update :tasks #(conj % (assoc task :end-time end-time))))]
                       (if (and endby (< endby end-time))
                         (reduced (assoc new-sol :failure id))
                         new-sol))))
                 {}
                 tasks))

       (gen/gen-schedule* tasks constraints)))

;; TODO: write distil-rule. It should take a map describing a failed schedule, as emitted by `simulate-schedule*`, and return the smallest possible set of tasks which cannot precede the failed task (or one of its predecessors).

;; 2017-03-13 01:23 NEW THINKING
;; Bind only tasks at job-boundaries for less binding overall
;; rather than [a . b c d . e f] -> [a b] [d e] -> [a b d e]
(defn toxic-seq
  "'Rewinds' a bad schedule to identify which subsequence of tasks contributed to the failure"
  [bad-schedule]
  (loop [current       (first (bad-schedule :tasks))
         [head & tail] (next (bad-schedule :tasks))
         bottleneck    [current]
         start         (- (current :end-time) (current :duration))]
    (cond
      (nil? head)
      bottleneck

      (and (= start (head :end-time))
           (or (= (current :job) (head :job))
               (= (current :machine) (head :machine))))
      (recur head tail (cons head bottleneck) (- start (head :duration)))

      :else
      (recur current tail bottleneck start))))

(defn distil-rule
  "Wraps `toxic-seq`, trimming result to focus on first and last tasks in each job"
  [bad-schedule]
  (partition 2 (rest (mapcat (comp (fn [tasks] [(first tasks) (last tasks)]) (partial map :id))
                             (partition-by :job (toxic-seq bad-schedule))))))

(defn implies?
  "Checks whether t1 implies t2"
  [lookup [a b] [c d]]
  (or (and (= a c) (= b d))
      (and (and (= (lookup a) (lookup c))
                (= (lookup b) (lookup d)))
           (and (<= a c) (>= b d)))))

(defn rule-implies?
  "Checks whether a rule implies another rule"
  [lookup rule1 rule2]
  (loop [r1 rule1
         r2 rule2]
    (cond
      (nil? r1)                               true
      (nil? r2)                               false
      (implies? lookup (first r1) (first r2)) (recur (next r1) (next r2))
      :else                                   (recur r1 (next r2)))))

;; This needs to get smarter
;; a1 > b2 sub-implies a1 > b1
(defn accrete-rules
  "Yeah"
  [lookup rules new-rule]
  (if (some #(rule-implies? lookup % new-rule) rules)
    rules
    (conj (remove #(rule-implies? lookup new-rule %) rules)
          new-rule)))

;; (defn accrete-rules [tasks rules new-rule]
;;   (if (some #(vec-subset? % new-rule) rules)
;;     rules
;;     (conj (remove #(vec-subset? new-rule %) rules)
;;           new-rule)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use 'schejule.core-test)

;; (def a-failure
;;   {:machines {9 271, 7 268, 2 343, 1 460},
;;    :jobs {"aag368" 271, "qrs991" 268, "twb787" 186, "xyo468" 460},
;;    :tasks '({:id 4, :job "xyo468", :machine 1, :duration 117, :endby 274, :ord 4, :end-time 460}
;;             {:id 3, :job "xyo468", :machine 2, :duration 157, :ord 3, :end-time 343}
;;             {:id 2, :job "twb787", :machine 2, :duration 186, :ord 2, :end-time 186}
;;             {:id 1, :job "qrs991", :machine 7, :duration 268, :ord 1, :end-time 268}
;;             {:id 0, :job "aag368", :machine 9, :duration 271, :ord 0, :end-time 271}),
;;    :failure 4})

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

(def a-big-failure-3
  {:machines {5 1918, 11 311, 4 1983, 7 1212, 9 812, 0 114, 3 1429, 6 880},
   :jobs {"brq890" 946, "jtj163" 242, "pke479" 414, "uym843" 880, "wim276" 1983},
   :tasks '({:id 17, :job "wim276", :machine 4, :duration 65, :endby 1037, :ord 17, :end-time 1983}
            {:id 16, :job "wim276", :machine 5, :duration 234, :ord 16, :end-time 1918}
            {:id 15, :job "wim276", :machine 5, :duration 255, :ord 15, :end-time 1684}
            {:id 14, :job "wim276", :machine 3, :duration 217, :ord 14, :end-time 1429}
            {:id 13, :job "wim276", :machine 7, :duration 266, :ord 13, :end-time 1212}
            {:id 12, :job "uym843", :machine 6, :duration 68, :endby 6800, :ord 12, :end-time 880}
            {:id 11, :job "uym843", :machine 9, :duration 61, :ord 11, :end-time 812}
            {:id 10, :job "uym843", :machine 3, :duration 293, :ord 10, :end-time 751}
            {:id 9, :job "uym843", :machine 5, :duration 243, :ord 9, :end-time 458}
            {:id 8, :job "uym843", :machine 0, :duration 114, :ord 8, :end-time 114}
            {:id 7, :job "pke479", :machine 9, :duration 172, :endby 600, :ord 7, :end-time 414}
            {:id 6, :job "jtj163", :machine 9, :duration 146, :ord 6, :end-time 242}
            {:id 4, :job "brq890", :machine 7, :duration 261, :ord 5, :end-time 946}
            {:id 3, :job "brq890", :machine 4, :duration 84, :ord 4, :end-time 685}
            {:id 2, :job "brq890", :machine 4, :duration 290, :ord 3, :end-time 601}
            {:id 5, :job "jtj163", :machine 4, :duration 96, :ord 2, :end-time 96}
            {:id 1, :job "brq890", :machine 11, :duration 96, :ord 1, :end-time 311}
            {:id 0, :job "brq890", :machine 5, :duration 215, :ord 0, :end-time 215}),
   :failure 17})

(distil-rule a-big-failure-3)
;; ((4 13))

;; ;=>(0 1 2 3 4 13 14 15 16 17)

(accrete-rules (schejule/by-job (:tasks a-big-failure-3)) '(((2 5) (6 7))) '((4 13)))
;; (((4 13)) ((2 5) (6 7)))

;; ;=>((0 1 2 3 4 13 14 15 16 17) (0 1 2 5 6 7))

;; (some #(and (% :failure) %) (simulate-schedule* sample-1-cleaned '((0 1 2 3 4 13 14 15 16 17) (0 1 2 5 6 7))))

(def a-big-failure-4
  {:machines {5 1457, 11 311, 4 1522, 7 946, 9 812, 0 114, 3 968, 6 880},
   :jobs {"brq890" 946, "jtj163" 242, "wim276" 1522, "pke479" 414, "uym843" 880},
   :tasks '({:id 17, :job "wim276", :machine 4, :duration 65, :endby 1037, :ord 17, :end-time 1522}
            {:id 16, :job "wim276", :machine 5, :duration 234, :ord 16, :end-time 1457}
            {:id 15, :job "wim276", :machine 5, :duration 255, :ord 15, :end-time 1223}
            {:id 14, :job "wim276", :machine 3, :duration 217, :ord 14, :end-time 968}
            {:id 12, :job "uym843", :machine 6, :duration 68, :endby 6800, :ord 13, :end-time 880}
            {:id 11, :job "uym843", :machine 9, :duration 61, :ord 12, :end-time 812}
            {:id 10, :job "uym843", :machine 3, :duration 293, :ord 11, :end-time 751}
            {:id 9, :job "uym843", :machine 5, :duration 243, :ord 10, :end-time 458}
            {:id 8, :job "uym843", :machine 0, :duration 114, :ord 9, :end-time 114}
            {:id 7, :job "pke479", :machine 9, :duration 172, :endby 600, :ord 8, :end-time 414}
            {:id 6, :job "jtj163", :machine 9, :duration 146, :ord 7, :end-time 242}
            {:id 4, :job "brq890", :machine 7, :duration 261, :ord 6, :end-time 946}
            {:id 13, :job "wim276", :machine 7, :duration 266, :ord 5, :end-time 266}
            {:id 3, :job "brq890", :machine 4, :duration 84, :ord 4, :end-time 685}
            {:id 2, :job "brq890", :machine 4, :duration 290, :ord 3, :end-time 601}
            {:id 5, :job "jtj163", :machine 4, :duration 96, :ord 2, :end-time 96}
            {:id 1, :job "brq890", :machine 11, :duration 96, :ord 1, :end-time 311}
            {:id 0, :job "brq890", :machine 5, :duration 215, :ord 0, :end-time 215}),
   :failure 17})

(distil-rule a-big-failure-4)
;; ((0 9) (10 14))

(accrete-rules (schejule/by-job (:tasks a-big-failure-4))
               '(((4 13))
                 ((2 5) (6 7)))
               '((0 9) (10 14)))

;; (((0 9) (10 14)) ((4 13)) ((2 5) (6 7)))

(def a-big-failure-5
  {:machines {4 1421, 9 711, 0 114, 5 1356, 7 1303, 11 668, 3 867, 6 779},
   :jobs {"jtj163" 242, "pke479" 414, "uym843" 779, "wim276" 1421, "brq890" 1303},
   :tasks '({:id 17, :job "wim276", :machine 4, :duration 65, :endby 1037, :ord 17, :end-time 1421}
            {:id 16, :job "wim276", :machine 5, :duration 234, :ord 16, :end-time 1356}
            {:id 15, :job "wim276", :machine 5, :duration 255, :ord 15, :end-time 1122}
            {:id 14, :job "wim276", :machine 3, :duration 217, :ord 14, :end-time 867}
            {:id 12, :job "uym843", :machine 6, :duration 68, :endby 6800, :ord 13, :end-time 779}
            {:id 11, :job "uym843", :machine 9, :duration 61, :ord 12, :end-time 711}
            {:id 10, :job "uym843", :machine 3, :duration 293, :ord 11, :end-time 650}
            {:id 4, :job "brq890", :machine 7, :duration 261, :ord 10, :end-time 1303}
            {:id 3, :job "brq890", :machine 4, :duration 84, :ord 9, :end-time 1042}
            {:id 2, :job "brq890", :machine 4, :duration 290, :ord 8, :end-time 958}
            {:id 1, :job "brq890", :machine 11, :duration 96, :ord 7, :end-time 668}
            {:id 0, :job "brq890", :machine 5, :duration 215, :ord 6, :end-time 572}
            {:id 13, :job "wim276", :machine 7, :duration 266, :ord 5, :end-time 266}
            {:id 9, :job "uym843", :machine 5, :duration 243, :ord 4, :end-time 357}
            {:id 8, :job "uym843", :machine 0, :duration 114, :ord 3, :end-time 114}
            {:id 7, :job "pke479", :machine 9, :duration 172, :endby 600, :ord 2, :end-time 414}
            {:id 6, :job "jtj163", :machine 9, :duration 146, :ord 1, :end-time 242}
            {:id 5, :job "jtj163", :machine 4, :duration 96, :ord 0, :end-time 96}),
   :failure 17})

(distil-rule a-big-failure-5)
;; ((10 14))

(accrete-rules (schejule/by-job (:tasks a-big-failure-5))
               '(((0 9) (10 14)) ((4 13)) ((2 5) (6 7)))
               '((10 14)))

;; ;; Why? (0 9 10 14 15 16 17)
;; ;;      (8 9 10 14 15 16 17)
;; ;;       ?
