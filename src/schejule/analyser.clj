(ns schejule.analyser
  (:require [schejule.generator :as gen]))

(defn subseq?
  "Returns true if `sub` contains a subset of `a-vector`'s elements in the same order as `a-vector`.
  Returns false otherwise."
  [s coll]
  (loop [v1 s
         v2 coll]
    (cond
      (nil? v1)                 true
      (nil? v2)                 false
      (= (first v1) (first v2)) (recur (next v1) (next v2))
      :else                     (recur v1 (next v2)))))

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
(defn distil-rule
  "'Rewinds' a bad schedule to identify which subsequence of tasks contributed to the failure"
  [bad-schedule]
  (loop [current       (first (bad-schedule :tasks))
         [head & tail] (next (bad-schedule :tasks))
         bottleneck    [(:id current)]
         start         (- (current :end-time) (current :duration))]
    (cond
      (nil? head)
      bottleneck

      (and (= start (head :end-time))
           (or (= (current :job) (head :job))
               (= (current :machine) (head :machine))))
      (recur head tail (cons (:id head) bottleneck) (- start (head :duration)))

      :else
      (recur current tail bottleneck start))))

(defn disjunctive-pairs
  "Wraps `distil-rule`, trimming result to focus on first and last tasks in each job"
  [job-lookup bad-schedule]
  (partition 2 (rest (mapcat (fn [tasks] [(first tasks) (last tasks)])
                             (partition-by job-lookup bad-schedule)))))

;; (disjunctive-pairs
;;  {0 "bqd174", 7 "fyf071", 20 "tzj588", 27 "umj484", 1 "bqd174", 24 "ufy692", 39 "ybc374", 46 "yuo576", 4 "bqd174", 15 "obu084", 48 "yuo576", 21 "ufy692", 31 "xcx565", 32 "xcx565", 40 "ysx968", 33 "xcx565", 13 "obu084", 22 "ufy692", 36 "ybc374", 41 "ysx968", 43 "ysx968", 29 "umj484", 44 "ysx968", 6 "fyf071", 28 "umj484", 25 "ufy692", 34 "xcx565", 17 "opm162", 3 "bqd174", 12 "obu084", 2 "bqd174", 23 "ufy692", 47 "yuo576", 35 "xcx565", 19 "tzj588", 11 "obu084", 9 "fyf071", 5 "fyf071", 14 "obu084", 45 "yuo576", 26 "umj484", 16 "opm162", 38 "ybc374", 30 "umj484", 10 "lgt782", 18 "tzj588", 42 "ysx968", 37 "ybc374", 8 "fyf071", 49 "yuo576"}
;;  '(10 0 1 2 3 4 22 27 28 29 31 32 33 38 41 42 43 48 49))

;; (defn implies?
;;   "Checks whether t1 implies t2"
;;   [lookup [a b] [c d]]
;;   (or (and (= a c) (= b d))
;;       (and (and (= (lookup a) (lookup c))
;;                 (= (lookup b) (lookup d)))
;;            (and (<= a c) (>= b d)))))

;; (defn rule-implies?
;;   "Checks whether a rule implies another rule"
;;   [lookup rule1 rule2]
;;   (loop [r1 rule1
;;          r2 rule2]
;;     (cond
;;       (nil? r1)                               true
;;       (nil? r2)                               false
;;       (implies? lookup (first r1) (first r2)) (recur (next r1) (next r2))
;;       :else                                   (recur r1 (next r2)))))

;; This needs to get smarter
;; a1 > b2 sub-implies a1 > b1
(defn accrete-rules
  "Yeah"
  [job-lookup rules failure]
  (let [new-rule (disjunctive-pairs job-lookup (distil-rule failure))]
    (if (some #(subseq? (apply concat %) (apply concat new-rule)) rules)
      rules
      (conj (remove #(subseq? (apply concat new-rule) (apply concat %)) rules)
            new-rule))))

