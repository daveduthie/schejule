(ns schejule.analyser
  (:require [schejule.generator :as gen]))

(defn subseq?
  "Returns true if `s` contains a subset of `coll`'s elements in the same order as they appear in `coll`.
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
  "Trims a sequence of tasks to focus on first and last tasks in each job"
  [job-lookup bad-schedule]
  (partition 2 (rest (mapcat (fn [tasks] [(first tasks) (last tasks)])
                             (partition-by job-lookup bad-schedule)))))

(defn accrete-rules
  "Yeah"
  [job-lookup rules failure]
  (let [new-rule (disjunctive-pairs job-lookup (distil-rule failure))]
    (if (some #(subseq? (apply concat %) (apply concat new-rule)) rules)
      rules
      (conj (remove #(subseq? (apply concat new-rule) (apply concat %)) rules)
            new-rule))))

