(ns schejule
  (:require [schejule.analyser :as z]))

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

(defn by-machine [tasks]
  (into {} (map (fn [task] [(:id task) (:machine task)])
                tasks)))

(defn solution-search
  [tasks sols failures]
  (let [job-lookup (by-job tasks)]
    (loop [anti-patterns []
           [s & others]  (z/simulate-schedule* tasks [])]
      (cond
        (Thread/interrupted) nil

        (:failure s)
        (let [anti-patterns' (z/accrete-rules job-lookup anti-patterns s)]
          (prn (str "Failure: " (s :failure)))
          (prn "Anti-Patterns")
          (prn anti-patterns')
          ;; (prn "Constraints")
          ;; (prn constraints)
          (send failures (fn [old] (update old (s :failure) #(inc (or % 0)))))
          (recur anti-patterns' (z/simulate-schedule* tasks anti-patterns')))

        :else
        (let [current (z/get-workspan s)]
          (prn (str "Success: " current))
          (send sols (fn [b] (if (or (nil? b)
                                     (< current (:workspan b)))
                               (assoc s :workspan current)
                               b)))
          (recur anti-patterns others))))))

;; ## Entrypoint

(defn adequate-schedule
  "Searches for a solution and returns the best found in `timeout` milliseconds.
  \"Best\" means the minimum workspan, given the `:endby` constraints encoded into the tasks."
  [tasks millis]
  (let [ts       (clean-tasks tasks)
        failures (agent nil)
        sols     (agent nil)
        f        (future (solution-search ts sols failures))]

    (doall (map prn ts))
    (Thread/sleep millis)
    (future-cancel f)
    (assoc @sols :failures @failures)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use 'schejule-test)

;;; ANTI-PATTERNS ALONG THE WAY
;;
;;     '(((10 0) (4 6 ) (6  22) (22 27) (29 31) (33 38) (38 41) (43 48))
;;       ((10 0) (4 16) (16 22) (22 27) (29 31) (33 38) (38 41) (43 48))
;;       ((0 10)))
;;
;;     '(((10 0) (2 6) (6 4) (4 16) (16 27) (29 31) (33 38) (38 41) (43 48))
;;       ((10 0) (2 22) (22 4) (4 16) (16 27) (29 31) (33 38) (38 41) (43 48))
;;       ((10 0) (4 6) (6 16) (16 27) (29 31) (33 38) (38 41) (43 48))
;;       ((10 0) (4         22) (22 27) (29 31) (33 38) (38 41) (43 48))
;;       ((0 10)))
;;
;;     '(((10 0) (4 16) (16         27) (29 31) (33 38) (38 41) (43 48))
;;       ((10 0) (4         22) (22 27) (29 31) (33 38) (38 41) (43 48))
;;       ((0 10)))
;;
;;     '(((10 0) (2 22) (22 4) (4 6 ) (8 29)                  (29 31) (33 38) (38 41) (43 48))
;;       ((10 0) (2 16) (16 4) (4 6 ) (6  27) (27 22) (22 28) (29 31) (33 38) (38 41) (43 48))
;;       ((10 0)               (4 16) (16 27)                 (29 31) (33 38) (38 41) (43 48))
;;       ((10 0)               (4 22) (22 27)                 (29 31) (33 38) (38 41) (43 48))
;;       ((0 10)))

;; What about a heuristic: look for longest common subseq in several antipatterns.
;; If > x antipatterns contain a common subseq of length > y,
;; nuke the subseq from every antipattern and make it a conjunctive constraint.
;;
;; Maybe I could target it by end-task:
;;
;; - Filter by (= last-task 48)
;; - If number of such aps > x, proceed
;; - Find longest common subseq
;; - Remove said subseq from aps
;; - And make it a conjunctive constraint in its own right.

(adequate-schedule schejule-test/sample-3 (* 1000 5))
