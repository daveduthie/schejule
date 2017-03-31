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
