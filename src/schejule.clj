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
        (recur
         (conj clean (assoc head :id id))
         tail
         (inc id))))))

(defn by-job [tasks]
  (into {} (map (fn [task] [(:id task) (:job task)])
                tasks)))

(defn adequate-schedule
  "Searches for a solution and returns the best found in `timeout` milliseconds.
  \"Best\" means the minimum workspan, given the `:endby` constraints encoded into the tasks."
  [tasks millis]
  (let [sols       (a/chan 1000)
        tasks      (clean-tasks tasks)
        job-lookup (by-job tasks)]

    ; place solutions on channel
    (>!! sols :yohoho)

    ; take solutions from channel retaining best
    (<!! sols)))

;; (use 'schejule.core-test)

;; (adequate-schedule sample-2 600)
