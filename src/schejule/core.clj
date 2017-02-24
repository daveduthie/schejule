(ns schejule.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer [== all everyg lvar run* succeed] :as l]
            [clojure.core.logic.fd :as fd]))

;; ## define helper macro to timeout a blocking function call
;; [Stack Overflow](http://stackoverflow.com/questions/6694530/executing-a-function-with-a-timeout/6697469#6697469)
(defmacro with-timeout [millis & body]
  `(let [future# (future ~@body)]
     (try
       (.get future# ~millis java.util.concurrent.TimeUnit/MILLISECONDS)
       (catch java.util.concurrent.TimeoutException x#
         (do
           (future-cancel future#)
           nil)))))

;; ## Ratiocinate
(defn sort-tasks
  "Sorts tasks by `:job`, then by `:id` to prepare for `solvo`"
  [tasks]
  (->> tasks (sort-by :job) (sort-by :id)))

(defn init-jobs
  "Binds tasks in a job to occur in the correct order.
  `lengths` must be a seq of integers defining no. of tasks in nth job."
  [tasks lengths]
  (if (seq lengths)
    (let [l (first lengths)]
      (all
       (everyg (fn [[x y]] (fd/< x y)) (partition 2 1 (take l tasks)))
       (init-jobs (drop l tasks) (next lengths))))
    succeed))

(defn solvo*
  "Find a feasible ordering for tasks.
  tasks **MUST** be sorted by `:job`, then by `:id`."
  [tasks]
  (let [n (count tasks)
        lengths (->> tasks (partition-by :job) (map count))
        vars (repeatedly n lvar)]
    (run* [q]
          (== q vars)
          (init-jobs vars lengths)
          (everyg #(fd/in % (fd/interval (- n 1))) vars)
          (fd/distinct vars))))

(defn constructo*
  "Wraps solvo*. Associates a sequence of ords with the matching tasks.
  Returns a lazy sequence."
  [tasks]
  (let [ts        (sort-tasks tasks)
        feasible* (solvo* ts)]
    (map (fn [tasks ords]
           (sort-by :ord (map (fn [t ord]
                                (assoc t :ord ord))
                              tasks
                              ords)))
         (repeat ts)
         feasible*)))

;; ## Attempt to filter on `:endby` constraints

(defn due-filter*
  "Filters feasible task orderings by whether jobs will be done on time.
  Returns a lazy sequence."
  [tasks]
  (keep (fn [tasks]
          (reduce (fn [solution task]
                    (let [{:keys [machine duration job id endby]} task

                          end-time (+ duration (max
                                                (or (get-in solution [:machines machine]) 0)
                                                (or (get-in solution [:jobs job]) 0)))]

                      ;; Check if `:endby` constraint has been violated.
                      (if (and endby (< endby end-time))
                        (reduced nil)
                        (-> solution
                            (assoc-in [:machines machine] end-time)
                            (assoc-in [:jobs job] end-time)
                            (update :tasks #(conj % (assoc task :end-time end-time)))))))
                  {}
                  tasks))

        (constructo* tasks)))

(defn get-workspan
  "Get the workspan of a solution."
  [solution]
  (->> solution
       :machines
       (map second)
       (apply max)))

(defn adequate-schedule
  "Searches for a solution and returns the best found in `timeout` milliseconds.
  \"Best\" means the minimum workspan, given the `:endby` constraints encoded into the tasks."
  [millis tasks]
  (with-timeout (+ millis 10)
    (loop [time          (System/currentTimeMillis)
           [head & tail] (due-filter* tasks)
           best          head]

      (if (or (not head) (< millis (- (System/currentTimeMillis) time)))
        best
        (let [best (if (< (get-workspan head) (get-workspan best))
                     head
                     best)]
          (recur time tail best))))))
