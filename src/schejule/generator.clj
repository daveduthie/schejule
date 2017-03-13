(ns schejule.generator
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer [== all defne everyg lvar run* succeed] :as l]
            [clojure.core.logic.fd :as fd]))

;; ## Helper functions

(defn bind-intra-job-ordero
  "Binds vars in a job to occur in the correct order.
  `lengths` must be a seq of integers defining no. of tasks in nth job."
  [vars lengths]
  (if (seq lengths)
    (let [l (first lengths)]
      (all
       (everyg (fn [[x y]] (fd/< x y)) (partition 2 1 (take l vars)))
       (bind-intra-job-ordero (drop l vars) (next lengths))))
    succeed))

(defn select-nths
  "Like `select-keys` but returns a plain vector of items from coll."
  [from ks]
  (reduce (fn [result s] (conj result (nth from s)))
          []
          ks))

(defne not-ordered-pairso [l]
  ([[[a b] . _]] (fd/> a b))
  ([[[a b] . tail]] (fd/< a b) (not-ordered-pairso tail)))

;; ## Workhorse

;; TODO: experiment with passing run* an atom with a goal inside,
;; realising part of the results,
;; updating the atom,
;; then realising the rest of the results.
(defn gen-sequence*
  "Find a feasible ordering for tasks.
  Returns a lazy sequence."
  [tasks constraints]
  (let [n       (count tasks)
        dom     (dec n)
        lengths (->> tasks (partition-by :job) (map count))
        vars    (repeatedly n lvar)
        rules   (map (fn [c]
                       (map (partial select-nths vars) c))
                     constraints)]
    (run* [q]
          (== q vars)
          (fd/distinct vars)
          (everyg #(fd/in % (fd/interval dom)) vars)
          (bind-intra-job-ordero vars lengths)
          (everyg #(not-ordered-pairso %) rules))))

(defn gen-schedule*
  "Wraps gen-sequence*. Associates a sequence of ords with the matching tasks.
  Tasks **MUST** be sorted by `:job`, then by `:id`.
  Returns a lazy sequence."
  [tasks constraints]
  (let [feasible* (gen-sequence* tasks constraints)]
    (map (fn [tasks ords]
           (sort-by :ord (map (fn [t ord] (assoc t :ord ord))
                              tasks
                              ords)))
         (repeat tasks)
         feasible*)))
