(ns schejule-test
  (:require [clojure.test :refer :all]
            [schejule :refer :all]))

;; helpers to generate sample data
(def alphabet (map char (range 97 123)))

(defn- gen-name []
  (str (rand-nth alphabet)
       (rand-nth alphabet)
       (rand-nth alphabet)
       (rand-int 10)
       (rand-int 10)
       (rand-int 10)))

(defn- gen-job
  "Generate a sample job"
  [n start]
  (let [job-name (gen-name)]
    (reduce (fn [tasks x]
              (conj tasks {:id (+ x start)
                           :job job-name
                           :machine (rand-nth (range 12))
                           :duration (rand-nth (range 60 300))}))
            []
            (range n))))

(defn- gen-joblist
  [n]
  (reduce (fn [jobs x]
            (if (= n (count jobs))
              (reduced jobs)
              (let [done    (count jobs)
                    left    (- n done)
                    current (min 5
                                 (Math/floorDiv n 3)
                                 (+ 1 (rand-int left)))]
                (into jobs (gen-job current done)))))
          []
          (range n)))

(def sample-1
  [{:id 0, :job "brq890", :machine 5, :duration 215}
   {:id 1, :job "brq890", :machine 11, :duration 96}
   {:id 2, :job "brq890", :machine 4, :duration 290}
   {:id 3, :job "brq890", :machine 4, :duration 84}
   {:id 4, :job "brq890", :machine 7, :duration 261}

   ;; {:id 5, :job "dct869", :machine 6, :duration 74}
   ;; {:id 6, :job "dct869", :machine 6, :duration 139}
   ;; {:id 7, :job "dct869", :machine 6, :duration 146}
   ;; {:id 8, :job "dct869", :machine 1, :duration 201}

   {:id 9, :job "wim276", :machine 7, :duration 266}
   {:id 10, :job "wim276", :machine 3, :duration 217}
   {:id 11, :job "wim276", :machine 5, :duration 255}
   {:id 12, :job "wim276", :machine 5, :duration 234}
   {:id 13, :job "wim276", :machine 4, :duration 65 :endby 1037}

   {:id 19, :job "uym843", :machine 0, :duration 114}
   {:id 20, :job "uym843", :machine 5, :duration 243}
   {:id 21, :job "uym843", :machine 3, :duration 293}
   {:id 22, :job "uym843", :machine 9, :duration 61}
   {:id 23, :job "uym843", :machine 6, :duration 68 :endby 6800}

   {:id 27, :job "pke479", :machine 9, :duration 172 :endby 600}

   {:id 28, :job "jtj163", :machine 4, :duration 96}
   {:id 29, :job "jtj163", :machine 9, :duration 146}])

(def sample-2
  [{:id 0, :job "aag368", :machine 9, :duration 271}
   {:id 1, :job "twb787", :machine 2, :duration 186}
   {:id 3, :job "xyo468", :machine 2, :duration 157}
   {:id 17, :job "xyo468", :machine 1, :duration 117 :endby 274}
   {:id 4, :job "qrs991", :machine 7, :duration 268}])

(def sample-2-cleaned
  [{:id 0, :job "aag368", :machine 9, :duration 271}
   {:id 1, :job "qrs991", :machine 7, :duration 268}
   {:id 2, :job "twb787", :machine 2, :duration 186}
   {:id 3, :job "xyo468", :machine 2, :duration 157}
   {:id 4, :job "xyo468", :machine 1, :duration 117, :endby 274}])

(def sample-1-cleaned
  [{:id 0, :job "brq890", :machine 5, :duration 215}
   {:id 1, :job "brq890", :machine 11, :duration 96}
   {:id 2, :job "brq890", :machine 4, :duration 290}
   {:id 3, :job "brq890", :machine 4, :duration 84}
   {:id 4, :job "brq890", :machine 7, :duration 261}
   {:id 5, :job "jtj163", :machine 4, :duration 96}
   {:id 6, :job "jtj163", :machine 9, :duration 146}
   {:id 7, :job "pke479", :machine 9, :duration 172, :endby 600}
   {:id 8, :job "uym843", :machine 0, :duration 114}
   {:id 9, :job "uym843", :machine 5, :duration 243}
   {:id 10, :job "uym843", :machine 3, :duration 293}
   {:id 11, :job "uym843", :machine 9, :duration 61}
   {:id 12, :job "uym843", :machine 6, :duration 68, :endby 6800}
   {:id 13, :job "wim276", :machine 7, :duration 266}
   {:id 14, :job "wim276", :machine 3, :duration 217}
   {:id 15, :job "wim276", :machine 5, :duration 255}
   {:id 16, :job "wim276", :machine 5, :duration 234}
   {:id 17, :job "wim276", :machine 4, :duration 65, :endby 1037}])
