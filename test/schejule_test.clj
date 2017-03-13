(ns schejule-test
  (:require [clojure.test :refer :all]
            [schejule :refer :all]))

;; helpers to generate sample data
(def alphabet (map char (range 97 123)))

(defn gen-name []
  (str (rand-nth alphabet)
       (rand-nth alphabet)
       (rand-nth alphabet)
       (rand-int 10)
       (rand-int 10)
       (rand-int 10)))

(defn gen-job
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

(defn gen-joblist
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

   {:id 9, :job "wim276", :machine 7, :duration 266}
   {:id 10, :job "wim276", :machine 3, :duration 217}
   {:id 11, :job "wim276", :machine 5, :duration 255}
   {:id 12, :job "wim276", :machine 5, :duration 234}
   {:id 13, :job "wim276", :machine 4, :duration 65 :endby 1037}

   {:id 19, :job "uym843", :machine 0, :duration 114}
   {:id 20, :job "uym843", :machine 5, :duration 243}
   {:id 21, :job "uym843", :machine 3, :duration 293}
   {:id 22, :job "uym843", :machine 9, :duration 61}
   {:id 23, :job "uym843", :machine 6, :duration 68 :endby 1000}

   {:id 27, :job "pke479", :machine 9, :duration 172 :endby 600}

   {:id 28, :job "jtj163", :machine 4, :duration 96}
   {:id 29, :job "jtj163", :machine 9, :duration 146}])

(def sample-2
  [{:id 0, :job "aag368", :machine 9, :duration 271}
   {:id 1, :job "twb787", :machine 2, :duration 186}
   {:id 3, :job "xyo468", :machine 2, :duration 157}
   {:id 17, :job "xyo468", :machine 1, :duration 117 :endby 274}
   {:id 4, :job "qrs991", :machine 7, :duration 268}])

(def sample-3
  [{:id 0, :job "obu084", :machine 2, :duration 116}
   {:id 1, :job "obu084", :machine 2, :duration 172}
   {:id 2, :job "obu084", :machine 10, :duration 140}
   {:id 3, :job "obu084", :machine 1, :duration 128}
   {:id 4, :job "obu084", :machine 11, :duration 270}

   {:id 5, :job "xcx565", :machine 3, :duration 268}
   {:id 6, :job "xcx565", :machine 3, :duration 200}
   {:id 7, :job "xcx565", :machine 4, :duration 131}
   {:id 8, :job "xcx565", :machine 5, :duration 228}
   {:id 9, :job "xcx565", :machine 1, :duration 107}

   {:id 10, :job "bqd174", :machine 6, :duration 245}
   {:id 11, :job "bqd174", :machine 4, :duration 295}
   {:id 12, :job "bqd174", :machine 8, :duration 228}
   {:id 13, :job "bqd174", :machine 4, :duration 109}
   {:id 14, :job "bqd174", :machine 8, :duration 96}

   {:id 15, :job "fyf071", :machine 0, :duration 91}
   {:id 16, :job "fyf071", :machine 8, :duration 207}
   {:id 17, :job "fyf071", :machine 9, :duration 72}
   {:id 18, :job "fyf071", :machine 3, :duration 213}
   {:id 19, :job "fyf071", :machine 7, :duration 131}

   {:id 20, :job "umj484", :machine 0, :duration 98}
   {:id 21, :job "umj484", :machine 8, :duration 130}
   {:id 22, :job "umj484", :machine 8, :duration 80}
   {:id 23, :job "umj484", :machine 3, :duration 64}
   {:id 24, :job "umj484", :machine 6, :duration 288}

   {:id 25, :job "tzj588", :machine 9, :duration 285}
   {:id 26, :job "tzj588", :machine 9, :duration 299}
   {:id 27, :job "tzj588", :machine 2, :duration 140}

   {:id 28, :job "opm162", :machine 8, :duration 295}
   {:id 29, :job "opm162", :machine 11, :duration 176}

   {:id 30, :job "ufy692", :machine 0, :duration 242}
   {:id 31, :job "ufy692", :machine 8, :duration 147}
   {:id 32, :job "ufy692", :machine 9, :duration 82}
   {:id 33, :job "ufy692", :machine 7, :duration 166}
   {:id 34, :job "ufy692", :machine 7, :duration 151}

   {:id 35, :job "ysx968", :machine 2, :duration 140}
   {:id 36, :job "ysx968", :machine 4, :duration 65}
   {:id 37, :job "ysx968", :machine 9, :duration 219}
   {:id 38, :job "ysx968", :machine 7, :duration 219}
   {:id 39, :job "ysx968", :machine 10, :duration 179}

   {:id 40, :job "yuo576", :machine 5, :duration 136}
   {:id 41, :job "yuo576", :machine 4, :duration 80}
   {:id 42, :job "yuo576", :machine 11, :duration 257}
   {:id 43, :job "yuo576", :machine 7, :duration 233}
   {:id 44, :job "yuo576", :machine 5, :duration 169 :endby 1075}

   {:id 45, :job "ybc374", :machine 10, :duration 116}
   {:id 46, :job "ybc374", :machine 2, :duration 70}
   {:id 47, :job "ybc374", :machine 4, :duration 249}
   {:id 48, :job "ybc374", :machine 6, :duration 208}

   {:id 49, :job "lgt782", :machine 6, :duration 275 :endby 350}])

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
