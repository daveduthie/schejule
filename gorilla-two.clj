;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns lacuna
  (:require [gorilla-plot.core :as plot]
            [schejule :refer :all]
            [schejule.analyser :as z]
            [schejule.generator :as g]
            [schejule-test :as t]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## ANTI-PATTERNS ALONG THE WAY
;; **

;; @@
'(((10 0) (4 6 ) (6  22) (22 27) (29 31) (33 38) (38 41) (43 48))
 ((10 0) (4 16) (16 22) (22 27) (29 31) (33 38) (38 41) (43 48))
 ((0 10)))

'(((10 0) (2 6) (6 4) (4 16) (16 27) (29 31) (33 38) (38 41) (43 48))
 ((10 0) (2 22) (22 4) (4 16) (16 27) (29 31) (33 38) (38 41) (43 48))
 ((10 0) (4 6) (6 16) (16 27) (29 31) (33 38) (38 41) (43 48))
 ((10 0) (4         22) (22 27) (29 31) (33 38) (38 41) (43 48))
 ((0 10)))

'(((10 0) (4 16) (16         27) (29 31) (33 38) (38 41) (43 48))
 ((10 0) (4         22) (22 27) (29 31) (33 38) (38 41) (43 48))
 ((0 10)))

'(((10 0) (2 22) (22 4) (4 6 ) (8 29)                  (29 31) (33 38) (38 41) (43 48))
 ((10 0) (2 16) (16 4) (4 6 ) (6  27) (27 22) (22 28) (29 31) (33 38) (38 41) (43 48))
 ((10 0)               (4 16) (16 27)                 (29 31) (33 38) (38 41) (43 48))
 ((10 0)               (4 22) (22 27)                 (29 31) (33 38) (38 41) (43 48))
 ((0 10)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"(10 0)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>22</span>","value":"22"}],"value":"(2 22)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>22</span>","value":"22"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"(22 4)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"}],"value":"(4 6)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>29</span>","value":"29"}],"value":"(8 29)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>29</span>","value":"29"},{"type":"html","content":"<span class='clj-long'>31</span>","value":"31"}],"value":"(29 31)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>33</span>","value":"33"},{"type":"html","content":"<span class='clj-long'>38</span>","value":"38"}],"value":"(33 38)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>38</span>","value":"38"},{"type":"html","content":"<span class='clj-long'>41</span>","value":"41"}],"value":"(38 41)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>43</span>","value":"43"},{"type":"html","content":"<span class='clj-long'>48</span>","value":"48"}],"value":"(43 48)"}],"value":"((10 0) (2 22) (22 4) (4 6) (8 29) (29 31) (33 38) (38 41) (43 48))"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"(10 0)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>16</span>","value":"16"}],"value":"(2 16)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>16</span>","value":"16"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"(16 4)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"}],"value":"(4 6)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-long'>27</span>","value":"27"}],"value":"(6 27)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>27</span>","value":"27"},{"type":"html","content":"<span class='clj-long'>22</span>","value":"22"}],"value":"(27 22)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>22</span>","value":"22"},{"type":"html","content":"<span class='clj-long'>28</span>","value":"28"}],"value":"(22 28)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>29</span>","value":"29"},{"type":"html","content":"<span class='clj-long'>31</span>","value":"31"}],"value":"(29 31)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>33</span>","value":"33"},{"type":"html","content":"<span class='clj-long'>38</span>","value":"38"}],"value":"(33 38)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>38</span>","value":"38"},{"type":"html","content":"<span class='clj-long'>41</span>","value":"41"}],"value":"(38 41)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>43</span>","value":"43"},{"type":"html","content":"<span class='clj-long'>48</span>","value":"48"}],"value":"(43 48)"}],"value":"((10 0) (2 16) (16 4) (4 6) (6 27) (27 22) (22 28) (29 31) (33 38) (38 41) (43 48))"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"(10 0)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>16</span>","value":"16"}],"value":"(4 16)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>16</span>","value":"16"},{"type":"html","content":"<span class='clj-long'>27</span>","value":"27"}],"value":"(16 27)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>29</span>","value":"29"},{"type":"html","content":"<span class='clj-long'>31</span>","value":"31"}],"value":"(29 31)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>33</span>","value":"33"},{"type":"html","content":"<span class='clj-long'>38</span>","value":"38"}],"value":"(33 38)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>38</span>","value":"38"},{"type":"html","content":"<span class='clj-long'>41</span>","value":"41"}],"value":"(38 41)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>43</span>","value":"43"},{"type":"html","content":"<span class='clj-long'>48</span>","value":"48"}],"value":"(43 48)"}],"value":"((10 0) (4 16) (16 27) (29 31) (33 38) (38 41) (43 48))"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"(10 0)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>22</span>","value":"22"}],"value":"(4 22)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>22</span>","value":"22"},{"type":"html","content":"<span class='clj-long'>27</span>","value":"27"}],"value":"(22 27)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>29</span>","value":"29"},{"type":"html","content":"<span class='clj-long'>31</span>","value":"31"}],"value":"(29 31)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>33</span>","value":"33"},{"type":"html","content":"<span class='clj-long'>38</span>","value":"38"}],"value":"(33 38)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>38</span>","value":"38"},{"type":"html","content":"<span class='clj-long'>41</span>","value":"41"}],"value":"(38 41)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>43</span>","value":"43"},{"type":"html","content":"<span class='clj-long'>48</span>","value":"48"}],"value":"(43 48)"}],"value":"((10 0) (4 22) (22 27) (29 31) (33 38) (38 41) (43 48))"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"}],"value":"(0 10)"}],"value":"((0 10))"}],"value":"(((10 0) (2 22) (22 4) (4 6) (8 29) (29 31) (33 38) (38 41) (43 48)) ((10 0) (2 16) (16 4) (4 6) (6 27) (27 22) (22 28) (29 31) (33 38) (38 41) (43 48)) ((10 0) (4 16) (16 27) (29 31) (33 38) (38 41) (43 48)) ((10 0) (4 22) (22 27) (29 31) (33 38) (38 41) (43 48)) ((0 10)))"}
;; <=

;; **
;;; ## Directing the search
;; **

;; **
;;; What about a heuristic: look for longest common subseq in several antipatterns.
;;; If > x antipatterns contain a common subseq of length > y,
;;; nuke the subseq from every antipattern and make it a conjunctive constraint.
;;; 
;;; Maybe I could target it by end-task:
;;; 
;;; - Filter by (= last-task 48)
;;; - If number of such aps > x, proceed
;;; - Find longest common subseq
;;; - Remove said subseq from aps
;;; - And make it a conjunctive constraint in its own right.
;;; 
;;; What about pulling out that conjunctive constraint and putting it as the first branch of a `conde` expression to entice core.logic to exlore that part of the search space?
;;; 
;; **
