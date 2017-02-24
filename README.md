# schejule

Schejule is a library for finding adequate solutions to job shop problems.

See [schejule -- Marginalia](https://daveduthie.github.io/schejule/) for details

Schejule has not been tested very thoroughly at all. at your own risk.

## Usage

```
(def sample-tasks-2
  [{:id 1 :job :client001 :duration 180 :machine 1}
   {:id 2 :job :client001 :duration 120 :machine 2}
   {:id 7 :job :stuff001 :duration 100 :machine 3}
   {:id 8 :job :stuff001 :duration 150 :machine 4}
   {:id 9 :job :job001 :duration 180 :machine 1}
   {:id 13 :job :thing001 :duration 240 :machine 3}
   {:id 17 :job :thing001 :duration 223 :machine 3}])

(adequate-solution 100 sample-tasks-2)
;;=> {:machines {1 360, 2 300, 3 563, 4 250},
          :jobs {:client001 300, :stuff001 250, :job001 360, :thing001 563},
          :tasks ({:id 17, :job :thing001, :duration 223, :machine 3, :ord 6, :end-time 563}
                   {:id 13, :job :thing001, :duration 240, :machine 3, :ord 5, :end-time 340}
                   {:id 9, :job :job001, :duration 180, :machine 1, :ord 4, :end-time 360}
                   {:id 8, :job :stuff001, :duration 150, :machine 4, :ord 3, :end-time 250}
                   {:id 7, :job :stuff001, :duration 100, :machine 3, :ord 2, :end-time 100}
                   {:id 2, :job :client001, :duration 120, :machine 2, :ord 1, :end-time 300}
                   {:id 1, :job :client001, :duration 180, :machine 1, :ord 0, :end-time 180})}
```

## License

Copyright © 2017 David Duthie

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
