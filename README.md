# schejule

Schejule is a library for finding adequate solutions to job shop problems.

See [schejule -- Marginalia](https://daveduthie.github.io/schejule/) for details

Schejule has not been tested very thoroughly at all. at your own risk.

## Usage

(def sample-tasks-2
  [{:id 1 :job :client001 :duration 180 :machine 1}
   {:id 2 :job :client001 :duration 120 :machine 2}
   {:id 7 :job :stuff001 :duration 100 :machine 3}
   {:id 8 :job :stuff001 :duration 150 :machine 4}
   {:id 9 :job :job001 :duration 180 :machine 1}
   {:id 13 :job :thing001 :duration 240 :machine 3}
   {:id 17 :job :thing001 :duration 223 :machine 3}])

(optimal-solution 100 sample-tasks-2)

## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
