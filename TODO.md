# TODO

- [ ] 2017-03-10 00:53 streamline constraint accretion to focus on offending pairs:

    `[(:a :b :c :d :e :f) (:b :e)] -> [([:a :b] [:d :f]) ([:b :e])]`
    
    ```
    (all
      (conde
        [(fd/> :a :b)]
        [(fd/> :d :f)])
      (conde
        [(fd/> :b :e)]
    ```
    
    - [ ] consider how to eliminate identical if they occur in every sequence
    - [ ] consider that `(a<b | b<c) && (b<c) -> (b<c)`
    - [ ] and `(a<b | b<c) && (c<b) -> (a<b)`

- [ ] 2017-03-03 12:52 investigate reducers library to speed up stream processing
- [x] 2017-03-03 12:53 design a constraint-accretion system to feed to solvo
    - [x] acummulate in due-filter
    - [x] teach solvo to read constraints
