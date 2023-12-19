(ns heatmap)

:maze_element/start
:maze_element/exit
:maze_element/block
:maze_element/path

;(let [:maze_element/start "^"])
;(let [:maze_element/exit "E"])
;(let [:maze_element/block "#"])
;(let [:maze_element/path " "])

(println "Hello")

(defn tuple-sum [& tuples] (map + tuples))

;(defn tuple-sum [tuple1 tuple2]
;  (mapv + tuple1 tuple2))


;(tuple-sum [1 2 3] [4 5 6] [7 8 9])

(
  defn generate_heatmap [maze depth]
  (let [movable_spots (clojure.lang.PersistentHashMap/EMPTY)]
    (doseq [[y row] (map-indexed vector maze)]
      (doseq [[x cell] (map-indexed vector row)]
        (when (not= cell "#") (conj movable_spots [[y x] 0] []))))
    (def targets [])
    (doseq [[y row] (map-indexed vector maze)]
      (doseq [[x cell] (map-indexed vector row)]
        (when (not= cell "E") (conj targets [y x]))))
    (doseq [num (range depth 0 -1)]
      ((def new_targets [])
    ))))

(defn filter-heatmap [heatmap starting-point]
  (if (or (not (contains? heatmap starting-point))
          (= (heatmap starting-point) 0))
    (throw (ex-info "No path between points" {}))
    (loop [filtered-heatmap {starting-point (heatmap starting-point)}
             ^:volatile-mutable current-cell starting-point]
        (if-let [next-cell (first
                            (filter (fn [modifier]
                                      (let [cell (tuple-sum current-cell modifier)]
                                        (and (contains? heatmap cell)
                                             (> (heatmap cell) (heatmap current-cell)))))
                                    [[1 0] [-1 0] [0 1] [0 -1]]))]
          (recur (assoc filtered-heatmap next-cell (heatmap next-cell))
                 next-cell)
          filtered-heatmap))))

(defn generate-heatmap-old [maze depth]
  (let [movable-spots (->> maze (map-indexed
                                 (fn [y row]
                                   (map-indexed (fn [x cell]
                                                  (when (not= (str cell) "#")
                                                    [[y x] 0]))
                                                row)))
                           (apply concat)
                           (filter identity)
                           (into {}))
        targets (->> maze
                     (map-indexed (fn [y row]
                                    (map-indexed (fn [x cell]
                                                   (when (= (str cell) "E")
                                                     [y x]))
                                                 row)))
                     (apply concat)
                     (filter identity))]

    (loop [num depth
           targets targets
           movable-spots movable-spots]
      (println "iteration: " num "Targets: " targets "movable-spots" movable-spots) ; debug
      (if (zero? num)
        movable-spots
        (let [next-num (dec num)
              updates (->> targets
                           (mapcat (fn [target]
                                     (when (contains? movable-spots target)
                                       (for [modifier [[1 0] [-1 0] [0 1] [0 -1]]
                                             :let [cell (tuple-sum target modifier)]
                                             :when (and (contains? movable-spots cell)
                                                        (< (get movable-spots cell 0) next-num))]
                                         [cell next-num]))))
                           (distinct))
              new-movable-spots (reduce (fn [acc [k v]]
                                          (assoc acc k v))
                                        movable-spots
                                        updates)
              new-targets (map first updates)]
          (println "new targets:" new-targets "Updates:" updates)
          (recur (dec num) new-targets new-movable-spots))))))
                                                              
(defn generate-heatmap [maze depth]
  (let [movable-spots (->> maze (map-indexed
                                 (fn [y row]
                                   (map-indexed (fn [x cell]
                                                  (when (not= (str cell) "#")
                                                    [y x]))
                                                row)))
                           (apply concat)
                           (filter identity)
                           (into {}))
        targets (->> maze
                     (map-indexed (fn [y row]
                                    (map-indexed (fn [x cell]
                                                   (when (= (str cell) "E")
                                                     [y x]))
                                                 row)))
                     (apply concat)
                     (filter identity))]

    (loop [num depth
           targets targets
           movable-spots movable-spots]
      (println "iteration: " num "Targets: " targets "movable-spots" movable-spots) ; debug
      (if (zero? num)
        movable-spots
        (let [next-num (dec num)
              updates (->> targets
                           (mapcat (fn [target]
                                     (when (contains? movable-spots target)
                                       (for [modifier [[1 0] [-1 0] [0 1] [0 -1]]
                                             :let [cell (tuple-sum target modifier)]
                                             :when (and (contains? movable-spots cell)
                                                        (< (get movable-spots cell 0) next-num))]
                                         cell))))
              new-movable-spots (reduce (fn [acc k]
                                          (assoc acc k next-num))
                                        movable-spots
                                        updates)
              new-targets updates]
          (println "new targets:" new-targets "Updates:" updates)
          (recur (dec num) new-targets new-movable-spots))))))


(defn main []
  (let [maze ["#######E########E####################",
              "# ### #   ###### #    #     #     # E",
              "# ### ### #      #  #    #     #    #",
              "# ### # # # ###### ##################",
              "#            #       #    #   #   # #",
              "#  # ##      # ##### #  # # # # # # #",
              "#  #         #   #   #  # # # # #   #",
              "#  ######   ###  #  ### # # # # ### #",
              "#  #    #               #   #   #   #",
              "#  # ## ########   ## ###########   #",
              "#    ##          ###                #",
              "# ## #############  ###   ####   ## #",
              "#  ### ##         #  #  #           #",
              "#  #   ## ####     #    #      ###  #",
              "#  # #### #  #     #    #####       #",
              "#  #      #      ###           ##   #",
              "#  #####           #   ##   #   #   #",
              "#                                   #",
              "##################^##################"]
        depth 20
        ; targets (find-targets maze "E")] 
        heatmap (generate-heatmap maze depth)]
    (println "Heatmap: " heatmap)
    ;(println (get heatmap [0 7])
    ))

(main)
