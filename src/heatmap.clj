(ns heatmap)


(defn tuple-sum [v1 v2] (mapv + v1 v2))

(defn generate-heatmap [maze depth]
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
      ;(println "iteration: " num "Targets: " targets "movable-spots" movable-spots) ; debug
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
          ;(println "new targets:" new-targets "Updates:" updates)
          (recur (dec num) new-targets new-movable-spots))))))

(defn filter-heatmap [heatmap starting-point]
  ;(println "Heatmap startingpoint: " (heatmap starting-point))
    (if (or (not (contains? heatmap starting-point))
            (= (heatmap starting-point) 0))
      (throw (ex-info "No path between points" {})))
  (let [directions [[1 0] [-1 0] [0 1] [0 -1]] ; down, up, right, left
        find-next-cell (fn [current-cell]
                         (some (fn [modifier]
                                 ;(println "current-cell: " current-cell "modifier: " modifier)
                                 (let [cell (tuple-sum current-cell modifier)]
                                   (when (and (contains? heatmap cell)
                                              (> (heatmap cell) (heatmap current-cell)))
                                     cell)))
                               directions))]
    (loop [current-cell starting-point
           filtered-heatmap {starting-point (heatmap starting-point)}]
      (let [next-cell (find-next-cell current-cell)]
        (if next-cell
          (recur next-cell (assoc filtered-heatmap next-cell (heatmap next-cell)))
          filtered-heatmap)))))


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
        depth 150
        ; targets (find-targets maze "E")] 
        heatmap (generate-heatmap maze depth)
        filtered (filter-heatmap heatmap [18 18])]
    ;(println "Heatmap: " heatmap)
    (println "Filtered heatmap: " filtered)
    (println (get heatmap [0 7]))))
(main)