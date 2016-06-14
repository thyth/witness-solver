(ns witness-solver.core
  (:require [clojure.set :as set]))

; A 3x2 vertex grid:
;   0 1 2 3 4 x
; 0 e e e e e
; 1 e v e v e
; 2 e e e e e
; 3 e v e v e
; 4 e e e e e
; 5 e v e v e
; 6 e e e e e
; y

; build-grid of height * width verticies and fill in edges
(defn build-grid [height width]
  (let [grid-w (inc (* 2 width))
        grid-h (inc (* 2 height))]
    (into {:width  width, :grid-w grid-w
           :height height, :grid-h grid-h}
          (for [x (range 0 grid-w)
                y (range 0 grid-h)]
            (if (and (odd? x) (odd? y))
              [[x y]
               {:type :vertex
                :x    x
                :y    y}]
              [[x y]
               {:type :edge
                :x    x
                :y    y
                :dir  (cond (and (even? x) (even? y)) :junction
                            (even? x) :vertical
                            (even? y) :horizontal)}])))))


(defn print-red []
  (print "\u001b[34m"))

(def reset-color "\u001b[37m")

(defn print-reset []
  (print reset-color))

(defmulti print-element :type)

(defmethod print-element :vertex [v]
  (if (:draw v)
    (print (:draw v))
    (cond
      (:triangle v) (case (:triangle v)
                      3 (print "\u2234")
                      2 (print ":")
                      1 (print "\u25b2")
                      (print " "))
      :default (print " "))))
(defmethod print-element :edge [e]
  (if (:draw e)
    (if (:active e)
      (do (print-red) (print (:draw e)) (print-reset))
      (print (:draw e)))
    (case (:dir e)
      :junction (if (:active e)
                  (do (print-red)
                      (if (:bold-junction e)
                        (print (:bold-junction e))
                        (print "\u254b"))
                      (print-reset))
                  (if (:junction e)
                    (print (:junction e))
                    (print "\u253c")))
      :vertical (if (:active e)
                  (do (print-red) (print "\u2503") (print-reset))
                  (print "\u2502"))
      :horizontal (if (:active e)
                    (do (print-red) (print "\u2501") (print-reset))
                    (print "\u2500"))
      (print "?"))))
(defmethod print-element :default [_]
  (print " "))

(defn- print-row [grid row]
  (dorun (map #(print-element (grid [% row]))
       (range 0 (:grid-w grid))))
  (print "\n"))

(defn neighbors-idx [grid gx gy]
  (let [up (grid [gx (dec gy)])
        down (grid [gx (inc gy)])
        left (grid [(dec gx) gy])
        right (grid [(inc gx) gy])
        f (fn [n x c] (if c (conj n [x c]) n))]
    (-> {} (f :up up) (f :right right) (f :down down) (f :left left))))

(defn fix-junctions [grid]
  (let [draw (for [x (range 0 (:grid-w grid))
                   y (range 0 (:grid-h grid))
                   :let [elem (grid [x y])
                         neis (neighbors-idx grid x y)
                         sides (into #{} (keys neis))
                         active-sides (into #{}
                                            (map first
                                                 (filter #(-> %
                                                              second
                                                              :active)
                                                         neis)))]
                   :when (= :junction (:dir elem))]
               (condp = sides
                 #{:up :right} [[x y]
                                "\u2514"
                                (condp = active-sides
                                  #{:up} "\u2516"
                                  #{:right} "\u2515"
                                  "\u2517")]
                 #{:right :down} [[x y]
                                  "\u250c"
                                  (condp = active-sides
                                    #{:right} "\u250d"
                                    #{:down} "\u250e"
                                    "\u250f")]
                 #{:down :left} [[x y]
                                 "\u2510"
                                 (condp = active-sides
                                   #{:left} "\u2511"
                                   #{:down} "\u2512"
                                   "\u2513")]
                 #{:left :up} [[x y]
                               "\u2518"
                               (condp = active-sides
                                 #{:left} "\u2519"
                                 #{:up} "\u251a"
                                 "\u251b")]
                 ; degenerate horizontal
                 #{:right :left} [[x y]
                                  "\u2500"
                                  (condp = active-sides
                                    #{:right} "\u257c"
                                    #{:left} "\u257e"
                                    "\u2501")]
                 ; degenerate vertical
                 #{:up :down} [[x y]
                               "\u2502"
                               (condp = active-sides
                                 #{:up} "\u257f"
                                 #{:down} "\u257d"
                                 "\u2503")]
                 #{:down :up :right} [[x y]
                                      "\u251c"
                                      (condp = active-sides
                                        #{:down :up} "\u2520"
                                        #{:down :right} "\u2522"
                                        #{:up :right} "\u2521"
                                        #{:down} "\u251f"
                                        #{:up} "\u251e"
                                        #{:right} "\u251d"
                                        "\u2523")]
                 #{:down :right :left} [[x y]
                                        "\u252c"
                                        (condp = active-sides
                                          #{:down :right} "\u2532"
                                          #{:down :left} "\u2531"
                                          #{:left :right} "\u252f"
                                          #{:down} "\u2530"
                                          #{:left} "\u252d"
                                          #{:right} "\u252e"
                                          "\u2566")]
                 #{:up :right :left} [[x y]
                                      "\u2534"
                                      (condp = active-sides
                                        #{:up :right} "\u253a"
                                        #{:up :left} "\u2539"
                                        #{:right :left} "\u2537"
                                        #{:up} "\u2538"
                                        #{:right} "\u2536"
                                        #{:left} "\u2535"
                                        "\u253b")]
                 #{:down :up :left} [[x y]
                                     "\u2524"
                                     (condp = active-sides
                                       #{:down :up} "\u2528"
                                       #{:down :left} "\u252a"
                                       #{:left :up} "\u2529"
                                       #{:down} "\u2527"
                                       #{:up} "\u2526"
                                       #{:left} "\u2525"
                                       "\u252b")]
                 #{:down :up :right :left} [[x y]
                                            "\u253c"
                                            (condp = active-sides
                                              #{:down :left} "\u2545"
                                              #{:left :up} "\u2543"
                                              #{:up :right} "\u2544"
                                              #{:right :down} "\u2546"
                                              #{:left :right} "\u253f"
                                              #{:up :down} "\u2542"
                                              #{:down} "\u2541"
                                              #{:up} "\u2540"
                                              #{:right} "\u253e"
                                              #{:left} "\u253d"
                                              "\u254b")]))]
    (reduce (fn [g [k standard bold]]
              (update g k merge {:junction standard
                                 :bold-junction bold}))
            grid (filter identity draw))))

(defn print-grid [grid]
  (let [junction-grid (fix-junctions grid)]
    (dorun (map (partial print-row junction-grid)
                (range 0 (:grid-h junction-grid))))))

(defn get-vertex [grid x y]
  (let [grid-x (inc (* 2 x))
        grid-y (inc (* 2 y))]
    (grid [grid-x grid-y])))

(defn mark-start [grid x y]
  (update grid [x y] merge {:draw "\u25cf"
                            :start true}))

(defn mark-end [grid gx gy]
  (update grid [gx gy] merge {:end true}))

(defn update-vertex [grid x y f]
  (let [grid-x (inc (* 2 x))
        grid-y (inc (* 2 y))]
    (update grid [grid-x grid-y] f)))

(defn mark-triangle [grid vx vy level]
  (update-vertex grid vx vy #(merge % {:triangle level})))

(defn create-challenge-puzzle [input]
  (let [constraints (mapv vec (partition 4 (format "%-16s" input)))
        grid (build-grid 4 4)
        insert (for [x (range 0 4)
                     y (range 0 4)
                     :let [constr (get-in constraints [y x])]
                     :when (condp = constr
                             \1 1
                             \2 2
                             \3 3
                             nil)]
                 [x y (inc (- (int constr) (int \1)))])
        gridc (reduce (fn [g [x y c]]
                        (mark-triangle g x y c))
                      grid insert)]
    (-> gridc (mark-start 0 8) (mark-end 8 0))))

(defn triangle-violated? [grid x y]
  (let [v (get-vertex grid x y)
        up (grid [(:x v) (dec (:y v))])
        down (grid [(:x v) (inc (:y v))])
        left (grid [(dec (:x v)) (:y v)])
        right (grid [(inc (:x v)) (:y v)])]
    (if (:triangle v)
      (not= (:triangle v) (count (filter :active [up down left right]))))))

(defn triangles-violations [grid]
  (for [x (range 0 (:width grid))
        y (range 0 (:height grid))
        :let [v (get-vertex grid x y)]
        :when (and (:triangle v)
                   (triangle-violated? grid x y))]
    v))

(defn triangles-satisfied? [grid]
  (empty? (triangles-violations grid)))

(defn bfs [grid open closed neighbor-fn]
  (if-let [top (peek open)]
    (let [neis (remove closed (neighbor-fn grid top))]
      (recur grid (into (pop open) neis) (conj closed top) neighbor-fn))
    closed))

; which verts adjacent to this one are reachable?
(defn reachable-adj-verts [grid [x y]]
  (let [vert (get-vertex grid x y)
        neis (neighbors-idx grid (:x vert) (:y vert))
        clear-dirs (remove #(-> % second :active) neis)
        clear-verts (filter identity
                            (map (fn [[dir edge]]
                                   (dir (neighbors-idx grid
                                                       (:x edge)
                                                       (:y edge))))
                                 clear-dirs))
        vert-coords (map (fn [{x :x y :y}]
                           [(/ (dec x) 2)
                            (/ (dec y) 2)])
                         clear-verts)]
    vert-coords))

; find all vertex coordinates that share a region with the specified vertex
(defn find-region [grid vx vy]
  (bfs grid
       [[vx vy]]
       #{}
       reachable-adj-verts))

(defn mark-regions [grid]
  (let [verts (for [x (range 0 (:width grid))
                    y (range 0 (:height grid))
                    :let [v (get-vertex grid x y)]
                    :when v]
                [x y])
        vset (into #{} verts)]
    (loop [grid grid
           vset vset
           idx 0]
      (if-let [[tx ty :as top] (first vset)]
        (let [region (find-region grid tx ty)]
          (recur (reduce (fn [g [x y]]
                           (update-vertex g x y #(assoc % :region idx)))
                         grid
                         region)
                 (set/difference vset region)
                 (inc idx)))
        (assoc grid :regions idx)))))

(defn print-regions [grid]
  (let [marked (mark-regions grid)
        drawn (into {}
                    (map (fn [[k v]]
                           (if (:region v)
                             [k (assoc v :draw (:region v))]
                             [k v]))
                         marked))]
    (print-grid drawn)))

(defn index-color [idx]
  (let [shift (rem (+ idx 1) 7)]
    (str "\u001B[3" shift "m")))

(defn mark-block [grid vx vy idx]
  (update-vertex grid vx vy #(merge % {:draw (str (index-color idx)
                                                  "\u25a0" reset-color)
                                       :block idx
                                       :square true})))

(defn mark-star [grid vx vy idx]
  (update-vertex grid vx vy #(merge % {:draw (str (index-color idx)
                                                  "\u2737" reset-color)
                                       :block idx
                                       :star true})))

(defn mark-anticonstraint [grid vx vy]
  (update-vertex grid vx vy #(merge % {:draw "Y"
                                       :anticonstraint true})))

(defn find-predicate [grid predicate]
  (for [x (range 0 (:grid-w grid))
        y (range 0 (:grid-h grid))
        :let [elem (grid [x y])]
        :when (predicate elem)]
    elem))

(defn find-region-id [grid id]
  (find-predicate grid #(= id (:region %))))

(defn iterate-region-violations [grid violation-fn]
  (mapcat (partial violation-fn grid)
          (range 0 (or (:regions grid) 0))))

(defn block-violations-region [grid id]
  (let [region (find-region-id grid id)
        blocks (filter :square region)
        groups (group-by :block blocks)]
    (if (< 1 (count groups))
      blocks)))

(defn block-violations [grid]
  (iterate-region-violations grid block-violations-region))

(defn block-satisfied? [grid]
  (empty? (block-violations grid)))

(defn star-violations-region [grid id]
  (let [region (find-region-id grid id)
        stars (filter :star region)
        colored (into #{} (filter :block region))]
    (map (fn [star]
           (let [minus (disj colored star)
                 same-color (filter #(= (:block star)
                                        (:block %))
                                    minus)]
             (if (not= 1 (count same-color))
               star)))
         stars)))

(defn star-violations [grid]
  (iterate-region-violations grid star-violations-region))

(defn star-satisfied? [grid]
  (empty (star-violations grid)))

(defn find-starts [grid]
  (find-predicate grid :start))

(defn find-ends [grid]
  (find-predicate grid :end))

(defn neighbors [grid gx gy]
  (vec (vals (neighbors-idx grid gx gy))))

(defn precheck-grid [grid]
  (-> grid
      mark-regions))

; TODO anticonstraint by region
(defn all-violations [grid]
  (concat (triangles-violations grid)
          (block-violations grid)
          (star-violations grid)))

; run all constraint checks
(defn full-check [grid]
  (let [prechecked (precheck-grid grid)]
    (if (empty? (all-violations prechecked))
      prechecked)))

(defn search [grid stack check-fn]
  #_(println "Stack:" stack)
  (let [top (assoc (peek stack) :active true)
        x (:x top)
        y (:y top)
        neis (filter #(= :edge (:type %))
                     (remove :active (neighbors grid x y)))
        adj-grid (assoc grid [x y] top)
        final-grid (promise)]
    #_(println "Neighs:" neis)
    (if (and (:end top)
             (if-let [checked (check-fn adj-grid)]
               (do (deliver final-grid checked)
                   checked)))
      [stack @final-grid]
      (loop [steps neis]
        (if-let [step (first steps)]
          (let [path (search adj-grid (conj stack step) check-fn)]
            (if path
              path
              (if (next steps)
                (recur (next steps))))))))))

(defn search-init [grid start check-fn]
  (search grid [start] check-fn))

(defn search-auto [grid check-fn]
  (search-init grid (first (find-starts grid)) check-fn))

(defn answer [grid]
  (search-auto grid full-check))
