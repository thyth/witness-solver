(ns witness-solver.core)

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
                         sides (into #{} (keys (neighbors-idx grid x y)))]
                   :when (= :junction (:dir elem))]
               (condp = sides
                 #{:up :right} [[x y] "\u2514" "\u2517"]
                 #{:right :down} [[x y] "\u250c" "\u250f"]
                 #{:down :left} [[x y] "\u2510" "\u2513"]
                 #{:left :up} [[x y] "\u2518" "\u251b"]
                 #{:down :up :right} [[x y] "\u251c" "\u2523"]
                 #{:down :right :left} [[x y] "\u252c" "\u2533"]
                 #{:up :right :left} [[x y] "\u2534" "\u253b"]
                 #{:down :up :left} [[x y] "\u2524" "\u252b"]
                 #{:down :up :right :left} nil))]
    (reduce (fn [g [k standard bold]]
              (update g k merge {:junction standard
                                 :bold-junction bold}))
            grid (filter identity draw))))

; build-grid of height * width verticies and fill in edges
(defn build-grid [height width]
  (let [grid-w (inc (* 2 width))
        grid-h (inc (* 2 height))]
    (fix-junctions (into {:width  width, :grid-w grid-w
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
                                           (even? y) :horizontal)}]))))))


(defn print-red []
  (print "\u001b[31m"))

(defn print-reset []
  (print "\u001b[37m"))

(defmulti print-element :type)

(defmethod print-element :vertex [v]
  (if (:draw v)
    (print (:draw v))
    (print " ")))
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

(defn- print-row [grid row]
  (dorun (map #(print-element (grid [% row]))
       (range 0 (:grid-w grid))))
  (print "\n"))
(defn print-grid [grid]
  (dorun (map (partial print-row grid)
              (range 0 (:grid-h grid)))))

(defn get-vertex [grid x y]
  (let [grid-x (inc (* 2 x))
        grid-y (inc (* 2 y))]
    (grid [grid-x grid-y])))

(defn mark-start [grid x y]
  (update grid [x y] merge {:draw "\u25c9"
                            :start true}))

(defn mark-end [grid gx gy]
  (update grid [gx gy] merge {:end true}))

(defn update-vertex [grid x y f]
  (let [grid-x (inc (* 2 x))
        grid-y (inc (* 2 y))]
    (update grid [grid-x grid-y] f)))

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
                        (update-vertex g x y
                                       #(merge % {:draw (case c
                                                          3 "\u2234"
                                                          2 ":"
                                                          1 "\u25b2")
                                                  :triangle c})))
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

(defn find-predicate [grid predicate]
  (for [x (range 0 (:grid-w grid))
        y (range 0 (:grid-h grid))
        :let [elem (grid [x y])]
        :when (predicate elem)]
    elem))

(defn find-starts [grid]
  (find-predicate grid :start))

(defn find-ends [grid]
  (find-predicate grid :end))

(defn neighbors [grid gx gy]
  (vec (vals (neighbors-idx grid gx gy))))

(defn search [grid stack check-fn]
  #_(println "Stack:" stack)
  (let [top (assoc (peek stack) :active true)
        x (:x top)
        y (:y top)
        neis (filter #(= :edge (:type %))
                     (remove :active (neighbors grid x y)))
        adj-grid (assoc grid [x y] top)]
    #_(println "Neighs:" neis)
    (if (:end top)
      (if (check-fn adj-grid)
        [stack adj-grid]
        nil)
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
