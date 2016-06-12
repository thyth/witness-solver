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

; build-grid of height * width verticies and fill in edges
(defn build-grid [height width]
  (let [grid-w (inc (* 2 width))
        grid-h (inc (* 2 height))]
    (into {:width width, :grid-w grid-w
           :height height, :grid-h grid-h}
          (for [x (range 0 grid-w)
                y (range 0 grid-h)]
            (if (and (odd? x) (odd? y))
              [[x y]
               {:type :vertex
                :x x
                :y y}]
              [[x y]
               {:type :edge
                :x x
                :y y
                :dir (cond (and (even? x) (even? y)) :junction
                           (even? x) :vertical
                           (even? y) :horizontal)}])))))

(defmulti print-element :type)

(defmethod print-element :vertex [v]
  (if (:draw v)
    (print (:draw v))
    (print " ")))
(defmethod print-element :edge [e]
  (if (:draw e)
    (print (:draw e))
    (case (:dir e)
      :junction (if (:active e)
                  (print "\u254b")
                  (print "\u253c"))
      :vertical (if (:active e)
                  (print "\u2503")
                  (print "\u2502"))
      :horizontal (if (:active e)
                    (print "\u2501")
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
  (update grid [x y] merge {:draw "\u2588"
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
                                       #(merge % {:draw c
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

(defn neighbors-idx [grid gx gy]
  (let [up (grid [gx (dec gy)])
        down (grid [gx (inc gy)])
        left (grid [(dec gx) gy])
        right (grid [(inc gx) gy])
        f (fn [n x c] (if c (conj n [x c]) n))]
    (-> {} (f :up up) (f :down down) (f :left left) (f :right right))))

(defn neighbors [grid gx gy]
  (vec (vals (neighbors-idx grid gx gy))))

#_(defn search [grid stack check-fn]
  (println "Stack:" stack)
  (let [top (assoc (peek stack) :active true)
        x (:x top)
        y (:y top)
        neis (filter #(= :edge (:type %))
                     (remove :active (neighbors grid x y)))
        adj-grid (assoc grid [x y] top)]
    (println "Neighs:" neis)
    (if (empty? neis)
      (if (and (:end top)
               (check-fn adj-grid))
        [stack adj-grid]
        nil)
      (reduce (fn [_ step]
                (let [chain (search adj-grid
                                    (conj stack step)
                                    check-fn)]
                  (println "RStack:" (conj stack step))
                  (println "Chain:" chain)
                  (println)
                  (if chain
                    (reduced chain)
                    nil)))
              neis))))

(defn search [grid stack check-fn]
  (println "Stack:" stack)
  (let [top (assoc (peek stack) :active true)
        x (:x top)
        y (:y top)
        neis (filter #(= :edge (:type %))
                     (remove :active (neighbors grid x y)))
        adj-grid (assoc grid [x y] top)]
    #_(println "Neighs:" neis)
    (if (empty? neis)
      (if (and (:end top)
               (check-fn adj-grid))
        [stack adj-grid]
        nil)
      (first (filter identity
                     (map #(search adj-grid
                                   (conj stack %)
                                   check-fn) neis))))))

(defn search-init [grid start check-fn]
  (search grid [start] check-fn))

(defn search-auto [grid check-fn]
  (search-init grid (first (find-starts grid)) check-fn))
