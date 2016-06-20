(ns witness-solver.core-test
  (:require [clojure.test :refer :all]
            [witness-solver.core :refer :all]))

(comment
  (-> (build-grid 6 6)
      (mark-start 0 12)
      (assoc [13 0] {:type :edge
                     :x 13 :y 0
                     :dir :horizontal})
      (mark-end 13 0)
      (mark-triangle 0 0 1 0)
      (mark-triangle 1 0 1 0)
      (mark-triangle 2 0 3 0)
      (mark-triangle 3 0 2 0)
      (mark-triangle 4 0 3 0)
      (mark-triangle 5 0 3 0)
      (mark-triangle 0 1 3 0)
      (mark-triangle 1 1 3 0)
      (mark-triangle 2 1 2 0)
      (mark-triangle 3 1 2 0)
      (mark-triangle 4 1 1 0)
      (mark-triangle 5 1 1 0)
      (mark-triangle 0 2 1 0)
      (mark-triangle 1 2 1 0)
      (mark-triangle 2 2 2 0)
      (mark-triangle 3 2 2 0)
      (mark-triangle 4 2 2 0)
      (mark-triangle 5 2 1 0)
      (mark-triangle 0 3 2 0)
      (mark-triangle 1 3 1 0)
      (mark-triangle 2 3 2 0)
      (mark-triangle 3 3 1 0)
      (mark-triangle 4 3 2 0)
      (mark-triangle 5 3 2 0)
      (mark-triangle 0 4 2 0)
      (mark-triangle 1 4 2 0)
      (mark-triangle 2 4 2 0)
      (mark-triangle 3 4 2 0)
      (mark-triangle 4 4 3 0)
      (mark-triangle 5 4 2 0)
      (mark-triangle 0 5 2 0)
      (mark-triangle 1 5 2 0)
      (mark-triangle 2 5 3 0)
      (mark-triangle 3 5 2 0)
      (mark-triangle 4 5 3 0)
      (mark-triangle 5 5 2 0)
      answer
      second
      print-grid))

(comment
    (time
        (-> (build-grid 4 4)
            (assoc [9 0]
                   {:type :edge
                    :x 9 :y 0
                    :dir :horizontal})
            (mark-start 0 8)
            (mark-end 9 0)
            (mark-block 0 0 0)
            (mark-block 0 1 1)
            (mark-star 1 1 1)
            (mark-star 2 1 0)
            (mark-block 2 0 0)
            (mark-triangle 1 3 3 0)
            answer-shortest
            second
            print-grid)))

(comment
    (time (-> (create-challenge-puzzle "223001000010031")
           answer-shortest
           second
           print-grid)))