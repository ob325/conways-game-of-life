(ns life.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def size 100)         ;; x-y size of board
(def square-size 10)  ;; pixel dimension of displayed grid


(defn blank-border
  "Returns copy of grid modified with blank edges"
  [state]
  (as-> state s
    (assoc s 0 (vec (repeat size 0)))          ;; top
    (assoc s (dec size) (vec (repeat size 0))) ;; bottom
    (map #(assoc % 0 0) s)                     ;; left 
    (map #(assoc % (dec size) 0) s)            ;; right
    (vec s)))


(defn setup
  "Returns grid with squares randomly alive at time zero"
  []
  (q/rect-mode :corner)
  (q/frame-rate 60)
  (q/color-mode :hsb)
  (->> (vec (map vec (partition size (take (* size size) (repeatedly #(rand-int 2))))))
       (blank-border)))


(def neighbor-offsets
  "Relative positions of all 8 neighbor cells"
  (->> (for [x (range -1 2) y (range -1 2)] [x y])
       (filter #(not= % [0 0]))))


(defn count-of-neighbors
  "Returns count of living orthogonal neighbors around point x y in state s"
  [x y s]
  (reduce (fn [acc v]
            (+ acc (get-in s [(+ x (first v))
                              (+ y (nth v 1))])))
          0
          neighbor-offsets))


(defn new-cell-state
  "Returns state of cell for next generation, given current state and n living neighbors"
  [n-neighbors current-state]
  (cond (and (#{2 3} n-neighbors)
             (= 1 current-state)) 1
        (and (= 0 current-state)
             (= 3 n-neighbors)) 1
        :else 0))

  
(defn update-state
  "Returns next generation based on current state"
  [state]
  (let [new-state (vec (repeat size (vec (repeat size 0))))
        positions (for [x (range 1 (dec size))
                        y (range 1 (dec size))]
                    [x y])]
    (reduce (fn [acc v]
              (assoc-in acc v (new-cell-state (count-of-neighbors (first v)
                                                                  (nth v 1)
                                                                  state)
                                              (get-in state v))))
            new-state
            positions)))


(defn draw-state [state]
  (q/stroke 0 0 255)
  (doseq [x (range size)
          y (range size)]
    (apply q/fill (if (zero? (nth (nth state y) x))
                      '(0 0 255)
                      '(0 0 0)))
    (q/rect (* x square-size) (* square-size y) square-size square-size)))

          
(q/defsketch life
  :title "Conway's Game of Life"
  :size [1000 1000]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
