(use '[leiningen.exec :only (deps)])
(deps '[[org.clojure/math.combinatorics "0.0.8"]])

(ns game-of-life
  (:require [clojure.math.combinatorics :as combo]))

(def relative-neighbour-positions [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])
(def sleep 500)
(def board-cols 10)
(def board-rows 10)

(defn alive-in-next-gen? [board, row, col]
  (let [alive (get-in board [row col])
        live-neighbour-count (->>
                              relative-neighbour-positions
                              (map
                                (fn [[relrow relcol]]
                                  (get-in board [(+ relrow row) (+ relcol col)])))
                              (filter identity)
                              (count))]
    (or
      (and alive (some #{live-neighbour-count} [2 3]))
      (and (not alive) (= live-neighbour-count 3)))))

(defn cells []
  (combo/cartesian-product (range 0 board-rows) (range 0 board-cols)))

(defn row-output [board]
  (map
    (fn [row]
        (clojure.string/join
          (map
            (fn [cell] (if (identity cell)
              "O"
              " "))
            row)))
        board))

(defn tick [board]
  (let [updated-board (reduce
                         (fn [acc [row col]]
                          (assoc-in acc [row col] (alive-in-next-gen? board row col)))
                         board
                         cells)]
    (do
      (doall (map println (row-output updated-board)))
      (Thread/sleep sleep)
      (tick updated-board))))

(defn game-of-life []
  (let [seed [1 1]]
    (tick seed)))

(game-of-life)


