(ns game-of-life
  (:require [clojure.walk]))

(def relative-neighbour-positions [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])
(def sleep 300)
(def board-cols 100)
(def board-rows 30)
(def live-percentage 0.1)
(def padding 10)

(defn deep-vectorize [col]
  (clojure.walk/postwalk
    (fn [x] (if (coll? x)
              (into [] x)
              x))
    col))

(defn seed []
  (deep-vectorize
   (take board-rows
      (repeatedly
        (fn [] (take board-cols
          (repeatedly
            #(< (rand 1) live-percentage))))))))

(defn print-board [board]
  (let [row-output (fn [board]
                     (map
                      (fn [row] (clojure.string/join
                                  (map #(if (identity %) "O" " ") row)))
                      board))]
  (print (apply str (take padding (repeat "\n"))))
  (doall (map println (row-output board)))))

(defn alive-in-next-gen? [board row_idx col_idx]
  (let [alive? (get-in board [row_idx col_idx])
        live-neighbour-count (->>
                              relative-neighbour-positions
                              (map
                                (fn [[relrow relcol]]
                                  (get-in board [(+ relrow row_idx) (+ relcol col_idx)])))
                              (filter identity)
                              (count))]
    (or
      (and alive? (some #{live-neighbour-count} [2 3]))
      (and (not alive?) (= live-neighbour-count 3)))))

(defn tick [board]
  (let [updated-board (deep-vectorize
                        (map-indexed
                          (fn [row_idx row]
                            (map-indexed
                              (fn [col_idx _]
                                (alive-in-next-gen? board row_idx col_idx))
                              row))
                          board))]
    (print-board updated-board)
    (Thread/sleep sleep)
    (tick updated-board)))

(tick (seed))
