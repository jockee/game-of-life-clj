(ns game-of-life)

(def relative-neighbour-positions [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])
(def sleep 300)
(def board-cols 100)
(def board-rows 30)
(def live-percentage 0.1)

(defn alive-in-next-gen? [board row col]
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
  (for [x (range 0 30) y (range 0 100)] (list x y)))

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
                         (cells))]
    (do
      (print "\n\n\n\n\n\n\n")
      (doall
        (map println (row-output updated-board)))
      (Thread/sleep sleep)
      (tick updated-board))))

(defn game-of-life []
  (let [seed (into [] (take board-rows
                            (repeatedly
                              (fn [] (into [] (take board-cols (repeatedly #(< (rand 1) 0.1))))))))]
    (tick seed)))

(game-of-life)
