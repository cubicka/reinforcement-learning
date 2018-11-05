(ns tictactoe.game
  (:require [rn.core :as rn]
            [rn.model :refer [make-model]]
            [clojure.pprint :refer [pprint]]))

(def initial-board (->> (repeat 3 nil)
                        (into [])
                        (repeat 3)
                        (into [])))

(def roles [:x :o])

(def initial-state
  {:board initial-board
   :next-turn :x})

(defn apply-step [{:keys [board next-turn] :as state} [{:keys [step]}]]
  (when (nil? (get-in board step))
    {:board (assoc-in board step next-turn)
     :next-turn (if (= next-turn (first roles))
                  (second roles)
                  (first roles))}))

(def all-step (for [i (range 3) k (range 3)] [i k]))

(defn possible-steps [{:keys [board next-turn]}]
  (->> all-step
       (filter #(-> board (get-in %) nil?))
       (map #(assoc {} :role next-turn :step %))))

(defn get-winner [state]
  "Given a state, return the winner.
  Return nil if there is no winner"
  (letfn [(diags [s] [(map #(get-in s [% %]) [0 1 2])
                      (map #(get-in s [% (- 2 %)]) [0 1 2])])
          (cols [s] (apply map vector s))
          (rows [s] s)
          (find-winner [r] (cond (apply = :o r) :o
                                 (apply = :x r) :x))]
    (some identity (map find-winner (apply concat ((juxt diags cols rows) state))))))

(defn is-game-over? [state]
  (let [winner (get-winner (:board state))
        moves (possible-steps state)]
    (or (not (nil? winner)) (zero? (count moves)))))

(def game
  {:initial-state initial-state
   :possible-steps possible-steps
   :apply-step apply-step
   :is-game-over? is-game-over?
   :rewards rewards
   :roles roles})

(def players {:x {:model (make-model {})}
              :o {:model (make-model {})}})

(defn rewards [state]
  (let [winner (get-winner (:board state))]
    (if (nil? winner)
      (constantly 0.5)
      #(if (= winner %) 1 0))))

(comment
  (->> (possible-steps initial-state)
       (rn/pick-step-for-players players initial-state)
       (apply-step initial-state)
       )
  (is-game-end? initial-state)
  (clojure.pprint/pprint (rn/game-turn possible-steps apply-step players initial-state))
  (def game-result (rn/play game players))
  (pprint game-result)
  (pprint (rn/find-relevant-log (:logs game-result) :x))
  (def learn-1 (rn/learn game players (:final-state game-result) (:logs game-result)))
  (pprint learn-1)
  (clojure.pprint/pprint (rewards (rn/play config players)))
  (def train-1 (rn/train 1000000 game players))
  (with-open [w (clojure.java.io/writer "data/train.edn")]
    (binding [*out* w
              *print-length* false]
      (pprint train-1)))
  (pprint (rn/match 10000 game {:x (:x train-1) :o (:o train-1)}))
  (pprint (rn/match 10000 game {:x (:x train-1) :o (:o players)}))
  (pprint (rn/match 10 game players))
  (rn/get-results players (:rewards game) (:final-state game-result))
  (def match-result (rn/match 10 game players))
  (apply merge-with + match-result)

  (def persistent-players (rn/train-player "muse" game players))
  (persistent-players :players)
  (persistent-players :train 1000000)
  (persistent-players :save)

  (def x (atom {:x {:model 3}}))
  (:x @x)
  )
