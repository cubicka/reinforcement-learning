(ns game.tictac
  (:require [clojure.set :refer [difference]]
            [clojure.core.matrix.random :refer [sample-binomial sample-rand-int]]
            [clojure.pprint :refer [pprint]]
            [rn.core :as rn]))

(def init-state [[nil nil nil] [nil nil nil] [nil nil nil]])
(def example-state [[nil nil nil] [nil :x :o] [nil :o :x]])

(def STATE (atom init-state))

(defn init-state! [] (reset! STATE init-state))

(defn end? [s] (not (some nil? (flatten s))))

(defn show
  ([] (show @STATE))
  ([s]
   (apply printf (str "-------\n"
                      "|%s|%s|%s|\n"
                      "|%s|%s|%s|\n"
                      "|%s|%s|%s|\n"
                      "-------\n")
          (map {nil " " :x "x" :o "o"} (flatten s)))
   (flush)))

(comment (show init-state))

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

(def tic-tac-toe
  {:get-winner get-winner
   :possible-states possible-states})

(defn possible-states
  "Given a state, return a map with all the possible states mapped to the corresponding winners.
  Notice that if role is not provided, an action can be taken by both :x and :o.
  And obviously one player can never take two more actions than another"
  ([state role]
   (letfn [(set-role [[i j]]
             (when (nil? (get-in state [i j]))
               (assoc-in state [i j] role)))]
     (when-not (get-winner state)
       (let [all-idx (for [i [0 1 2] j [0 1 2]] [i j])
             next-states (remove nil? (map set-role all-idx))
             winners (map get-winner next-states)]
         (zipmap next-states winners)))))
  ([state]
   (let [possible-players
         (let [freq (frequencies (flatten state))
               [count-x count-o] (map #(get freq % 0) [:x :o])
               count-diff (- count-x count-o)]
           (cond (= count-diff 0) [:x :o]
                 (= count-diff 1) [:o]
                 (= count-diff -1) [:x]))]
     (apply merge (map #(possible-states state %) possible-players)))))

(defn set-role [state [i j] role]
  (when (nil? (get-in state [i j]))
    (assoc-in state [i j] role)))

(defn possible-states
  [state]
  (when-not (get-winner state)
    (let [all-idx (for [i [0 1 2] j [0 1 2]] [i j])
          next-states (remove nil? (map #(set-role state % :x) all-idx))
          winners (map get-winner next-states)]
      (zipmap next-states winners))))

(comment (possible-states init-state)
         (possible-states example-state)
         )

;; cache all possible states and corresponding winners
(def all-states
  (let [helper (fn find-rest [init-states seen-states-result]
                 (let [new-states-result (apply merge (map possible-states init-states))
                       new-states (difference (set (keys new-states-result))
                                              (set (keys seen-states-result)))]
                   (if (empty? new-states)
                     seen-states-result
                     (find-rest new-states (merge seen-states-result new-states-result)))))]
    (helper #{init-state} {init-state nil})))

(comment
  (-> all-states keys count)
  (pprint (take 10 all-states))
  )

(defn gen-player
  "Generate a player using transient map.
  step-size: control update ratio
  explore-rate: used in binorminal distribution to determine
                making a decision based on estimations or random choice
  states: observed states"
  [& {:keys [role states]
      :or {states [init-state]}
      :as info}]
  (let [trans #(if (nil? role) 0.5
                   (if (= % role) 1.0 0.0))]
    (transient (-> (rn/gen-player info)
                   (assoc :estimations (into {} (map #(vector (first %) (trans (second %))) all-states)))
                   (assoc :states states)
                   ))))

(defn feed-reward!
  "Update the estimation values of a player based on
  current states and the given reward"
  [{:keys [states step-size estimations] :as player}
   reward]
  (let [get-est (partial get estimations)
        new-est (rest (reductions #(+ (get-est %2)
                                      (* step-size (- %1 (get-est %2))))
                                  reward
                                  states))
        updated-est (merge estimations (zipmap states new-est))]
    (assoc! player :estimations updated-est)))

(defn add-state!
  "add state to current states of a player"
  [{:keys [states] :as player}
   state]
  (let [added-states (cons state states)]
    (assoc! player :states added-states)))

(defn decision
  "Given a state, calculate the player's decision.
  Return nil if there exists no choice."
  [{:keys [role explore-rate estimations] :as player}
   state]
  (let [optional-states (possible-states state role)]
    (when-not (empty? optional-states)
      (let [is-explore? (= 1 (first (sample-binomial 1 explore-rate)))
            shuffled-states (shuffle (keys optional-states))]
        (if is-explore?
          (nth shuffled-states (first (sample-rand-int 1 (count shuffled-states))))
          (if-let [sel-estimations (select-keys estimations shuffled-states)]
            (let [max-val (apply max (vals sel-estimations))]
              (ffirst (shuffle (filter #(= max-val (second %)) (vec sel-estimations)))))))))))

(defn play!
  "Given two player, the first player make decision first.
  If current state already has a winner, then feed them reward,
  else they play iteratively and update their states until
  one win or tie."
  [player1 player2 reward?]
  (let [winner (all-states @STATE)]
    (if (nil? winner)
      (let [p1-decision (decision player1 @STATE)]
        (if p1-decision
          (do (reset! STATE p1-decision)
              (dorun (map #(add-state! % @STATE) [player1 player2]))
              (recur player2 player1 reward?))
          (when reward? (dorun (map #(feed-reward! % 0.5) [player1 player2])))))
      (let [score #(if (true? %) 1 0)]
        (when reward?
          (dorun
           (map
            #(feed-reward! % (score (= (:role %) winner)))
            [player1 player2])))
        winner))))

(defn run-once!
  "This function takes about 1ms each time!"
  [p1 p2 reward?]
  (init-state!)
  (dorun (map #(assoc! % :states [init-state]) [p1 p2]))
  (play! p1 p2 reward?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial
(def p1 (gen-player :role :x))
(def p2 (gen-player :role :o))

(comment
  (get :role p1)
  (def temp-transient (transient p1))
  (def p1-persistent (persistent! p1))
  (pprint (keys p1-persistent))
  (pprint (take 10 (:estimations p1-persistent)))
  )

;; Train:
(defn train!
  "According to our experience, the frequencty that p1 loss
  will be around the explor-rate times n-rounds."
  [n]
  (prn (frequencies (for [i (range n)] (run-once! p1 p2 true)))))

(defn compete!
  [n]
  (dorun (map #(assoc! % :explore-rate 0) [p1 p2]))
  (prn (frequencies (for [i (range n)] (run-once! p1 p2 false)))))

(comment
  (train! 100000)
  )
