(ns rn.core
  (:require [rn.model :refer [pick-entry update-model]]
            [util.core :refer [deep-merge]]))

(defn state-step->model-entry [state step]
  {:state state :step step})

(defn pick-step-for-players [players state options]
  (->> options (group-by :role)
       (map (fn [[role steps]]
              (pick-entry (-> role players :model)
                          state
                          steps)))))

(defn game-turn [possible-steps apply-step players state]
  (let [picked-steps (pick-step-for-players players state (possible-steps state))
        next-state (apply-step state picked-steps)]
    {:next-state next-state :log-entry {:state state :step picked-steps}}))

(defn play [game players]
  (let [{is-game-over? :is-game-over?
         possible-steps :possible-steps
         apply-step :apply-step
         initial-state :initial-state} game
        a-turn (partial game-turn possible-steps apply-step players)]
    (loop [state initial-state logs []]
      (if (is-game-over? state)
        {:final-state state :logs logs}
        (let [{next-state :next-state log-entry :log-entry} (a-turn state)]
          (recur next-state (conj logs log-entry)))))))

(defn find-relevant-step [steps role]
  (->> steps (drop-while #(not= role (:role %)))
       first))

(defn find-relevant-log [logs role]
  (->> logs
       (map #(assoc % :step
                    (find-relevant-step (:step %) role)))
       (remove #(nil? (:step %)))))

(defn learn-result [players rewards-fn logs]
  (for [[role {model :model}] players
        :let [log-entry (find-relevant-log logs role)]]
    {:role role :model (update-model model (rewards-fn role) log-entry)}))

(defn learn [game players final-state logs]
  (let [{rewards :rewards} game
        rewards-fn (rewards final-state)]
    (learn-result players rewards-fn logs)))

(defn train [n game players]
  (loop [n n players players]
    (if (zero? n)
      players
      (as-> (play game players) item
        (learn game players (:final-state item) (:logs item))
        (apply deep-merge (map #(hash-map (:role %) {:model (:model %)}) item))
        (recur (dec n) item)))))

(defn get-results [players rewards final-state]
  (let [rewards-fn (rewards final-state)]
    (apply merge {} (map #(hash-map (first %) (rewards-fn (first %))) players))))

(defn into-match-mode [model]
  (assoc model :explore-rate 0))

(defn match [n game players]
  (let [{rewards :rewards
         roles :roles} game
        results (into {} (map #(vector % 0) roles))]
    (apply merge-with + (for [i (range n)]
                          (as-> (play game players) accum
                            (get-results players rewards (:final-state accum)))))))

(defn persistent-train [n game players]
  (->> (train n game @players)
       (reset! players)))

(defn train-player [name game initial-players]
  (let [players (atom initial-players)]
    (fn [action & args]
      (case action
        :train (persistent-train (first args) game players)
        :players @players
        :save (with-open [w (clojure.java.io/writer (str "data/" name ".edn"))]
                (binding [*out* w
                          *print-length* false]
                  (clojure.pprint/pprint @players)))))))
