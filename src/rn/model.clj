(ns rn.model
  (:require [util.core :refer [deep-merge pick-random is-happen?]]))

(def default-model
  {:explore-rate 0.1
   :learning-rate 0.1
   :weights {}
   :estimation 0.5})

(defn make-model [{:keys [explore-rate learning-rate weights]
                   :or {explore-rate (:explore-rate default-model)
                        learning-rate (:learning-rate default-model)
                        weights (:weights default-model)}}]
  {:explore-rate explore-rate :weights weights
   :learning-rate learning-rate})

(defn estimate [model state step]
  (if-let [value (get-in model [:weights state step])]
    value
    (:estimation default-model)))

(defn pick-best-estimate [model state steps]
  (let [estimate-fn (partial estimate model state)
        estimations (->> steps (map #(hash-map :step % :estimation (estimate-fn %))))
        best-est (apply max (map :estimation estimations))
        best-entries (filter #(= best-est (:estimation %)) estimations)]
    (pick-random (map :step best-entries))))

(defn should-explore? [model]
  (is-happen? (:explore-rate model)))

(defn pick-entry [model state steps]
  (if (should-explore? model)
    (pick-random steps)
    (pick-best-estimate model state steps)))

(defn next-estimation [model reward state step]
  (let [current-estimation (estimate model state step)]
    (+ current-estimation
       (* (:learning-rate model) (- reward current-estimation)))))

(defn update-model [model reward log-entry]
  (update model :weights
          #(apply deep-merge %
                  (for [{state :state step :step} log-entry]
                    {state {step (next-estimation model reward state step)}}))))
