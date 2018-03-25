(ns reagent-form.validators)

(defn required [v]
  (cond
    (keyword? v) true

    :else (not (empty? v))))

(defn required-select [options]
  (->> options
       (filter #(= (and (:selected %)
                        (not (:disabled %))) true))
       first
       :value
       empty?
       not))

(defn simple-email
  [v]
  (re-matches #"^[a-zA-Z0-9._+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$" v))
