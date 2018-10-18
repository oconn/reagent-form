(ns reagent-form.transformers
  (:require [clojure.string]

            [reagent-form.utils :as u]))

(defn ensure-lower
  "Forces lower case"
  [val]
  (if (string? val)
    (clojure.string/lower-case val)
    (u/reagent-form-error "Only strings can be forced to lower")))

(defn trim
  "Trims whitespace"
  [val]
  (if (string? val)
    (clojure.string/trim val)
    (u/reagent-form-error "Only strings can be trimmed.")))

(defn alpha-only
  "Only allow non digit values"
  [value]
  (clojure.string/replace value #"\d" ""))

(defn digit-only
  "Only allow digits"
  [val]
  (let [update-exp #"\D"]
    (if (string? val)
      (clojure.string/replace val update-exp "")
      (-> val
          (str)
          (clojure.string/replace update-exp "")
          (js/parseInt)))))

(defn float-only
  "Only allow digits and decimal point"
  [val]
  (let [update-exp #"[^0-9\.]"]
    (if (string? val)
      (clojure.string/replace val update-exp "")
      (-> val
          (str)
          (clojure.string/replace update-exp "")
          (js/parseFloat)))))

(defn no-whitespace
  "Disallow whitespace"
  [value]
  (clojure.string/replace value #"\s" ""))

(defn str->int
  "Converts a string to an integer"
  [value]
  (if (empty? value)
    nil
    (js/parseInt value)))
