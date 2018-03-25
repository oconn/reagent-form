(ns reagent-form.components.field-hint
  (:require [reagent-form.utils :refer [get-field-hints
                                        get-field-errors]]))

(defn mount-field-hint
  [{:keys [node form-state]}]
  (let [params
        (second node)

        {:keys [field-key]}
        (:rf/field-hint params)

        mounted-node
        (assoc-in node [1]
                  (-> params
                      (dissoc :rf/field-hint)))]
    (fn []
      (let [hint (first (get-field-hints @form-state field-key))
            error (first (get-field-errors @form-state field-key))]
        (when (and hint (not error))
          [assoc-in mounted-node [2] hint])))))
