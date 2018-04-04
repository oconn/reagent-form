(ns reagent-form.components.field-error
  (:require [reagent-form.utils :refer [get-field-errors
                                        field-hidden?]]))

(defn mount-field-error
  [{:keys [node form-state]}]
  (let [params
        (second node)

        {:keys [field-key]}
        (:rf/field-error params)

        mounted-node
        (assoc-in node [1]
                  (-> params
                      (dissoc :rf/field-error)))]
    (fn []
      (let [hidden (field-hidden? @form-state field-key)
            error (first (get-field-errors @form-state field-key))]
        (when (and error
                   (not hidden))
          [assoc-in mounted-node [2] error])))))
