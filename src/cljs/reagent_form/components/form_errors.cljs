(ns reagent-form.components.form-errors
  (:require [reagent-form.utils :refer [add-class
                                        get-form-errors
                                        get-field-errors]]))

(defn mount-form-errors
  [{:keys [node form-state]}]
  (let [params
        (second node)

        {:keys [error-message]
         :or {error-message "Please address the errors above."}}
        (:rf/form-errors params)

        mounted-node
        (assoc-in node [1]
                  (dissoc params :rf/form-errors))]
    (fn []
      (let [reagent-form-error (first (get-field-errors @form-state :reagent-form))
            field-errors (get-form-errors @form-state)
            error (if (not-empty field-errors) error-message reagent-form-error)]

        (when-not (nil? error)
          [assoc-in mounted-node [2] error])))))
