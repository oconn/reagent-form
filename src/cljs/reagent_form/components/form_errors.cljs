(ns reagent-form.components.form-errors
  (:require [reagent-form.utils :refer [add-class
                                        get-form-errors]]))

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
      (when-not (empty? (get-form-errors @form-state))
        [assoc-in mounted-node [2] error-message]))))
