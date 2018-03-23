(ns reagent-form.components.hidden-field
  (:require [reagent-form.utils :refer [show-field!
                                        hide-field!]]))

(defn- mount-hidden-field
  "Hides a field if hide-on returns true"
  [{:keys [node form-state]}]
  (let [{:keys [hide-on field-key]
         :or {hide-on (constantly false)}}
        (get-in node [1 :rf/hidden-field])

        mounted-node
        (update-in node [1] dissoc :rf/hidden-field)]
    (fn []
      (let [hidden (hide-on @form-state)]

        (if hidden
          (hide-field! form-state field-key)
          (show-field! form-state field-key))

        (when-not hidden
          mounted-node)))))
