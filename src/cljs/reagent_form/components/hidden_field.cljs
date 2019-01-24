(ns reagent-form.components.hidden-field
  (:require [reagent-form.utils :refer [show-field!
                                        hide-field!
                                        ensure-field-key-or-throw]]))

(defn mount-hidden-field
  "Hides a field if hide-on returns true"
  [{:keys [node form-state]}]
  (let [{:keys [hide-on field-key]
         :or {hide-on (constantly false)}}
        (get-in node [1 :rf/hidden-field])

        mounted-node
        (update-in node [1] dissoc :rf/hidden-field)]

    (ensure-field-key-or-throw field-key node)

    (fn []
      (let [should-hide (hide-on @form-state)
            hidden-node (assoc-in mounted-node [1 :style :display] :none)]

        (if should-hide
          (hide-field! form-state field-key)
          (show-field! form-state field-key))

        (if should-hide hidden-node mounted-node)))))
