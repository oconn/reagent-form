(ns reagent-form.components.form
  (:require [reagent-form.utils :refer [get-form-data
                                        update-form-errors!
                                        validate-form!]]))

(defn mount-form
  [{:keys [form-state node is-submitting]}]
  (let [params
        (second node)

        {:keys [on-submit
                transformers]
         :or {on-submit identity
              transformers []}}
        (:rf/form params)

        mounted-node
        (assoc-in node [1]
                  (-> params
                      (dissoc :rf/form)
                      (assoc :on-submit
                             (fn [event]
                               (.preventDefault event)

                               (when (and (validate-form! form-state)
                                          (not (if is-submitting
                                                 @is-submitting
                                                 false)))

                                 (on-submit (get-form-data @form-state
                                                           transformers)))))))]
    (fn []
      mounted-node)))
