(ns reagent-form.components.form
  (:require [reagent-form.utils :refer [get-form-data
                                        initialize-field!
                                        validate-form!]]))

(defn- on-form-submit
  [form-state is-submitting transformers]
  )

(defn mount-form
  [{:keys [form-state node is-submitting]}]
  (let [params
        (second node)

        {:keys [on-submit
                transformers
                validators]
         :or {on-submit identity
              transformers []
              validators []}}
        (:rf/form params)

        mounted-node
        (assoc-in node [1]
                  (-> params
                      (dissoc :rf/form)
                      (assoc :on-submit
                             (fn [event]
                               (.preventDefault event)

                               (let [form-is-valid
                                     (validate-form! form-state)

                                     form-is-not-submitting
                                     (not (if is-submitting
                                            @is-submitting
                                            false))]

                                 (when (and form-is-valid
                                            form-is-not-submitting)

                                   (on-submit (get-form-data @form-state
                                                             transformers))))))))]

    (when validators
      (initialize-field! form-state :reagent-form {:validators validators}))

    (fn []
      mounted-node)))
