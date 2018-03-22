(ns reagent-form.components.input
  (:require [reagent-form.utils :refer [invoke-or-return
                                        initialize-field!
                                        update-field-value!
                                        validate-field!
                                        get-field-value
                                        get-field-errors]]))

(defn- get-value
  "Returns an inputs value"
  [target]
  (let [type (.-type target)]
    (case type
    "radio" (.-id target)
    "checkbox" (.-checked target)
    (.-value target))))

(defn mount-input
  [{:keys [node is-submitting form-state]}]
  (let [!ref
        (atom nil)

        params
        (second node)

        {:keys [default-errors
                default-hints
                default-value
                field-key
                hint-triggers
                masks
                on-blur
                on-change
                placeholder
                transformers
                validators]
         :as rf-params}
        (:rf/input params)

        update-field-value-fn
        #(do
           (update-field-value! form-state field-key %)

           ;; Once the form has been submitted, live (on-change) validation will
           ;; take over for inputs until the error is cleared, then it will
           ;; return to default behavior (only validate on-blur)
           (when-not (empty? (get-field-errors @form-state field-key))
             (validate-field! form-state field-key)))

        mounted-node
        (assoc-in node [1]
                  (-> params
                      (dissoc :rf/input)
                      (merge {:ref
                              #(reset! !ref %)

                              :on-blur
                              (fn [event]
                                (update-field-value-fn
                                 (-> event .-target get-value))
                                (validate-field! form-state field-key)
                                ((or on-blur identity) event))

                              :on-change
                              (fn [event]
                                (update-field-value-fn
                                 (-> event .-target get-value))
                                ((or on-change identity) event))})))]

    (when-not field-key
      (throw (js/Error. (str "Missing field-key for " node))))

    (initialize-field! form-state
                       field-key
                       {:default-errors (or default-errors [])
                        :default-hints (or default-hints [])
                        :default-value default-value
                        :hint-triggers (or hint-triggers [])
                        :masks (or masks [])
                        :transformers (or transformers [])
                        :validators (or validators [])})

    (fn []
      (update-in mounted-node [1]
                 assoc
                 :value
                 (get-field-value  @form-state field-key)))))
