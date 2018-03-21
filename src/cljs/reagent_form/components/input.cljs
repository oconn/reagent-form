(ns reagent-form.components.input
  (:require [reagent-form.utils :refer [invoke-or-return
                                        initialize-field!
                                        update-field-value!
                                        validate-field!
                                        get-field-value]]))

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

        {:keys [default-value
                field-key
                masks
                on-blur
                on-change
                placeholder
                transformers
                validators]
         :as rf-params}
        (:rf/input params)

        update-field-value-fn
        #(update-field-value! form-state field-key %)

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
                       {:default-value default-value
                        :masks (or masks [])
                        :transformers (or transformers [])
                        :validators (or validators [])})

    (fn []
      (update-in mounted-node [1]
                 assoc
                 :value
                 (get-field-value  @form-state field-key)))))
