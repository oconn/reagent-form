(ns reagent-form.components.select-input
  (:require [reagent-form.utils :refer [invoke-or-return
                                        initialize-field!
                                        update-field-value!
                                        validate-field!
                                        get-field-value
                                        get-field-errors]]))

(defn- format-select-value
  [selected-value options]
  (->> options
       (map (fn [{:keys [value] :as option}]
              (if (= selected-value value)
                (assoc option :selected true)
                (assoc option :selected false))))
       (vec)))

(defn- get-select-value
  [options]
  (->> options
       (filter #(= (and (:selected %)
                        (not (:disabled %))) true))
       (first)
       :value))

(defn mount-select-input
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
                on-blur
                on-change
                options
                placeholder
                transformers
                validators
                validate-on-blur]
         :or {validate-on-blur true}
         :as rf-params}
        (:rf/select-input params)

        update-field-value-fn
        #(do
           (update-field-value! form-state
                                field-key
                                (format-select-value
                                 %
                                 (get-field-value @form-state field-key)))

           ;; Once the form has been submitted, live (on-change) validation will
           ;; take over for inputs until the error is cleared, then it will
           ;; return to default behavior (only validate on-blur)
           (when-not (empty? (get-field-errors @form-state field-key))
             (validate-field! form-state field-key)))

        mounted-node
        (assoc-in node [1]
                  (-> params
                      (dissoc :rf/select-input)
                      (merge {:ref
                              #(reset! !ref %)

                              :on-blur
                              (fn [event]
                                (update-field-value-fn
                                 (-> event .-target .-value))

                                (when validate-on-blur
                                  (validate-field! form-state field-key))

                                ((or on-blur identity) event))

                              :on-change
                              (fn [event]
                                (update-field-value-fn
                                 (-> event .-target .-value))

                                ((or on-change identity) event))})))]

    (when-not field-key
      (throw (js/Error. (str "Missing field-key for " node))))

    (initialize-field! form-state
                       field-key
                       {:default-errors (or default-errors [])
                        :default-hints (or default-hints [])
                        :default-value (format-select-value default-value options)
                        :hint-triggers (or hint-triggers [])
                        :masks []
                        :transformers (or (into [get-select-value]
                                                transformers)
                                          [get-select-value])
                        :validators (or validators [])})

    (fn []
      (let [field-value
            (get-field-value @form-state field-key)

            selected-value
            (->> field-value
                 (filter #(true? (:selected %)))
                 (first)
                 (:value))

            updated-node
            [(first mounted-node)
             (cond-> (second mounted-node)
               selected-value
               (assoc :value selected-value)

               @is-submitting
               (assoc :disabled true))
             (for [{:keys [display value selected disabled]} field-value]
               ^{:key value} [:option (cond-> {:value value}
                                        disabled (assoc :disabled true))
                              display])]]

        updated-node))))
