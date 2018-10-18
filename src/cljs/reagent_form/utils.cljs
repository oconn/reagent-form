(ns reagent-form.utils)

;; Utils

(defn reagent-form-error
  [error-message]
  (throw (js/Error. (str "Reagent Form: " error-message))))

(defn add-class
  "Appends a class to an existing class if it exists"
  [class-name custom-class]
  (str class-name
       (when custom-class
         (str " " custom-class))))

(defn invoke-or-return
  "If the provided value is a function, return the invoked value of
  the function, otherwise return the value"
  [value]
  (if (fn? value) (value) value))

(defn format-field-state
  "Takes field state and returns a formated version"
  [{:keys [default-errors
           default-hints
           default-value
           default-visibility
           hint-triggers
           masks
           transformers
           validators]
    :or {default-errors []
         default-hints []
         default-visibility :visible
         hint-triggers []
         masks []
         transformers []
         validators []}}]
  {:data (invoke-or-return default-value)
   :errors (invoke-or-return default-errors)
   :hints (invoke-or-return default-hints)
   :hint-triggers (invoke-or-return hint-triggers)
   :masks (invoke-or-return masks)
   :transformers (invoke-or-return transformers)
   :validators (invoke-or-return validators)
   :visibility (invoke-or-return default-visibility)
   :reset-with {:default-errors default-errors
                :default-hints default-hints
                :default-value default-value
                :hint-triggers hint-triggers
                :masks masks
                :transformers transformers
                :validators validators
                :visibility default-visibility}})

(defn ensure-field-key-or-throw
  "Ensures the existance and validity of a field's field-key"
  [field-key node]
  (let [reserved-field-keys #{:reagent-form}]
    (cond
      (nil? field-key)
      (throw (js/Error. (str "Missing field-key for " node)))

      (contains? reserved-field-keys field-key)
      (throw (js/Error.
              (str "Reserved field-key '" field-key "' for " node)))

      :else
      nil)))

;; Getters

(defn get-field-value
  "Returns the value of a field"
  [form-state field-key]
  (get-in form-state [field-key :data]))

(defn get-field-errors
  "Returns the field's errors"
  [form-state field-key]
  (get-in form-state [field-key :errors]))

(defn get-field-hints
  "Returns the field's hints"
  [form-state field-key]
  (get-in form-state [field-key :hints]))

(defn get-form-data
  "Returns form data in fully transformed format"
  ([form-state]
   (get-form-data form-state []))
  ([form-state form-level-transformers]
   (let [transformed-field-data
         (reduce
          (fn [form-data [field-key {:keys [transformers data visibility]}]]
            (if (= visibility :hidden)
              form-data
              (let [field-value ((apply comp (reverse transformers)) data)]
                (assoc form-data field-key field-value))))
          {}
          (dissoc form-state :reagent-form))]
     ((apply comp form-level-transformers) transformed-field-data))))

(defn get-form-errors
  "Returns a list of existing form errors. NOTE: This will not check
  to see if there are errors with the existing data. To get errors
  for existing data, see `calculate-form-errors`"
  [form-state]
  (reduce
   (fn [form-errors [field-key {:keys [errors visibility]}]]
     (if (= visibility :hidden)
       form-errors
       (concat form-errors (map (fn [message] {:field-key field-key
                                              :message message})
                                errors))))
   []
   (dissoc form-state :reagent-form)))

(defn calculate-form-errors
  "Returns form errors as tuples consisting of [field-key error-message]"
  [form-state]
  (map
   (fn [[field-key {:keys [validators data visibility]}]]
     (let [errors
           (if (= visibility :hidden)
             []
             (reduce (fn [errors {:keys [validator message]
                                 :or {message "Required"}}]
                       ;; Validators resolve to true if valid, false if not
                       (if (validator (if (= field-key :reagent-form)
                                        form-state
                                        data))
                         errors
                         (conj errors {:field-key field-key
                                       :message message})))
                     []
                     validators))]
       [field-key errors]))
   form-state))

(defn field-hidden?
  "Returns the hidden state of a field"
  [form-state field-key]
  (= :hidden (get-in form-state [field-key :visibility] :visible)))

;; Setters

(defn initialize-field!
  "Initialized a field data structure"
  [form-state field-key field-state]
  (swap! form-state
         #(assoc % field-key (format-field-state field-state))))

(defn update-field-value!
  "Updates a form with field level changes"
  [form-state field-key value]
  (let [{:keys [masks hint-triggers]} (field-key @form-state)
        updated-state (assoc-in @form-state
                                [field-key :data]
                                ((apply comp masks) value))]
    (swap! form-state
           #(cond-> updated-state
              (not (empty? hint-triggers))
              (assoc-in [field-key :hints]
                        (reduce (fn [hints {:keys [trigger message]}]
                                  (if (trigger (get-field-value updated-state
                                                                field-key))
                                    (conj hints message)
                                    hints))
                                []
                                hint-triggers))))))

(defn validate-field!
  "Checks to see if a field is valid and updates errors if they exist"
  [form-state field-key]
  (let [{:keys [validators data]} (field-key @form-state)]
    (let [errors (reduce (fn [errors {:keys [validator message]
                                     :or {message "Required"}}]
                           (if (validator data)
                             errors
                             (conj errors message)))
                         []
                         validators)]
      (swap! form-state assoc-in [field-key :errors] errors))))

(defn update-form-errors!
  "Applies errors to form state"
  [form-state form-errors]
  (swap! form-state #(reduce (fn [state [field-key errors]]
                               (assoc-in state
                                         [field-key :errors]
                                         (->> errors
                                              (map :message)
                                              (vec))))
                             %
                             form-errors)))

(defn validate-form!
  "Updates form state with any generated errors. Returns true if the
  form is free of errors, false if it contains an error"
  [form-state]
  (let [errors (calculate-form-errors @form-state)]
    (update-form-errors! form-state errors)

    (empty? (->> errors
                 (map last)
                 (flatten)
                 (vec)))))

(defn reset-form!
  "Resets a form back to a default state"
  [form-state]
  (->> @form-state
       (reduce (fn [state [field-key field-state]]
                 (assoc state
                        field-key
                        (format-field-state
                         (:reset-with field-state))))
               {})
       (reset! form-state)))

(defn show-field!
  "Marks a field as visible"
  [form-state field-key]
  (when (field-hidden? @form-state field-key)
    (swap! form-state #(assoc-in % [field-key :visibility] :visible))))

(defn hide-field!
  "Marks a field as hidden"
  [form-state field-key]
  (when-not (field-hidden? @form-state field-key)
    (swap! form-state #(assoc-in % [field-key :visibility] :hidden))))
