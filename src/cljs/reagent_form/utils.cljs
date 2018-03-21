(ns reagent-form.utils)

(defn get-form-data
  "Returns form data in fully transformed format"
  [form-state]
  (reduce
   (fn [form-data [field-key {:keys [transformers data]}]]
     (let [field-value ((apply comp transformers) data)]
       (assoc form-data field-key field-value)))
   {}
   form-state))

(defn get-form-errors
  "Returns a list of existing form errors. NOTE: This will not check
  to see if there are errors with the existing data. To get errors
  for existing data, see `calculate-form-errors`"
  [form-state]
  (reduce
   (fn [form-errors [field-key {:keys [errors]}]]
     (concat form-errors (map (fn [message] {:field-key field-key
                                            :message message})
                              errors)))
   []
   form-state))

(defn calculate-form-errors
  "Returns form errors as tuples consisting of [field-key error-message]"
  [form-state]
  (map
   (fn [[field-key {:keys [validators data]}]]
     (let [errors
           (reduce (fn [errors {:keys [validator message]
                               :or {message "Required"}}]
                     ;; Validators resolve to true if valid, false if not
                     (if (validator data)
                       errors
                       (conj errors {:field-key field-key
                                     :message message})))
                   []
                   validators)]
       [field-key errors]))
   form-state))

(defn invoke-or-return
  "If the provided value is a function, return the invoked value of
  the function, otherwise return the value"
  [value]
  (if (fn? value) (value) value))

(defn format-field-state
  "Takes field state and returns a formated version"
  [{:keys [default-errors
           default-value
           masks
           transformers
           validators]
    :or {default-errors []
         masks []
         transformers []
         validators []}}]
  {:data (invoke-or-return default-value)
   :errors (invoke-or-return default-errors)
   :masks (invoke-or-return masks)
   :validators (invoke-or-return validators)
   :transformers (invoke-or-return transformers)
   :reset-with {:default-errors default-errors
                :default-value default-value
                :masks masks
                :transformers transformers
                :validators validators}})

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

(defn initialize-field!
  "Initialized a field data structure"
  [form-state
   field-key
   field-state]
  (swap! form-state
         #(assoc % field-key (format-field-state field-state))))

(defn update-field-value!
  "Updates a form with field level changes"
  [form-state field-key value]
  (let [{:keys [masks]} (field-key @form-state)]
    (swap! form-state
           #(assoc-in %
                      [field-key :data]
                      ((apply comp masks) value)))))

(defn validate-field!
  "Checks to see if a field is valid an updates errors if they exist"
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

(defn get-field-value
  "Returns the value of a field"
  [form-state field-key]
  (get-in form-state [field-key :data]))

(defn get-field-errors
  "Returns the field's errors"
  [form-state field-key]
  (get-in form-state [field-key :errors]))

(defn add-class
  [class-name custom-class]
  (str class-name
       (when custom-class
         (str " " custom-class))))
