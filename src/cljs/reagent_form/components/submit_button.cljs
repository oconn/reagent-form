(ns reagent-form.components.submit-button
  (:require [reagent-form.utils :refer [get-form-errors]]))

(defn mount-submit-button
  [{:keys [node form-state is-submitting]}]
  (let [params
        (second node)

        {:keys [submission-text
                submitting-element
                is-disabled?]}
        (:rf/submit-button params)

        mounted-node
        (assoc-in node [1] (dissoc params :rf/submit-button))]
    (fn []
      (let [form-errors (get-form-errors @form-state)
            submitting @is-submitting
            submit-button (cond
                            (not (empty? form-errors))
                            (assoc-in mounted-node [1 :disabled] true)

                            (and (some? is-disabled?)
                                 (is-disabled?))
                            (assoc-in mounted-node [1 :disabled] true)

                            submitting
                            (cond-> (-> mounted-node
                                        (update-in [1 :class]
                                                   #(str % " submitting"))
                                        (assoc-in [1 :disabled]
                                                  true))
                              submission-text
                              (assoc-in [1 :value] submission-text))

                            :else
                            mounted-node)]

        (if submitting
          [:span.reagent-form-submitting-tag-container
           submit-button
           (or submitting-element [:span.reagent-form-submitting-tag])]
          [:span.reagent-form-submitting-tag-container submit-button])))))
