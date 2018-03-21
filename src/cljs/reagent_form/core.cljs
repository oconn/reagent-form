(ns reagent-form.core
  (:require [clojure.walk :refer [postwalk]]
            [reagent.core :as reagent]
            [reagent-form.components.form :as form]
            [reagent-form.components.input :as input]
            [reagent-form.components.field-error :as field-error]

            [reagent-form.utils :refer [add-class]]))

(defn- rf-node?
  [node key]
  (and (coll? node)
       (contains? (second node) key)))

(defn- walk-node
  [{:keys [id is-submitting]} form-state]

  (when (and (not (nil? is-submitting))
             (not= (type is-submitting)
                   reagent.ratom/Reaction))
    (throw
     (js/Error. (str "When passing \"is-submitting\" into re-frame-from, "
                     "you must pass a reagent ratom"))))

  (fn [node]
    (cond
      (rf-node? node :rf/form)
      [form/mount-form {:node node
                        :form-state form-state
                        :is-submitting is-submitting}]

      (rf-node? node :rf/input)
      [input/mount-input {:node node
                          :form-state form-state
                          :is-submitting is-submitting}]

      (rf-node? node :rf/field-error)
      [field-error/mount-field-error {:node node
                                      :form-state form-state}]

      :else
      node)))

(defn form
  [form-data html]
  (let [form-state (reagent/atom {})]
    [postwalk (walk-node form-data form-state) html]))

(defn input
  [{:keys [default-value
           error-class
           field-key
           form-field-class
           input-class
           type
           label
           label-class
           masks
           on-blur
           on-change
           placeholder
           transformers
           validators]
    :or {default-value ""
         type :text}}]
  [:div {:class (add-class "reagent-form-field" form-field-class)}
   (when label
     [:label {:for field-key
              :class (add-class "reagent-form-label" label-class)}
      label])

   [:input (cond-> {:rf/input {:field-key field-key
                               :validators validators
                               :transformers transformers
                               :on-change on-change
                               :masks masks
                               :default-value default-value
                               :placeholder placeholder}
                    :id field-key
                    :type type
                    :class (add-class "reagent-form-input" input-class)}
             placeholder (assoc :placeholder placeholder))]

   [:p {:rf/field-error {:field-key field-key}
        :class (add-class "reagent-form-error" error-class)}]])
