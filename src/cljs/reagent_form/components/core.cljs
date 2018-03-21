(ns reagent-form.components.core
  (:require [clojure.walk :refer [postwalk]]
            [reagent.core :as reagent]
            [re-frame.core :as re-frame]

            #_[reagent-form.components.button :as button]
            [reagent-form.components.form :as form]
            [reagent-form.components.input :as input]
            [reagent-form.components.field-error :as field-error]
            [reagent-form.components.utils :refer [add-class]]
            #_[reagent-form.components.select :as select]))

(defn- rff-node?
  [node key]
  (and (coll? node)
       (contains? (second node) key)))

(defn- walk-node
  [{:keys [id is-submitting]} form-state]

  (when (and (not (nil? is-submitting))
             (not= (type is-submitting)
                   reagent.ratom/Reaction))
    (throw
     (js/Error. "When passing \"is-submitting\" into re-frame-from, you must pass a reagent ratom")))

  (fn [node]
    (cond
      (rff-node? node :rff/form)
      [form/mount-form {:node node
                        :form-state form-state
                        :is-submitting is-submitting}]

      (rff-node? node :rff/input)
      [input/mount-input {:node node
                          :form-state form-state
                          :is-submitting is-submitting}]

      (rff-node? node :rff/field-error)
      [field-error/mount-field-error {:node node
                                      :form-state form-state}]

      #_(rff-node? node :rff/select)
      #_[select/mount-select node id is-submitting]

      #_(rff-node? node :rff/submit-button)
      #_[button/mount-submit-button node id is-submitting]

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

   [:input (cond-> {:rff/input {:field-key field-key
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

   [:p {:rff/field-error {:field-key field-key}
        :class (add-class "reagent-form-error" error-class)}]])

(defn select
  [{:keys [key
           label
           validators
           default-value
           options
           on-change
           class-name]
    :or {options [{:value ""
                   :display "Select an option"
                   :disabled true}]
         on-change identity}}]
  [:div {:class (str "rff-input-wrapper "
                     (when class-name class-name))}
   [:label.rff-input-label {:for key} label]
   [:select.rff-select {:rff/select {:key key
                                     :validators validators
                                     :options options
                                     :on-change on-change
                                     :default-value (or default-value
                                                        (:value (first options)))}
                        :id key}]
   [:p.rff-field-error {:rff/field-error {:key key}}]])
