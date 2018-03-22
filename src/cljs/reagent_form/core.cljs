(ns reagent-form.core
  (:require [clojure.walk :refer [postwalk]]
            [reagent.core :as reagent]
            [reagent-form.components.form :as form]
            [reagent-form.components.input :as input]
            [reagent-form.components.field-error :as field-error]
            [reagent-form.components.form-errors :as form-errors]
            [reagent-form.components.submit-button :as submit-button]

            [reagent-form.utils :refer [add-class
                                        reset-form!]]))

(defn- rf-node?
  "Checks to see if any given node is an rf field"
  [node key]
  (and (coll? node)
       (contains? (second node) key)))

(defn- render-custom-field
  "Extension point for users to provider their own custom components "
  [{:keys [node] :as params}]
  (let [{:keys [render]}
        (get-in node [1 :rf/custom-field])]
    [render params]))

(defn- walk-node
  "Walks the form replacing rf fields with form aware components"
  [{:keys [id is-submitting custom-fields]}
   form-state]

  (when (and (not (nil? is-submitting))
             (not (contains? #{reagent.ratom/Reaction
                               reagent.ratom/RAtom}
                             (type is-submitting))))
    (throw
     (js/Error. (str "When passing \"is-submitting\" into reagent-from, "
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

      (rf-node? node :rf/form-errors)
      [form-errors/mount-form-errors {:node node
                                      :form-state form-state}]

      (rf-node? node :rf/submit-button)
      [submit-button/mount-submit-button {:node node
                                          :form-state form-state
                                          :is-submitting is-submitting}]

      (rf-node? node :rf/custom-field)
      (render-custom-field {:node node
                            :form-state form-state
                            :is-submitting is-submitting})

      :else
      node)))

(defn form
  [{:keys [on-initialized
           on-form-close]
    :or {on-initialized identity
         on-form-close identity}
    :as form-data}
   html]
  (let [form-state (reagent/atom {})]
    (reagent/create-class
     {:component-did-mount
      (fn [_]
        (on-initialized {:reset-form #(reset-form! form-state)}))
      :component-will-unmount
      #(on-form-close)
      :reagent-render
      (fn [form-data html]
        [postwalk (walk-node form-data form-state) html])})))

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
