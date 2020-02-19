(ns reagent-form.core
  (:require [clojure.walk :refer [postwalk]]
            [reagent.core :as reagent]
            [reagent-form.components.form :as form]
            [reagent-form.components.input :as input]
            [reagent-form.components.select-input :as select-input]
            [reagent-form.components.hidden-field :as hidden-field]
            [reagent-form.components.field-error :as field-error]
            [reagent-form.components.field-hint :as field-hint]
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
  [{:keys [is-submitting custom-fields]}
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

      (rf-node? node :rf/select-input)
      [select-input/mount-select-input {:node node
                                        :form-state form-state
                                        :is-submitting is-submitting}]

      (rf-node? node :rf/field-error)
      [field-error/mount-field-error {:node node
                                      :form-state form-state}]

      (rf-node? node :rf/field-hint)
      [field-hint/mount-field-hint {:node node
                                    :form-state form-state}]

      (rf-node? node :rf/form-errors)
      [form-errors/mount-form-errors {:node node
                                      :form-state form-state}]

      (rf-node? node :rf/submit-button)
      [submit-button/mount-submit-button {:node node
                                          :form-state form-state
                                          :is-submitting is-submitting}]

      (rf-node? node :rf/hidden-field)
      [hidden-field/mount-hidden-field {:node node
                                        :form-state form-state}]

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
        (on-initialized {:form-state form-state
                         :reset-form #(reset-form! form-state)}))
      :component-will-unmount
      #(on-form-close)
      :reagent-render
      (fn [form-data html]
        [postwalk (walk-node form-data form-state) html])})))

(defn form-field
  [{:keys [error-class
           field-key
           form-field-class
           hide-on
           hint-class
           label
           label-class]}
   field]
  [:div (cond-> {:class (add-class "reagent-form-field" form-field-class)}
          hide-on (merge {:rf/hidden-field {:hide-on hide-on
                                            :field-key field-key}}))
   (when label
     [:label {:class (add-class "reagent-form-field-label" label-class)
              :for field-key} label])

   [:p {:rf/field-hint {:field-key field-key}
        :class (add-class "reagent-form-field-hint" hint-class)}]

   [:p {:rf/field-error {:field-key field-key}
        :class (add-class "reagent-form-field-error" error-class)}]

   field])

(def rf-input-params
  [:default-errors
   :default-hints
   :default-value
   :field-key
   :hide-on
   :hint-triggers
   :masks
   :on-blur
   :on-change
   :transformers
   :validators
   :validate-on-blur])

(defn input
  [{:keys [field-key] :as params}]
  (form-field
   params
   [:input
    (merge {:rf/input (select-keys params (conj rf-input-params :type))}
           (select-keys params [:checked
                                :class
                                :id
                                :placeholder
                                :style
                                :type
                                :disabled
                                :data-test]))]))

(defn textarea
  [{:keys [field-key auto-focus] :as params :or {auto-focus false}}]
  (form-field
   params
   [:textarea
    (merge {:rf/input (select-keys params rf-input-params)}
           (select-keys params [:class
                                :auto-focus
                                :id
                                :placeholder
                                :style
                                :data-test]))]))

(defn select-input
  [{:keys [field-key] :as params}]
  (form-field
   params
   [:select
    (merge {:rf/select-input (select-keys params (conj rf-input-params
                                                       :options))}
           (select-keys params [:class
                                :id
                                :placeholder
                                :style]))]))

(defn custom-field
  [{:keys [field-key] :as params}]
  (form-field
   params
   [:div {:rf/custom-field params}]))

(defn submit-button
  [{:keys [default-text
           is-disabled?
           form-errors-class
           form-error-message
           submission-text
           submit-button-class
           submit-button-container-class]
    :or {default-text "Submit"
         is-disabled? (constantly false)}}]

  [:div {:class (add-class "reagent-form-submit-button-container"
                           submit-button-container-class)}
   [:p {:rf/form-errors (cond-> {}
                          form-error-message (assoc :error-message
                                                    form-error-message))
        :class (add-class "reagent-form-errors"
                          form-errors-class)}]

   [:input {:rf/submit-button {:submission-text (or submission-text
                                                    default-text)
                               :is-disabled? is-disabled?}
            :class (add-class "reagent-form-submit-button"
                              submit-button-class)
            :type :submit
            :value default-text}]])
