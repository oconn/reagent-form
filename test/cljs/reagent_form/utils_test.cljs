(ns reagent-form.utils-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [cljs.pprint :refer [pprint]]
            [reagent-form.transformers :as t]
            [reagent-form.utils :as u]))

(deftest get-field-value
  (let [form-state (atom {})]
    (u/initialize-field! form-state
                         :username
                         {:default-value "matt"})
    (testing "returns the value for a field"
      (is (= "matt" (u/get-field-value @form-state :username))))
    (testing "returns nil if the field does not exist"
      (is (nil? (u/get-field-value @form-state :no-field))))))

(deftest get-field-errors
  (let [form-state (atom {})]
    (u/initialize-field! form-state
                         :username
                         {:default-value "matt"
                          :default-errors ["error"]})
    (testing "returns the value for a field's errors"
      (is (= ["error"] (u/get-field-errors @form-state :username))))
    (testing "returns nil if the field does not exist"
      (is (nil? (u/get-field-errors @form-state :no-field))))))

(deftest get-field-hints
  (let [form-state (atom {})]
    (u/initialize-field! form-state
                         :username
                         {:default-value "matt"
                          :default-hints ["hint"]})
    (testing "returns the value for a field's hints"
      (is (= ["hint"] (u/get-field-hints @form-state :username))))
    (testing "returns nil if the field does not exist"
      (is (nil? (u/get-field-hints @form-state :no-field))))))

(deftest get-form-data
  (let [form-state (atom {})]
    (testing "Returns form-state's data"
      (u/initialize-field! form-state
                           :username
                           {:default-value "matt"
                            :default-hints ["hint"]})
      (is (= {:username "matt"} (u/get-form-data @form-state))))
    (testing "Returns form-state's data, applying transformers (left to right)"
      (u/initialize-field! form-state
                           :age
                           {:default-value "1"
                            :transformers [#(js/parseInt %)
                                           inc]})
      (is (= {:username "matt" :age 2} (u/get-form-data @form-state))))
    (testing "Does not return hidden field data"
      (u/initialize-field! form-state
                           :password
                           {:default-value "shh..."
                            :default-visibility :hidden})
      (is (= "shh..." (u/get-field-value @form-state :password)))
      (is (= {:username "matt" :age 2} (u/get-form-data @form-state))))
    (testing "Allows for additional transformations to be applied after all field level transformations"
      (is (= {:username "matt" :age 3}
             (u/get-form-data @form-state [#(update % :age inc)]))))))

(deftest get-form-errors
  (let [form-state (atom {})]
    (testing "Returns errors in form-state"
      (u/initialize-field! form-state
                           :username
                           {:default-value "matt"
                            :default-errors ["error"]})
      (is (= '({:field-key :username :message "error"})
             (u/get-form-errors @form-state))))))

(deftest calculate-form-errors
  (let [form-state (atom {})]
    (u/initialize-field! form-state
                         :username
                         {:default-value "matt"
                          :default-errors ["error"]
                          :validators [{:validator #(not= "matty" %)}]})

    (testing "Returns error if they exist in form-state, clearing existing errors if present."
      (is (not (empty? (u/get-form-errors @form-state))))
      (is (-> @form-state
              (u/calculate-form-errors)
              (first)
              (last)
              (empty?)))
      (u/update-field-value! form-state :username "matty")
      (is (-> @form-state
              (u/calculate-form-errors)
              (first)
              (last)
              (empty?)
              (not))))))

(deftest field-hidden?
  (let [form-state (atom {})]
    (u/initialize-field! form-state :username {:default-value "matt"})
    (u/initialize-field! form-state :password {:default-value "shh..."
                                               :default-visibility :hidden})
    (testing "Returns the correct value for visibility"
      (is (= false (u/field-hidden? @form-state :username)))
      (is (= true (u/field-hidden? @form-state :password))))))

(deftest invoke-or-return
  (testing "returns the provided value. If the provided value is a function, then returns the result of invoking that function."
    (is (= (u/invoke-or-return 1) 1))
    (is (= (u/invoke-or-return (constantly 1)) 1))))

(deftest format-form-field
  (testing "creates a map of valid form-field state"
    (let [default-hints (constantly ["Some hints"])]
      (is (= (u/format-field-state {})
             {:data nil
              :errors []
              :hints []
              :hint-triggers []
              :masks []
              :validators []
              :transformers []
              :visibility :visible
              :reset-with {:default-errors []
                           :default-hints []
                           :default-value nil
                           :hint-triggers []
                           :masks []
                           :transformers []
                           :validators []
                           :visibility :visible}}))
      (is (= (u/format-field-state {:default-value 1
                                    :default-errors ["Some errors"]
                                    :default-hints default-hints})
             {:data 1
              :errors ["Some errors"]
              :hints ["Some hints"]
              :hint-triggers []
              :masks []
              :validators []
              :transformers []
              :visibility :visible
              :reset-with {:default-errors ["Some errors"]
                           :default-hints default-hints
                           :default-value 1
                           :hint-triggers []
                           :masks []
                           :transformers []
                           :validators []
                           :visibility :visible}})))))

(deftest ensure-field-key-or-throw
  (testing "Throws an error if field-key is not present"
    (is (= true (try
                  (do (u/ensure-field-key-or-throw nil [:div])
                      false)
                  (catch js/Error e true)))))
  (testing "Does not throw an error if field-key is present"
    (is (= false (try
                   (do (u/ensure-field-key-or-throw :username [:div])
                       false)
                   (catch js/Error e true)))))
  (testing "Throws an error if a reserved field-key name is used"
    (is (= true (try
                  (do (u/ensure-field-key-or-throw :reagent-form [:div])
                      false)
                  (catch js/Error e true))))))

(deftest add-class
  (testing "Appends a classname if present"
    (is (= "class-a class-b" (u/add-class "class-a" "class-b")))
    (is (= "class-a" (u/add-class "class-a" nil)))))

(deftest initialized-field!
  (testing "Adds a field to form-state when properly initialized"
    (is (= (u/initialize-field! (atom {}) :username {})
           {:username {:data nil
                       :errors []
                       :hints []
                       :hint-triggers []
                       :masks []
                       :validators []
                       :transformers []
                       :visibility :visible
                       :reset-with {:default-errors []
                                    :default-hints []
                                    :default-value nil
                                    :hint-triggers []
                                    :masks []
                                    :transformers []
                                    :validators []
                                    :visibility :visible}}}))))

(deftest update-field-value!
  (let [form-state (atom {})]
    (u/initialize-field! form-state
                         :username
                         {:default-value "mat"
                          :masks [t/alpha-only]
                          :hint-triggers
                          [{:trigger #(not= (count %) 4)
                            :message "Username should be 4 chars long"}]})
    (testing "Updates a field with a new value"
      (is (= "matt" (u/get-field-value
                     (u/update-field-value! form-state :username "matt")
                     :username))))
    (testing "Applies masks on update"
      (is (= "matt" (u/get-field-value
                     (u/update-field-value! form-state :username "matt1")
                     :username))))
    (testing "Applies hint when trigger predicate passes"
      (is (empty? (u/get-field-hints @form-state :username)))
      (is (not (empty? (u/get-field-hints
                        (u/update-field-value! form-state :username "matty")
                        :username)))))))

(deftest validate-field!
  (let [form-state (atom {})]
    (u/initialize-field! form-state
                         :username
                         {:default-value "matty"
                          :validators [{:validator #(not= "matty" %)
                                        :message "Nope"}
                                       {:validator (constantly false)
                                        :message "Always wrong"}]})
    (testing "calculates all field-errors when run and contains errors"
      (is (empty? (u/get-field-errors @form-state :username)))
      (u/validate-field! form-state :username)
      (is (= (count (u/get-field-errors @form-state :username)) 2)))))

(deftest update-form-errors!
  (let [form-state (atom {})]
    (u/initialize-field! form-state
                         :username
                         {:default-value "matty"
                          :validators [{:validator #(not= "matty" %)
                                        :message "Nope"}
                                       {:validator (constantly false)
                                        :message "Always wrong"}]})
    (testing "calculates all field-errors when run and contains errors"
      (is (empty? (u/get-field-errors @form-state :username)))
      (u/update-form-errors! form-state (u/calculate-form-errors @form-state))
      (is (= (count (u/get-field-errors @form-state :username)) 2)))))

(deftest validate-form!
  (let [form-state (atom {})]
    (u/initialize-field! form-state
                         :username
                         {:default-value "matty"
                          :validators [{:validator #(not= "matty" %)
                                        :message "Nope"}]})

    (testing "Applies errors to form-state when they exist, also returns the true if from-state is free of errors"
      (is (empty? (u/get-form-errors @form-state)))
      (u/validate-form! form-state)
      (is (not (empty? (u/get-form-errors @form-state))))
      (u/update-field-value! form-state :username "matt")
      (u/validate-form! form-state)
      (is (empty? (u/get-form-errors @form-state))))))

(deftest reset-form!
  (let [form-state (atom {})
        age (atom 1)]
    (u/initialize-field! form-state
                         :username
                         {:default-value "matty"
                          :validators [{:validator #(not= "matty" %)
                                        :message "Nope"}]})
    (u/initialize-field! form-state
                         :age
                         {:default-value (fn [] @age)})

    (testing "Resets form-state back to it's original state"
      (let [initial-state @form-state]
        (u/update-field-value! form-state :username "matt")
        (u/validate-form! form-state)
        (is (empty? (u/get-form-errors @form-state)))
        (is (= initial-state (u/reset-form! form-state)))))
    (testing "Closed over defaults are re-calculated"
      (is (= 1 (u/get-field-value @form-state :age)))
      (swap! age inc)
      (u/reset-form! form-state)
      (is (= 2 (u/get-field-value @form-state :age))))))

(deftest show-field!
  (let [form-state (atom {})]
    (u/initialize-field! form-state
                         :username
                         {:default-value "matt"
                          :default-visibility :hidden})
    (testing "Shows hidden fields"
      (is (true? (u/field-hidden? @form-state :username)))
      (u/show-field! form-state :username)
      (is (false? (u/field-hidden? @form-state :username))))))

(deftest hide-field!
  (let [form-state (atom {})]
    (u/initialize-field! form-state
                         :username
                         {:default-value "matt"
                          :default-visibility :visible})
    (testing "Hides visible fields"
      (is (false? (u/field-hidden? @form-state :username)))
      (u/hide-field! form-state :username)
      (is (true? (u/field-hidden? @form-state :username))))))
