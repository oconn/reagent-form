(ns reagent-form.utils-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]

            [reagent-form.utils :as u]))

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

(deftest initialized-field!
  (testing "Adds a field to form-state when properly initialized"
    (is (=  (u/initialize-field! (atom {}) :username {})
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
