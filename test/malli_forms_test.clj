(ns malli-forms-test
  (:require
    [clojure.test :as test :refer [deftest is]]
    [malli.core :as m]
    [malli-forms :as mf]))

(deftest munge-name-part-test
  (is (= "some_FSLASH_field"
         (mf/munge-name-part "some/field")
         (mf/munge-name-part :some/field)))
  (is (= "some_LT_field"
         (mf/munge-name-part "some<field")))
  (is (= "some_GT_field"
         (mf/munge-name-part "some>field")))
  (is (= "some_DOT_namespaced_FSLASH_keyword"
         (mf/munge-name-part :some.namespaced/keyword))))


(deftest path->name-test
  (is (= "my-map[some_FSLASH_field]"
         (mf/path->name [:my-map :some/field])))
  (is (= "something" (mf/path->name [:something]))))

(deftest field-spec-test
  (is (= [:and #::mf{:name  "root"
                     :path  []
                     :label nil
                     :id    "mf-root"
                     :type  :number}
          [:< #::mf{:name   "0"
                    :path   [0]
                    :label  "0"
                    :id     "mf-0"
                    :type   :number} 4]
          [:> #::mf{:name   "1"
                    :path   [1]
                    :label  "1"
                    :id     "mf-1"
                    :type   :number} 0]]
         (-> [:and [:< 4] [:> 0]]
             mf/complete-field-specs
             m/form))))

(deftest single-input-test
  (is (= [:input {:id       "mf-root"
                  :name     "root"
                  :required true
                  :type     :email}])
         (mf/encode-fields [string? {::mf/type :email}] nil))
  (is (= '(([:label {:for "mf-password"} "Password"]
            [:input {:id        "mf-password"
                     :name      "password"
                     :type      :text
                     :required  true}]))
         (mf/encode-fields [:map [:password string?]] nil)))
  (is (= '(([:label {:for "mf-email"} "Email"]
            [:input {:id        "mf-email"
                     :name      "email"
                     :type      :email
                     :required  true}]))
         (mf/encode-fields [:map [:email {::mf/type :email} string?]] nil))))
           
(deftest enum-test
  (is (= [:select {:name    "root"
                   :id      "mf-root"
                   :required true}
          '([:option {:value "one"} "One"]
            [:option {:value "two"} "Two"]
            [:option {:value "three"} "Three"])]
         (mf/encode-fields [:enum :one :two :three] nil))))

(deftest map-test
  (is (= '(([:label {:for "mf-user_FSLASH_id"} "User ID"]
            [:input {:type :email
                     :required true
                     :id "mf-user_FSLASH_id"
                     :name "user_FSLASH_id"}])
           ([:label {:for "mf-user_FSLASH_state"} "User state"]
            [:select {:id "mf-user_FSLASH_state"
                      :name "user_FSLASH_state"
                      :required true}
             ([:option {:value "active"} "Active"]
              [:option {:value "locked"} "Locked"]
              [:option {:value "suspended"} "Suspended"])]))
         (mf/encode-fields
            [:map
             [:user/id {::mf/type :email} string?]
             [:user/state [:enum :active :locked :suspended]]]
            nil)))
  (is (= '(([:label {:for "mf-user_FSLASH_id"} "User ID"]
            [:input {:type :email
                     :required true
                     :id "mf-user_FSLASH_id"
                     :name "user_FSLASH_id"
                     :value "user@example.com"}]
           ([:label {:for "mf-user_FSLASH_state"} "User state"]
            [:select {:id "mf-user_FSLASH_state"
                      :name "user_FSLASH_state"
                      :required true}
             ([:option {:value "active"} "Active"]
              [:option {:value "locked"
                        :selected true} "Locked"]
              [:option {:value "suspended"} "Suspended"])]))
         (mf/encode-fields
            [:map
             [:user/id {::mf/type :email} string?]
             [:user/state [:enum :active :locked :suspended]]]
            {:user/id     "user@example.com"
             :user/state  :locked})))))
