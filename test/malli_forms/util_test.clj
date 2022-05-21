(ns malli-forms.util-test
  (:require
    [clojure.test :as test :refer [deftest is testing]]
    [malli-forms.util :as util]))

(deftest munge-name-part-test
  (is (= "some%2Ffield"
         (#'util/munge-name-part "some/field")
         (#'util/munge-name-part :some/field)))
  (is (= "some%3Cfield"
         (#'util/munge-name-part "some<field")))
  (is (= "some%3Efield"
         (#'util/munge-name-part "some>field")))
  (is (= "some_DOT_namespaced%2Fkeyword"
         (#'util/munge-name-part :some.namespaced/keyword))))

(deftest path->name-test
  (is (= "my-map[some%2Ffield]"
         (util/path->name [:my-map :some/field])))
  (is (= "something" (util/path->name [:something]))))

(deftest update-in*-test
  (is (= #{{:a 1, :b 2}}
         (util/update-in* #{{:a 1}} [{:a 1}] assoc :b 2))
      "Most basic case involving a set")
  (is (= {:a {:b #{{:c 0
                    :d 1}}}}
         (util/update-in* {:a {:b #{{:c 0}}}} [:a :b {:c 0}] assoc :d 1))
      "More complex nested case")
  (is (= {:a [#{{:b [2]}}]}
         (util/update-in* {:a [#{{:b [1]}}]} [:a 0 {:b [1]} :b 0] inc))
      "Unreasonably complex case"))

(deftest generous-decode-test
  (is (= :a
         (util/generous-decode [:a :b :c :d] "a")
         (util/generous-decode [:a :b :c :d] ":a"))
      "Various ways of expressing the same thing")
  (testing "Conflict cases"
    (is (= "a"
           (util/generous-decode [:a :b :c :d "a"] "a"))
        "ambiguous case returns exact match")
    (testing "ordering"
      (is (= :a (util/generous-decode [:a :b :c :d 'a] "a"))))
    (is (= :a (util/generous-decode [:a :b :c :d "a"] ":a"))
        "less ambiguous cases")))
