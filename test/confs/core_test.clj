(ns confs.core-test
  (:require [clojure.test :refer :all]
            [confs.core :as sut]))

(defn temp-path
  []
  (.getAbsolutePath (java.io.File/createTempFile "temp" "")))

(deftest test-merge-maps
  (testing "two values non-nested"
    (is (= {:a 2}
           (sut/-deep-merge
            {:a 1}
            {:a 2}))))
  (testing "two values"
    (is (= {:a {:b 1 :c 2}}
           (sut/-deep-merge
            {:a {:b 1}}
            {:a {:c 2}}))))
  (testing "three values"
    (is (= {:a {:b 1 :c 2 :d 3}}
           (sut/-deep-merge
            {:a {:b 1}}
            {:a {:c 2}}
            {:a {:d 3}}))))
  (testing "overrides"
    (is (= {:a {:b :b :c :c :d :d}}
           (sut/-deep-merge
            {:a {:b 1}}
            {:a {:c 2}}
            {:a {:b :b
                 :c :c
                 :d :d}}))))
  (testing "three levels"
    (is (= {:a {:b {:c :e}}}
           (sut/-deep-merge
            {:a {:b {:c :d}}}
            {:a {:b {:c :e}}})))))

(deftest load-confs-with-nothing
  (sut/reset!)
  (is (thrown? AssertionError (sut/conf :no-keys-in-empty-conf))))

(deftest test-load-confs
  (let [path (temp-path)
        data {:a "foo"
              :b {:c "blah"}
              :d nil
              :e false}
        _ (spit path (str data))]
    (sut/reset! path)
    (testing "nil is illegal as a value"
      (is (thrown? AssertionError (sut/conf :d))))
    (testing "false is fine as a value"
      (is (= false (sut/conf :e))))
    (testing "keys"
      (is (= (sut/conf :a) "foo")))
    (testing "nested keys"
      (is (= (sut/conf :b :c) "blah")))
    (testing "nil values, like a key miss, throws an error"
      (is (thrown? AssertionError (sut/conf :missing)))
      (is (thrown? AssertionError (sut/conf :b :missing))))))

(deftest test-load-confs-edn-str
  (let [path (temp-path)
        data {:a :default-val}]
    (spit path (str data))
    (testing "overriding a value from an edn string"
      (sut/reset! "{:a :new-val}" path)
      (is (= :new-val (sut/conf :a))))
    (testing "edn strings which read to nil are ignored"
      (sut/reset! "   " path)
      (is (= :default-val (sut/conf :a))))))

(deftest test-load-confs-multiple-paths
  (let [extra-conf (temp-path)
        base-conf (temp-path)]
    (testing "confs provided are searched left to right for values"
      (spit base-conf (str {:a :default-val}))
      (spit extra-conf (str {:a :new-val}))
      (sut/reset! extra-conf base-conf)
      (is (= :new-val (sut/conf :a))))
    (testing "the first value can be an edn-str"
      (spit base-conf (str {:a :default-val}))
      (spit extra-conf (str {:a :new-val}))
      (sut/reset! "{:a :newest-val}" extra-conf base-conf)
      (is (= :newest-val (sut/conf :a))))
    (testing "extra confs must be overriding values already defined in the last (base) sut/conf"
      (spit extra-conf (str {:unknown-key :not-gonna-work}))
      (is (thrown? AssertionError (sut/reset! extra-conf base-conf))))))

(deftest test-deeply-nested-configs
  (let [base-conf (temp-path)
        conf1 (temp-path)
        conf2 (temp-path)]
    (spit base-conf (pr-str {:a {:b {:c :d}}
                             :foo :bar}))
    (spit conf1 (pr-str {:a {:b {:c :e}}}))
    (spit conf2 (pr-str {:a {:b {:c :f}}}))
    (sut/reset! conf2 conf1 base-conf)
    (is (= :f (sut/conf :a :b :c)))
    (sut/reset! (pr-str {:foo :baz}) conf1 base-conf)
    (is (= :baz (sut/conf :foo)))
    (is (= :e (sut/conf :a :b :c)))))

(deftest load-just-empty-str
  (sut/reset! "")
  (is (thrown? AssertionError (sut/conf :literally-anything))))

(deftest test-keys-in
  (is (= [[1 2]
          [1 4]
          [1 6 7]]
         (sut/-keys-in {1 {2 :val
                           4 :val
                           6 {7 :val}}}))))
