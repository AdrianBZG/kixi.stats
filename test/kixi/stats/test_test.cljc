(ns kixi.stats.test-test
  (:require [clojure.test.check.generators :as gen]
            [kixi.stats.test :as sut]
            [kixi.stats.data :as d]
            [kixi.stats.test-helpers :refer [=ish approx= numeric]]
            #?@(:cljs
                [[clojure.test.check.clojure-test :refer-macros [defspec]]
                 [clojure.test.check.properties :refer-macros [for-all]]
                 [cljs.test :refer-macros [is deftest]]]
                :clj
                [[clojure.test.check.clojure-test :refer [defspec]]
                 [clojure.test.check.properties :refer [for-all]]
                 [clojure.test :refer [is deftest]]
                 [incanter.stats :as s]])))

(def test-opts {:num-tests 100})

(deftest chisq-test-test
  (is (=ish (sut/chisq-test (d/map->ITable {[:a :x] 2 [:a :y] 4 [:b :x] 6 [:b :y] 8}))
            {:p-value 0.6903283294641935, :X-sq 0.1587301587301587, :dof 1})))

#?(:clj
   (deftest t-test-test
     (let [xs [4 7 1 7 2 7 8 3 7 3 2 7]
           ys [4 7 2 5 1 4 7 6 1 2 4 2]]
       (is ((approx= 1e-2)
            (:p-value (sut/t-test {:mu (s/mean xs) :sd (s/sd xs) :n (count xs)}
                                  {:mu (s/mean ys) :sd (s/sd ys) :n (count ys)}))
            (:p-value (s/t-test xs :y ys)))))))

#?(:clj
   (defspec t-test-spec
     {:num-tests 50}
     (for-all [xs (gen/vector numeric 5 100)
               ys (gen/vector numeric 5 100)]
       (is (let [a (sut/t-test {:mu (s/mean xs) :sd (s/sd xs) :n (count xs)}
                               {:mu (s/mean ys) :sd (s/sd ys) :n (count ys)})
                 b (s/t-test xs :y ys)]
             #_(println xs ys)
             ((approx= 1e-2) (:p-value a) (:p-value b)))))))

