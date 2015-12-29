(ns innovation.core-test
  (:require [clojure.test :refer :all]
            [innovation.core :refer :all]))


(def test-card (->Card "Test Card" 2 [] ['crown 'crown nil 'leaf]))

(deftest card-test
  (testing "can make a card"
    (is (= (:title test-card "Test Card")) "Card has expected title")
    (is (= (:age test-card 2)) "Card has expected age")
  (testing "has expected resources"
    (is (= (count-card-resources test-card 'crown) 2) 
      "Card has expected number of crowns")
    (is (= (count-card-resources test-card 'leaf) 1)
      "Card has expected number of leaves")
    (is (= (count-card-resources test-card 'tower) 0)
      "Card has expected number of towers"))))

(def test-left-pile (->Pile [test-card test-card test-card] 'left))
(def test-right-pile (->Pile [test-card test-card test-card] 'right))
(def test-up-pile (->Pile [test-card test-card test-card] 'up))

(deftest pile-test
  (let [test-pile (->Pile [test-card test-card test-card] nil)
        left-splayed-pile (assoc test-pile :splay-direction 'left)
        right-splayed-pile (assoc test-pile :splay-direction 'right)
        up-splayed-pile (assoc test-pile :splay-direction 'up)]
    (testing "can make a pile"
      (is (= (:splay-direction test-pile) nil)
        "Pile has expected splay direction")
      (is (= (count-pile-resources test-pile 'crown) 2)
        "Properly counted number of crowns when not splayed")
      (is (= (count-pile-resources test-pile 'leaf) 1)
        "Properly counted number of leaves when not splayed"))
    (testing "pile is splayed left"
      (is (= (:splay-direction left-splayed-pile) 'left)
        "Pile has expected splay direction")
      (is (= (count-pile-resources left-splayed-pile 'crown) 2)
        "Properly counted number of crowns when splayed left")
      (is (= (count-pile-resources left-splayed-pile 'leaf) 3)
        "Properly counted number of leaves when splayed left"))
    (testing "pile is splayed right"
      (is (= (:splay-direction right-splayed-pile) 'right)
        "Pile has expected splay direction")
      (is (= (count-pile-resources right-splayed-pile 'crown) 6)
        "Properly counted number of crowns when splayed right")
      (is (= (count-pile-resources right-splayed-pile 'leaf) 1)
        "Properly counted number of leaves when splayed right"))
    (testing "pile is splayed up"
      (is (= (:splay-direction up-splayed-pile) 'up)
        "Pile has expected splay direction")
      (is (= (count-pile-resources up-splayed-pile 'crown) 4)
        "Properly counted number of crowns when splayed up")
      (is (= (count-pile-resources up-splayed-pile 'leaf) 3)
        "Properly counted number of leaves when splayed up"))))