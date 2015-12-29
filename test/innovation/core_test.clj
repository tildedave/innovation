(ns innovation.core-test
  (:require [clojure.test :refer :all]
            [innovation.core :refer :all]))


(def test-card (->Card "Test Card" :green 2 [] ['crown 'crown nil 'leaf]))

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

(def test-pile (->Pile [test-card test-card test-card] nil))

(deftest pile-test
  (let [left-splayed-pile (assoc test-pile :splay-direction 'left)
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

(defn- make-color
  "Transform a pile into a different color for testing"
  [pile color]
  (update pile :cards (fn [cards] (map #(assoc % :color color) cards))))

(defn- make-age
  "Transform a pile to a different age (top card) for testing"
  [pile age]
  (update pile :cards (fn [cards] (cons (assoc (first cards) :age age) (rest cards)))))

(def test-play-area (->PlayArea 
  {:green (make-color test-pile :green), :red (make-color test-pile :red)}
  [test-card (assoc test-card :age 2)]
  [test-card]
  []))

(def empty-play-area (make-empty-play-area))

(deftest play-area-test
  (testing "can make a play area"
    (is (= (-> test-play-area :piles :green)
           (make-color test-pile :green)) "green pile was as expected")
    (is (= (-> test-play-area :piles :red)
           (make-color test-pile :red)) "red pile was as expected"))
  (testing "determines 'age' of play area"
    (is (= (play-area-age test-play-area) 2) "age was as expected")
    (is (= 
        (play-area-age (assoc test-play-area :piles {:green (make-age test-pile 1)}))
        1)
      "age was as expected"))
  (testing "meld a card"
    (is (= (-> (meld-card-to-play-area empty-play-area test-card) :piles :green)
           (->Pile [test-card] nil))
      "Melding a card creates an empty pile")
    (is (= (:piles (meld-card-to-play-area (meld-card-to-play-area empty-play-area test-card) test-card))
           {:green (->Pile [test-card test-card] nil)})
      "Melding the same card card twice creates a pile with two cards")))