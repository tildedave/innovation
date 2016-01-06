(ns innovation.core-test
  (:require [clojure.test :refer :all]
            [innovation.core :refer :all]))


(def test-card (->Card "Test Card" :green 2 [] ['crown 'crown nil 'leaf]))
(def test-card-red (assoc test-card :color :red))
(def test-card-first-age (assoc test-card :age 1))

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

(defn- make-pile-age
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
    (is (= (play-area-age empty-play-area) 1) "no cards means age is 1")
    (is (=
        (play-area-age (assoc test-play-area :piles {:green (make-pile-age test-pile 1)}))
        1)
      "age was as expected")))

(deftest meld-card-test
  (testing "meld a card"
    (is (= (-> (meld-card empty-play-area test-card) :piles :green)
           (->Pile [test-card] nil))
      "Melding a card creates an empty pile")
    (is (=
      (-> empty-play-area
        (#(meld-card % test-card))
        (#(meld-card % test-card-first-age))
        :piles
        :green)
      (->Pile [test-card-first-age test-card] nil))
      "Melding the same color twice creates a pile with second card on top")
    (let [play-area-with-two-piles
            (-> empty-play-area
              (#(meld-card % test-card))
              (#(meld-card % test-card-red)))]
      (is (= (-> play-area-with-two-piles :piles :red) (->Pile [test-card-red] nil))
        "Melding creates a new test pile (red card)")
      (is (= (-> play-area-with-two-piles :piles :green) (->Pile [test-card] nil))
        "Melding creates a new test pile (green card)"))))

(deftest tuck-card-test
  (testing "tuck a card"
    (is (= (-> (tuck-card empty-play-area test-card) :piles :green)
           (->Pile [test-card] nil))
      "Tucking a card creates an empty pile")
    (is (=
      (-> empty-play-area
        (#(tuck-card % test-card))
        (#(tuck-card % test-card-first-age))
        :piles
        :green)
      (->Pile [test-card test-card-first-age] nil))
      "Tucking the same color twice creates a pile with second card on bottom")))

(def test-supply-pile (->SupplyPile {1 [], 2 [test-card]}))

(deftest draw-card-test
  (testing "draw a card (no age fallback)"
    (let [result (draw-card test-supply-pile 2)]
      (is (= (:card result) test-card)
        "Drawing a card returned the top card for the age")
      (is (= (:supply-pile result) (->SupplyPile {1 (), 2 ()}))
        "Drawing a card returned an updated supply pile")))
  (testing "draw a card (age fallback)"
    (let [result (draw-card test-supply-pile 1)]
      (is (= (:card result) test-card)
        "Drawing a card from an empty age defaulted to the next one")
      (is (= (:supply-pile result) (->SupplyPile {1 (), 2 ()}))
        "Drawing a card returned an updated supply pile")))
  (testing "draw a card from higher age"
    (let [result
      (draw-card (->SupplyPile {1 [test-card], 2 [], 3 [], 4 [test-card]}) 2)]
      (is (= (:card result) test-card)
        "Drawing a card from a higher age fell back multiple times")
      (is (= (:supply-pile result) (->SupplyPile {1 [test-card], 2 [], 3 [], 4 []}))
        "Drawing a card from a higher age fell back multiple times"))))

(deftest return-card-test
  (testing "return a card"
    (is (= (return-card test-supply-pile test-card-red)
           (->SupplyPile {1 [], 2 [test-card test-card-red]}))
      "Returning a card put it on the bottom of the supply pile")
    (is (= (return-card test-supply-pile test-card-first-age)
           (->SupplyPile {1 [test-card-first-age], 2 [test-card]}))
      "Returning a card put it on the bottom of an empty supply pile")))

(deftest score-card-test
  (testing "score a card"
    (is (= (:score-pile (score-card empty-play-area test-card)) [test-card])
      "Scoring a card added it to the score pile")))

(def hand-card (assoc test-card :title "Card from Hand"))

(def current-player-area
  (assoc empty-play-area
    :hand [hand-card]
    :piles {:red (->Pile [test-card] nil)}))

(def test-game-state
  (->GameState [empty-play-area current-player-area] test-supply-pile [test-card-first-age] 1 2))

(deftest game-test-state
  (testing "game state"
    (let [next-moves (generate-possible-moves test-game-state)]
      (is (some #(= ['draw 2] %) next-moves)
        "Drawing from the current age was a possible next move")
      (is (some #(= ['meld hand-card] %) next-moves)
        "Melding a card from the hand was a possible next move"))))