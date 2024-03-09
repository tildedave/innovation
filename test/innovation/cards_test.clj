(ns innovation.cards-test
  (:require [clojure.test :refer :all]
            [innovation.core :refer :all]
            [innovation.cards :refer :all]))

(def test-card (->Card "Test Card" :green 2 [] ['crown 'crown nil 'leaf]))
(def first-card (->Card "First Card" :green 2 [] ['crown 'crown nil 'leaf]))
(def test-pile (->Pile [first-card test-card test-card] nil))
(def test-play-area (->PlayArea test-pile [] [] []))
(def test-supply-pile (->SupplyPile {1 [test-card test-card test-card] 2 [] 3 []}))
(def test-game-state (->GameState [test-play-area] test-supply-pile [] 0 2))

(deftest apply-effect-test
    (testing "apply-effect test (draw card)"
        (let [updated-game-state (apply-effect test-game-state 0 ['draw 1])]
            (is (= (count (get (:supply-pile updated-game-state) 1)) 0)
                "Supply pile was empty as expected after draw")
            (is (= (:hand (first (:play-areas updated-game-state))) [test-card])
                "Hand was as expected after drawing"))))

(deftest card-sailing-test
  (testing "sailing card test"
    (is (= (:title card-sailing) "Sailing") "Has expected title"))
  (testing "sailing card dogma"))
