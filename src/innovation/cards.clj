(ns innovation.cards
  (:require [innovation.core :refer :all]
            [clojure.tools.trace :refer :all]))

(defn apply-effect
    "Apply an effect (list of an effect with appropriate data) to a
     gamestate and returns the updated gamestate"
    [game-state player-index effect]
    (case (first effect)
        'draw
            (let [age (second effect)
                  {card :card supply-pile :supply-pile} (trace (draw-card (trace (:supply-pile game-state)) age))]
                  ; TODO - add card to hand
                  (assoc game-state :supply-pile supply-pile))))

; TODO dogmas
(def card-sailing (->Card "Sailing" 'green 1 [-] ['crown 'crown nil 'leaf]))
