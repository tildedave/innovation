(ns innovation.core
  (:require [clojure.tools.trace :refer :all]))

; colors:
; - green
; - yellow
; - blue
; - red
; - purple

; resources:
; - crown
; - leaf
; - tower
; - factory
; - clock
; - lightbulb

(defrecord Card [title color age dogmas resources])

; splay-directions:
; - nil (no splay)
; - left
; - right
; - up

(defrecord Pile [cards splay-direction])

(defrecord PlayArea [piles score-pile achivements hand])

(defrecord Dogma [icon effect])

(defn make-pile
  "Create a new pile with the given card"
  [card]
  (->Pile [card] nil))

(defn make-empty-play-area
  "Create a new play area with empty piles, score pile, hand, and no achievements"
  []
  (->PlayArea {} [] [] []))

(defrecord SupplyPile [age-map])

(defrecord GameState [play-areas supply-pile available-achievements player-index actions-left])

(defn count-card-resources
  "Count the number of resources on a card"
  [card resource-to-find]
  (reduce
    #(if (= %2 resource-to-find) (+ %1 1) %1)
    0
    (:resources card)))

(defn- filter-card-resources
  "Filter out resources in a card (for splaying calculation)"
  [f card]
  (update-in
    card
    [:resources]
    (fn [x]
      (map-indexed (fn [i res] (if (f i) res nil))
        (:resources card)))))

(defn count-pile-resources
  "Count the number of resources on a pile"
  [pile resource-to-find]
  (let [first-card (first (:cards pile)) rest-cards (rest (:cards pile))]
    (reduce
      ; there's a better way to do this I think
      #(+ %1 (count-card-resources %2 resource-to-find))
      0
      (cons first-card
        (let [filter-fn (condp = (:splay-direction pile)
            nil    (fn [x] false)
            'left  #(= % 3)
            'right #(or (= % 0) (= % 1))
            'up    #(> % 0))]
          (map #(filter-card-resources filter-fn %) rest-cards))))))

(defn draw-card
  "Draw a card from a supply pile.  Returns the card drawn and a new supply pile"
  [supply-pile age]
  (let [age-map (:age-map supply-pile)
        ; if you tried to draw a 1 and there's no 1 available, draw the next one
        ; actual-age iterates through the age-map to find the first pile that's
        ; non-empty but equal to or greater than the request age, then returns those
        ; cards
        [actual-age cards] (first (filter #(and (> (count (second %)) 0) (>= (first %) age)) age-map))]
    {:card (first (trace cards)),
     :supply-pile (update supply-pile :age-map (fn [map] (assoc map actual-age (rest cards))))}))

(defn return-card
  "Return a card to a supply pile."
  [supply-pile card]
  (update
    supply-pile
    :age-map
    (fn [map] (update map (:age card) #(conj % card)))))

(defn meld-card
  "Meld a card onto a play area. Creates a new pile if the color does not yet exist"
  [play-area card]
  (update-in play-area [:piles (:color card)]
    (fn [pile]
      (if (nil? pile)
        (make-pile card)
        (update pile :cards (fn [cards] (cons card cards)))))))

(defn tuck-card
  "Tuck a card onto the play area. Creates a new pile if the color does not yet exist"
  [play-area card]
  (update-in play-area [:piles (:color card)]
    (fn [pile]
      (if (nil? pile)
        (make-pile card)
        (update pile :cards (fn [cards] (conj cards card)))))))

(defn score-card
  "Score a card onto the play area"
  [play-area card]
  (update play-area :score-pile (fn [pile] (cons card pile))))

(defn play-area-age
  "Determine which age a play-area is in"
  [play-area]
  (reduce max
    1
    (map #(:age (first (:cards %))) (vals (:piles play-area)))))

(defn generate-possible-moves
  "Generate possible moves from a game state"
  [game-state]
  (let [current-play-area (nth (:play-areas game-state) (:player-index game-state))
        current-age (play-area-age current-play-area)]
    (concat
      [['draw current-age]]
      (map (fn [card] ['meld card]) (:hand current-play-area)))))
