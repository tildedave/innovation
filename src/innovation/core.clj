(ns innovation.core)

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

(defn make-pile
  "Create a new pile with the given card"
  [card]
  (->Pile [card] nil))

(defn make-empty-play-area 
  "Create a new play area with empty piles, score pile, hand, and no achievements"
  []
  (->PlayArea {} [] [] []))

(defrecord Board [play-areas])

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


(defn play-area-age
  "Determine which age a play-area is in"
  [play-area]
  (reduce max
    -1
    (map #(:age (first (:cards %))) (vals (:piles play-area)))))