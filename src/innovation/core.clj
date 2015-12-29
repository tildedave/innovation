(ns innovation.core)

; resources:
; - crown
; - leaf
; - tower
; - factory
; - clock
; - lightbulb

(defrecord Card [title age dogmas resources])

; splay-directions:
; - nil (no splay)
; - left
; - right
; - up

(defrecord Pile [cards splay-direction])

(defrecord PlayerBoard [piles hand])

(defrecord Board [player-boards])

(defn count-card-resources
  "Count the number of resources on a card"
  [card resource-to-find]
  (reduce 
    #(if (= %2 resource-to-find) (+ %1 1) %1)
    0 
    (:resources card)))

(defn filter-card-resources 
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
            nil (fn [x] false)
            'left #(= % 3)
            'right #(or (= % 0) (= % 1))
            'up #(> % 0))]
          (map #(filter-card-resources filter-fn %) rest-cards))))))