(module generators
  (:import
    [e :from engine]
    [rot :from "rot-js"]))

(defn cellular-gen []
  (rot:Map.Cellular.
    (e:viewport-width)
    (e:viewport-height)
    #js {:born [5 6 7 8]
         :survive [4 5 6 7 8]}))

(defn gen-lake [size]
  (let [lake #{(e:empty-spot #{:air})}
        lake (reduce (fn [lake _]
                       (let [[x y] (rand-nth lake)
                             x (+ x (rand-nth [-1 0 1]))
                             y (+ y (rand-nth [-1 0 1]))]
                         (if (= :air (:type (e:env-by-coords x y)))
                           (conj lake [x y])
                           lake)))
               lake
               (range size))]
    (mapcat (fn [[x y]]
              (cons [x y]
                (for [dx [-1 0 1]
                      dy [-1 0 1]
                      :let [x (+ x dx)
                            y (+ y dy)]
                      :when (and
                              (< 0.5 (rand))
                              (= :air (:type (e:env-by-coords x y))))]
                  [x y]))) lake)))

(defn add-tree! []
  (let [x (rand-int (e:viewport-width))
        y (rand-int (e:viewport-height))]
    (e:ent-set! (e:new-eid)
      {:x x
       :y y
       :tile :tree})))
