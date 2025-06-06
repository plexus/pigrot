(module tilegrids
  "Data structures for handling rectangular grids of 'tiles'")

(defprotocol TileGrid
  (width [this])
  (height [this])
  (write [this x y val])
  (read [this x y]))

(deftype BaseGrid
  MutableCollection
  (-conj! [this v]
    (if (vector? v)
      (let [[x y v] v]
        (write this x y v))
      (write this (:x v) (:y v) v))
    this)

  Seqable
  (-seq [this]
    (for [y (range (height this))
          x (range (width this))
          :let [t (read this x y)]]
      (when t
        [x y t]))))

(deftype FixedTileGrid :extends BaseGrid
  (constructor [width height]
    (set! (.-width this) width)
    (set! (.-height this) height)
    (set! (.-tiles this) (js:Array (* width height)))
    nil)

  (clear [] (set! (.-tiles this) []))

  TileGrid
  (width [this] (.-width this))
  (height [this] (.-height this))
  (write [this x y v]
    (set! (oget
            (.-tiles this)
            (+ x (* y (.-width this))) )
      v))
  (read [this x y]
    (oget
      (.-tiles this)
      (+ x (* y (.-width this)))))

  Seqable
  (-seq [this]
    (let [w (.-width this)]
      (seq
        (.map (.-tiles this)
          (fn [t idx]
            (when t
              [(mod idx w) (quot idx w) t])))))))

(deftype LayeredGrid :extends BaseGrid
  :fields [(layers [])]

  TileGrid
  (width [this] (apply max (map width (.-layers this))))
  (height [this] (apply max (map width (.-layers this))))
  (read [this x y]
    (loop [i 0]
      (if-let [l (oget (.-layers this) i)]
        (if-let [r (read l x y)]
          r
          (recur (inc i))))))
  (write [this x y v]
    (throw (js:Error. "Can't write to LayeredGrid, write to individual layers")))

  Seqable
  (-seq [this]
    (let [w (width this)
          top (first (.-layers this))
          bottom (last (.-layers this))]
      (seq
        (js:Array.from
          (seq bottom)
          (fn [t idx]
            (let [x (mod idx w)
                  y (quot idx w)
                  u (read top x y)]
              (or
                (and u [x y u])
                t))))))))

(deftype SparseGrid :extends BaseGrid
  :fields [width height (tiles #js {})]

  (clear [] (set! (.-tiles this) #js {}))

  TileGrid
  (width [this] (.-width this))
  (height [this] (.-height this))
  (write [this x y v]
    (set! (oget
            (.-tiles this)
            (js:String.fromCharCode (+ x (* y (.-width this)))) )
      v))
  (read [this x y]
    (oget
      (.-tiles this)
      (js:String.fromCharCode
        (+ x (* y (.-width this)))))))

(deftype CameraGrid :extends BaseGrid
  :fields [w h vw wh grid (cx 0) (cy 0)]
  (pan [cx cy]
    (set! (.-cx this) cx)
    (set! (.-cy this) cy))

  TileGrid
  (width [this] (.-vw this))
  (height [this] (.-vh this))
  (read [this x y]
    (read (.-grid this)
      (+ x (.-cx this))
      (+ y (.-cy this))))
  (write [this x y v]
    (write (.-grid this)
      (+ x (.-cx this))
      (+ y (.-cy this)))
    v))


(comment
  (do
    (def g (FixedTileGrid. 3 3))
    (def s (SparseGrid. 3 3))
    (write g 1 1 {:t 0})
    (write s 2 2 {:t 1})
    (seq (LayeredGrid. [s g])))

  (do
    (def g (FixedTileGrid. 100 100))
    (def s (SparseGrid. 100 100))
    (write g 99 98 {:t 0})
    (write s 99 99 {:t 1})
    (def l (LayeredGrid. [s g])))

  (do
    (time (count (seq g)))
    (time (count (seq s)))
    (time (count (seq l)))))

(time
  (count
    (js:Array. 100000000)))

(type-name (seq
             (js:Array. 1000000)))

(.-iterable
  (seq [1 2 3]))

(.-iterable
  (.of_iterable IteratorSeq [1 2 3]))

(time
  (reduce
    (fn [acc _] (inc acc))
    0
    (seq
      (js:Array. 1000000))))

(def a [1 2 3]))
(def s (seq a))

(.unshift a 4)

(defn foo [g]
  (read g 1 2))
