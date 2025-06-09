(module tilegrids
  "Data structures for handling rectangular grids of 'tiles'")

(defprotocol TileGrid
  (width [this])
  (height [this])
  (write [this x y val])
  (read [this x y])
  (update-xy [this x y f & args])
  (reduce-xyt [this f init]))

(deftype BaseGrid
  TileGrid
  (update-xy [this x y f & args]
    (write this x y (apply f (read this x y) args))
    this)

  MutableCollection
  (-conj! [this v]
    (if (vector? v)
      (let [[x y v] v]
        (write this x y v))
      (write this (:x v) (:y v) v))
    this)

  Seqable
  (-seq [this]
    (seq
      (reduce-xyt this (fn [acc x y t] (conj! acc t)) []))))

(deftype FixedTileGrid :extends BaseGrid
  (constructor [width height]
    (set! (.-width this) width)
    (set! (.-height this) height)
    ;; Make sure we don't end up with a sparse array, we don't want that here
    (set! (.-tiles this)
      (into [] (map (constantly nil) (range (* width height)))))
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
  (reduce-xyt [this f init]
    (let [w (.-width this)
          v (box init)]
      (.forEach (.-tiles this)
        (fn [t idx]
          (swap! v f (mod idx w) (quot idx w) t)))
      @v)))

(deftype RectGrid :extends BaseGrid
  (constructor [width height]
    (set! (.-width this) width)
    (set! (.-height this) height)
    ;; Make sure we don't end up with a sparse array, we don't want that here
    (set! (.-tiles this)
      (mapv #(mapv (constantly nil) (range width)) (range height)))
    nil)

  (clear []
    (let [{:props [width height]} this]
      (set! (.-tiles this)
        (mapv #(mapv (constantly nil) (range width)) (range height)))))

  TileGrid
  (width [this] (.-width this))
  (height [this] (.-height this))
  (write [this x y v]
    (set! (oget (oget (.-tiles this) y) x)
      v))
  (read [this x y]
    (oget (oget (.-tiles this) y) x))
  (reduce-xyt [this f init]
    (let [w (.-width this)
          v (box init)]
      (.forEach (.-tiles this)
        (fn [row y]
          (.forEach row
            (fn [t x]
              (swap! v f x y t)))))
      @v)))

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

  (reduce-xyt [this f init]
    (let [w (width this)
          overlays (butlast (.-layers this))
          bottom (last (.-layers this))]
      (reduce-xyt bottom
        (fn [acc x y t]
          (let [u (some #(read % x y) overlays)]
            (f acc x y (or u t))))
        init))))

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
    (def g (RectGrid. 3 3))
    (def s (SparseGrid. 3 3))
    (write g 1 1 {:t 0})
    (write g 0 1 {:t 2})
    (write s 2 2 {:t 1})
    (def l (LayeredGrid. [s g])))

  (reduce-xyt l (fn [acc x y t]
                  (conj acc [x y t])) [])

  )

;;   (do
;;     (def g (FixedTileGrid. 100 100))
;;     (def s (SparseGrid. 100 100))
;;     (write g 99 98 {:t 0})
;;     (write s 99 99 {:t 1})
;;     (def l (LayeredGrid. [s g])))

;;   (do
;;     (time (count (seq g)))
;;     (time (count (seq s)))
;;     (time (count (seq l)))))

;; (time
;;   (count
;;     (js:Array. 100000000)))

;; (type-name (seq
;;              (js:Array. 1000000)))

;; (.-iterable
;;   (seq [1 2 3]))

;; (.-iterable
;;   (.of_iterable IteratorSeq [1 2 3]))

;; (time
;;   (reduce
;;     (fn [acc _] (inc acc))
;;     0
;;     (seq
;;       (js:Array. 1000000))))

;; (def a [1 2 3]))
;; (def s (seq a))

;; (.unshift a 4)

;; (defn foo [g]
;;   (read g 1 2))
