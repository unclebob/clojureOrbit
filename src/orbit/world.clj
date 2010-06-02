(ns orbit.world
  (:import (java.awt Color Dimension)
    (javax.swing JPanel JFrame Timer JOptionPane)
    (java.awt.event ActionListener KeyListener))
  (:use clojure.contrib.import-static))

(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN)
(require ['physics.object :as 'object])
(require ['physics.vector :as 'vector])
(require ['physics.position :as 'position])

(def center (position/make 500 500))

(defstruct controls :magnification :center)

(defn size-by-mass [{m :mass}]
  (+ 3 m)
  )

(defn color-by-mass [{m :mass}]
  (let [
    blue (min 255 (int (* 20 m)))
    red (min 100 (- 255 blue))
    ]
    (Color. red 128 blue)
    )
  )

(defn draw-object [g obj controls]
  (let [
    mag (:magnification controls)
    sun-center (:center controls)
    x-offset (- (:x center) (* mag (:x sun-center)))
    y-offset (- (:y center) (* mag (:y sun-center)))
    x (+ x-offset (* mag (:x (:position obj))))
    y (+ y-offset (* mag (:y (:position obj))))
    s (max 3 (* mag (size-by-mass obj)))
    half-s (/ s 2)
    c (color-by-mass obj)
    ]
    (.setColor g c)
    (.fillOval g (- x half-s) (- y half-s) s s)
    )
  )

(defn find-sun [world]
  (first (filter #(not (= -1 (.indexOf (:name %) "sun"))) world)))

(defn draw-world [g world controls]
  (let [
    sun (find-sun world)
    ]
    (doseq [obj world]
      (draw-object g obj controls)
      )
    (.drawString g (str "Objects: " (count world)) 20 20)
    )
  )

(defn update-world [world]
  (dosync
    (alter world object/update-all))
  )

(defn magnify [factor controls world]
  (dosync
    (let [
      sun-position (:position (find-sun @world))
      new-mag (* factor (:magnification @controls))
      ]
      (alter controls #(assoc % :magnification new-mag))
      (alter controls #(assoc % :center sun-position))
      )
    )
  )


(defn world-panel [frame world controls]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (draw-world g @world @controls)
      )
    (actionPerformed [e]
      (update-world world)
      (.repaint this)
      )
    (keyPressed [e]
      (let [
        c (.getKeyChar e)
        ]
        (cond
          (= \q c) (System/exit 1)
          (or (= \+ c) (= \= c)) (magnify 1.1 controls world)
          (or (= \- c) (= \_ c)) (magnify 0.9 controls world)
          (= \space c) (magnify 1.0 controls world)
          )
        (.repaint this)
        )
      )
    (getPreferredSize []
      (Dimension. 1000 1000)
      )
    (keyReleased [e])
    (keyTyped [e])
    )
  )

(defn random-about [n]
  (- (rand (* 2 n)) n))

(defn random-velocity [p sun]
  (let [
    sp (:position sun)
    direction (vector/rotate90 (vector/unit (vector/subtract p sp)))
    ]
    (vector/scale direction (+ (rand 0.1) 0.5))
    )
  )

(defn random-position [sun-position]
  (let [
    r (+ (rand 150) 80)
    theta (rand (* 2 Math/PI))
    ]
    (position/add sun-position (position/make (* r (Math/cos theta)) (* r (Math/sin theta))))
    )
  )


(defn random-object [sun n]
  (let [
    sp (:position sun)
    p (random-position sp)
    ]
    (object/make p (rand 0.2) (random-velocity p sun) (vector/make) (str "r" n))
    )
  )

(defn create-world []
  (let [
    v0 (vector/make)
    sun (object/make center 20 (vector/make 0 0) v0 "sun")
    ]
    (loop [world [sun] n 300]
      (if (zero? n)
        world
        (recur (conj world (random-object sun n)) (dec n))
        )
      )
    )
  )

(defn world-frame []
  (let [
    controls (ref (struct controls 1 center))
    world (ref (create-world))
    frame (JFrame. "Orbit")
    panel (world-panel frame world controls)
    timer (Timer. 1 panel)
    ]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))
    (.start timer)
    )
  )

(defn run-world []
  (world-frame)
  )
