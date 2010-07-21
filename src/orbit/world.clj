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

(defstruct controls :magnification :center :trails :clear)

(defn size-by-mass [{m :mass}]
  (+ 0 (Math/sqrt m))
  )

(defn color-by-mass [{m :mass}]
  (cond
    (< m 1) Color/black
    (< m 2) (Color. 210 105 30)
    (< m 5) Color/red
    (< m 10) (Color. 107 142 35)
    (< m 20) Color/magenta
    (< m 40) Color/blue
    :else (Color. 255 215 0)
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
    s (max 2 (* mag (size-by-mass obj)))
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
    (.clearRect g 0 0 1000 20)
    (.drawString g (format "Objects: %d, Magnification: %4.3g"
      (count world)
      (:magnification controls)) 20 20)
    )
  )

(defn update-world [world controls]
  (dosync
    (alter world #(object/update-all %)))
  )

(defn magnify [factor controls world]
  (dosync
    (let [
      sun-position (:position (find-sun @world))
      new-mag (* factor (:magnification @controls))
      ]
      (alter controls #(assoc % :magnification new-mag))
      (alter controls #(assoc % :center sun-position))
      (alter controls #(assoc % :clear true))
      )
    )
  )

(defn reset-screen-state [controls]
  (dosync (alter controls #(assoc % :clear false))))

(defn toggle-trail [controls]
  (dosync (alter controls #(assoc % :trails (not (:trails @controls))))))

(defn- quit-key? [c]
  (= \q c)
  )

(defn- plus-key? [c]
  (or (= \+ c) (= \= c))
  )

(defn- minus-key? [c]
  (or (= \- c) (= \_ c))
  )

(defn- space-key? [c]
  (= \space c)
  )

(defn- trail-key? [c]
  (= \t c)
  )

(defn handle-key [c world controls]
  (cond
    (quit-key? c) (System/exit 1)
    (plus-key? c) (magnify 1.1 controls world)
    (minus-key? c) (magnify 0.9 controls world)
    (space-key? c) (magnify 1.0 controls world)
    (trail-key? c) (toggle-trail controls)
    ))

(defn world-panel [frame world controls]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      (when (or (:clear @controls) (not (:trails @controls)))
        (proxy-super paintComponent g))
      (draw-world g @world @controls)
      (reset-screen-state controls)
      )
    (actionPerformed [e]
      (update-world world @controls)
      (.repaint this)
      )
    (keyPressed [e]
      (handle-key (.getKeyChar e) world controls)
      (.repaint this)
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
    sd (position/distance p sp)
    v (Math/sqrt (/ 1 sd))
    direction (vector/rotate90 (vector/unit (vector/subtract p sp)))
    ]
    (vector/scale direction (+ (rand 0.01) (* v 13.5)))
    )
  )

(defn random-position [sun-position]
  (let [
    r (+ (rand 300) 30)
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
    sun (object/make center 150 (vector/make 0 0) v0 "sun")
    ]
    (loop [world [sun] n 500]
      (if (zero? n)
        world
        (recur (conj world (random-object sun n)) (dec n))
        )
      )
    )
  )

(defn world-frame []
  (let [
    controls (ref (struct-map controls
      :magnification 1.0
      :center center
      :trails false
      :clear false))
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
