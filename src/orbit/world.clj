(ns orbit.world
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener))
  (:use clojure.contrib.import-static)
  (:require [physics.object :as object]
            [physics.vector :as vector]
            [physics.position :as position]))

(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN)

(def center (position/make 500 500))

(defstruct controls :magnification :center)

(defn size-by-mass [{m :mass}]
  (+ 0 (Math/sqrt m)))

(defn color-by-mass [{m :mass}]
  (condp > m
    1 Color/black
    2 (Color. 210 105 30)
    5 Color/red
    10 (Color. 107 142 35)
    20 Color/magenta
    40 Color/blue
    (Color. 255 215 0)))

(defn draw-object [g obj controls]
  (let [mag (:magnification controls)
        sun-center (:center controls)
        x-offset (- (first center) (* mag (first sun-center)))
        y-offset (- (last center) (* mag (last sun-center)))
        x (+ x-offset (* mag (first (:position obj))))
        y (+ y-offset (* mag (last (:position obj))))
        s (max 2 (* mag (size-by-mass obj)))
        half-s (/ s 2)
        c (color-by-mass obj)]
    (.setColor g c)
    (.fillOval g (- x half-s) (- y half-s) s s)))

(defn find-sun [world]
  (first (filter #(not (= -1 (.indexOf (:name %) "sun"))) world)))

(defn draw-world [g world controls]
  (let [sun (find-sun world)]
    (doseq [obj world]
      (draw-object g obj controls))
    (.clearRect g 0 0 1000 20)
    (.drawString g (format "Objects: %d, Magnification: %4.3g"
                           (count world)
                           (:magnification controls)) 20 20)))

(defn update-world-history [world]
  (let [new-world (conj world (object/update-all (last world)))]
    (if (> (count new-world) 200)
      (let [r (rand-int 100)]
        (vec (concat (take r new-world) (drop (inc r) new-world))))
      new-world)))

(defn update-world [world controls]
  (swap! world update-world-history))

(defn magnify [factor controls world]
  (dosync
    (let [sun-position (:position (find-sun (last @world)))
          new-mag (* factor (:magnification @controls))]
      (swap! controls assoc
             :magnification new-mag
             :center sun-position))))

(defn clear-trails [world]
  (vec (drop (dec (count world)) world)))

(defn handle-key [c world controls]
  (condp = c
    \q (System/exit 0)
    \+ (magnify 1.3 controls world)
    \- (magnify 0.9 controls world)
    \_ (magnify 0.7 controls world)
    \= (magnify 1.1 controls world)
    \c (magnify 1.0 controls world)
    \space (do
             (swap! world clear-trails)
             (magnify 1.0 controls world))
    nil))

(defn world-panel [frame world controls]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (doseq [w @world] (draw-world g w @controls)))
    (keyPressed [e]
      (handle-key (.getKeyChar e) world controls)
      (.repaint this))
    (getPreferredSize []
      (Dimension. 1000 1000))
    (keyReleased [e])
    (keyTyped [e])))

(defn random-about [n]
  (- (rand (* 2 n)) n))

(defn random-velocity [p sun]
  (let [sp (:position sun)
        sd (position/distance p sp)
        v (Math/sqrt (/ 1 sd))
        direction (vector/rotate90 (vector/unit (vector/subtract p sp)))]
    (vector/scale direction (+ (rand 0.01) (* v 13.5)))))

(defn random-position [sun-position]
  (let [r (+ (rand 300) 30)
        theta (rand (* 2 Math/PI))]
    (position/add sun-position (position/make (* r (Math/cos theta)) (* r (Math/sin theta))))))

(defn random-object [sun n]
  (let [sp (:position sun)
        p (random-position sp)]
    (object/make p (rand 0.2) (random-velocity p sun) (vector/make) (str "r" n))))

(defn create-world []
  (let [v0 (vector/make)
        sun (object/make center 150 (vector/make 0 0) v0 "sun")]
    (loop [world [sun] n 500]
      (if (zero? n)
        world
        (recur (conj world (random-object sun n)) (dec n))))))

(defn world-frame []
  (let [controls (atom (struct-map controls
                         :magnification 1.0
                         :center center))
        world (atom [(create-world)])
        frame (JFrame. "Orbit")
        panel (world-panel frame world controls)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true)
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE))
    (future
      (while true
        (update-world world @controls)
        (.repaint panel)))))

(defn run-world []
  (world-frame))
