(ns orbit.world
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener MouseListener KeyEvent))
  (:require [physics.object :as object]
            [physics.vector :as vector]
            [physics.position :as position])
  (:import physics.object.object))

(def center (position/make 500 500))

(defstruct controls :magnification :sun-center)

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

(defn to-object-coords [screen-coords mag sun-center]
  (let [x-offset (- (:x center) (* mag (:x sun-center)))
        y-offset (- (:y center) (* mag (:y sun-center)))
        x (/ (- (:x screen-coords) x-offset) mag)
        y (/ (- (:y screen-coords) y-offset) mag)]
    [x y]))

(defn to-screen-coords [object-coords mag sun-center]
  (let [x-offset (- (:x center) (* mag (:x sun-center)))
        y-offset (- (:y center) (* mag (:y sun-center)))
        x (+ x-offset (* mag (:x object-coords)))
        y (+ y-offset (* mag (:y object-coords)))]
    [x y]))

(defn draw-object [g obj mag sun-center]
  (let [[x y] (to-screen-coords (:position obj) mag sun-center)
        s (max 2 (* mag (size-by-mass obj)))
        half-s (/ s 2)
        c (color-by-mass obj)]
    (.setColor g c)
    (.fillOval g (- x half-s) (- y half-s) s s)))

(defn draw-world [g world controls]
  (doseq [obj world]
    (draw-object g obj (:magnification controls) (:sun-center controls))))

(defn draw-status [g world controls]
  (doto g
    (.setColor Color/BLACK)
    (.clearRect 0 0 1000 20)
    (.drawString (format "Objects: %d, Magnification: %4.3g, Delay: %d, Tick: %d, Tick-time:%dms %s"
                         (count world)
                         (:magnification controls)
                         (:delay controls)
                         (:tick controls)
                         (:tick-time controls)
                         (if (:track-sun controls) "Tracking" ""))
                 20 20)))

(defn draw-collision [g [age pos] mag sun-center]
  (let [[x y] (to-screen-coords pos mag sun-center)
        size (* 1.5 age)
        half-size (/ size 2)]
    (.setColor g Color/RED)
    (.fillOval g (- x half-size) (- y half-size) size size)))

(defn draw-collisions [g controls]
  (let [mag (:magnification controls)
        sun-center (:sun-center controls)]
    (doseq [collision (:collisions controls)]
      (draw-collision g collision mag sun-center))))

(defn age-collisions [collisions]
  (let [aged-collisions (map (fn [[age pos]] [(dec age) pos]) collisions)]
    (filter #(> (first %) 0) aged-collisions)))

(defn draw-world-panel [g world controls-atom]
  (let [controls @controls-atom]
    (draw-world g world controls)
    (draw-collisions g controls)
    (draw-status g world controls))
  (swap! controls-atom assoc :collisions (age-collisions (:collisions @controls-atom))))

(defn prune-history [world-history]
  (if (> (count world-history) 100)
    (let [r (rand-int 50)]
      (vec (concat (take r world-history) (drop (inc r) world-history))))
    world-history))

(defn update-world-history [world-history]
  (let [[collisions updated-world] (object/update-all (last world-history))
        new-world-history (conj world-history updated-world)]
    [collisions (prune-history new-world-history)]))

(defn add-collisions [new-collisions current-collisions]
  (concat current-collisions (map (fn [p] [10 p]) new-collisions)))

(defn update-screen [world-history-atom controls-atom]
  (let [[collisions new-world-history] (update-world-history @world-history-atom)]
    (reset! world-history-atom new-world-history)
    (swap! controls-atom assoc :collisions (add-collisions collisions (:collisions @controls-atom)))))

(defn is-sun? [o]
  (not (= -1 (.indexOf (:name o) "sun"))))

(defn find-sun [world]
  (first (filter is-sun? world)))

(defn magnify [factor controls world-history-atom]
  (let [sun-position (:position (find-sun (last @world-history-atom)))
        new-mag (* factor (:magnification @controls))]
    (swap! controls assoc
           :magnification new-mag
           :sun-center sun-position)))

(defn clear-trails [world-history]
  (vec (drop (dec (count world-history)) world-history)))


(defn slow-down [controls]
  (swap! controls #(update-in % [:delay] inc)))

(defn dec-delay [delay]
  (if (> delay 0)
    (dec delay)
    delay))

(defn speed-up [controls]
  (swap! controls #(update-in % [:delay] dec-delay)))

(defn track-sun [controls]
  (swap! controls #(update-in % [:track-sun] not)))

(defn handle-key [c world-history-atom controls]
  (condp = c
    \q (System/exit 0)
    \+ (magnify 1.3 controls world-history-atom)
    \- (magnify 0.9 controls world-history-atom)
    \_ (magnify 0.7 controls world-history-atom)
    \= (magnify 1.1 controls world-history-atom)
    \c (magnify 1.0 controls world-history-atom)
    \space (magnify 1.0 controls world-history-atom)
    \s (slow-down controls)
    \f (speed-up controls)
    \t (track-sun controls)
    nil))

(defn world-panel [frame world-history-atom controls-atom]
  (proxy [JPanel ActionListener KeyListener MouseListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (doseq [w @world-history-atom] (draw-world-panel g w controls-atom)))
    (keyPressed [e]
      (handle-key (.getKeyChar e) world-history-atom controls-atom)
      (.repaint this))
    (getPreferredSize []
      (Dimension. 1000 1000))
    (keyReleased [e])
    (keyTyped [e])
    (mouseEntered [e])
    (mouseClicked [e])
    (mouseExited [e])
    (mousePressed [e]
      (when (nil? (:mouseDown controls-atom))
        (swap! controls-atom assoc :mouseDown e :mouseUp nil)))
    (mouseReleased [e]
      (when (nil? (:mouseUp controls-atom))
        (swap! controls-atom assoc :mouseUp e)))))

(defn random-about [n]
  (- (rand (* 2 n)) n))

(defn random-velocity [p sun]
  (let [sp (:position sun)
        sd (position/distance p sp)
        v (Math/sqrt (/ 1 sd))
        direction (vector/rotate90 (vector/unit (vector/subtract p sp)))]
    (vector/scale direction (+ (rand 0.01) (* v 13.5 3)))))

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
        sun (object/make center 1500 (vector/make 0 0) v0 "sun")]
    (loop [world [sun] n 400]
      (if (zero? n)
        world
        (recur (conj world (random-object sun n)) (dec n))))))

(defn add-object-to-world [obj world-history]
  (let [last-world (conj (last world-history) obj)
        history (vec (butlast world-history))]
    (conj history last-world)))

(defn handle-mouse [world-history-atom controls-atom]
  (let [down-event (:mouseDown @controls-atom)
        up-event (:mouseUp @controls-atom)
        down-pos [(.getX down-event) (.getY down-event)]
        up-pos [(.getX up-event) (.getY up-event)]
        duration (- (.getWhen up-event) (.getWhen down-event))
        mag (:magnification @controls-atom)
        v (vector/scale (vector/subtract up-pos down-pos) (/ 0.01 mag))
        pos (to-object-coords down-pos mag (:sun-center @controls-atom))
        obj (object/make pos (/ duration 100) v (vector/make) "m")]
    (println "duration:" duration)
    (swap! world-history-atom #(add-object-to-world obj %))
    (swap! controls-atom assoc :mouseUp nil :mouseDown nil)))

(defn world-frame []
  (let [controls-atom (atom (struct-map controls
                              :magnification 1.0
                              :sun-center center
                              :delay 0
                              :track-sun true
                              :collisions []
                              :tick-time 0
                              :tick 0))
        world-history-atom (atom [(create-world)])
        frame (JFrame. "Orbit")
        panel (world-panel frame world-history-atom controls-atom)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel)
      (.addMouseListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true)
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE))
    (future
      (while true
        (let [start-time (System/currentTimeMillis)]
          (Thread/sleep (* 2 (:delay @controls-atom)))
          (when (:track-sun @controls-atom)
            (magnify 1.0 controls-atom world-history-atom))
          (update-screen world-history-atom controls-atom)
          (when (not (nil? (:mouseUp @controls-atom)))
            (handle-mouse world-history-atom controls-atom))
          (swap! controls-atom assoc :tick-time (- (System/currentTimeMillis) start-time))
          (swap! controls-atom assoc :tick (inc (:tick @controls-atom)))
          (.repaint panel)))
      )))

(defn run-world []
  (world-frame))
