(ns dla.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [constraints.core :as c]
            [kdtree :as kd]
            [clojure.set :as s]))

(def radius 5.)
(def link-length 10.) ; 2x radius

(defn prnt [x] (println x) x)
(defn prntc [c] (println 'c (:x (:c c)) (:y (:c c)) (:id c) (:link c) (:back-links c)) c)
(defn prntcs [cs] (doseq [c cs] (prntc c)) cs)
(defn iterate-times [n f init] (first (drop n (iterate f init))))
(defn to-v2 [p] [(:x p) (:y p)])

;;;;;;;;;; Circles ;;;;;;;;;;;;;;
(defn mk-circle [x y r] {:c {:x x :y y} :r r :id 0 :link-dist 0})
(defn add-circle [c cs] 
  (let [id (+ 1 (max (map :id cs)))]
    (conj (assoc c :id id) cs)))


(defn center []
  (assoc (mk-circle 0 0 radius)
         :id 0
         :layer 0))

(defn randomize-radii [circles]
  (map (fn [c] (assoc c :r (q/random 2.5 10))) circles))

(defn add-back-links [circles]
  (map (fn [c] 
         (assoc c :back-links 
                (->> circles
                     (filter #(= (:id c) (:link %)))
                     (map :id)
                     set)))
       circles))

;;;;;;;;;; KD-TREE Particle Approach ;;;;;;;;;;;;;;

(defn simple-stick? [two-nears]
  (and (not
         (and (= 2 (count two-nears))
              (< (:dist-squared (first two-nears)) (Math/pow (* 2 (:r (meta (first two-nears)))) 2))
              (< (:dist-squared (second two-nears)) (Math/pow (* 2 (:r (meta (second two-nears)))) 2))))

       (< (:dist-squared (first two-nears)) (Math/pow (* 2 (:r (meta (first two-nears)))) 2))))

(defn stick-particle [tree rand-range stick?]
  (let [sz rand-range -sz (- rand-range)
        w sz -w -sz 
        h sz -h -sz] 
    (loop [p [(q/random -w w) (q/random -h h)]]
      (let [near (kd/nearest-neighbor tree p 2)]
        (if (stick? near)
          [p (first near)]
          (recur [(q/random -w w) (q/random -h h)]))))))

(defn aggregate [nparts]
  (first 
    (iterate-times nparts
                   (fn [[cs tree rand-range]] 
                     (let [[p hit] (stick-particle tree rand-range simple-stick?)
                           c (assoc (mk-circle (p 0) (p 1) radius) 
                                    :id (+ 1 (:id (last cs)))
                                    :link (:id (meta hit))
                                    :layer (+ 1 (:layer (meta hit))))]
                       [(conj cs c)
                        (kd/insert tree (with-meta (to-v2 (:c c)) c))
                        (cond (> (+ 10 (p 0)) rand-range) (+ 10 (p 0))
                              (> (+ 10 (p 1)) rand-range) (+ 10 (p 1))
                              :otherwise rand-range
                              )]))
                   (let [c0 (center)
                         c1 (assoc c0 :c (c/add (:c c0) {:x    0 :y  150}) :id 1)
                         c2 (assoc c0 :c (c/add (:c c0) {:x  150 :y    0}) :id 2)
                         c3 (assoc c0 :c (c/add (:c c0) {:x    0 :y -150}) :id 3)
                         c4 (assoc c0 :c (c/add (:c c0) {:x -150 :y    0}) :id 4)
                         cs [c0]]; c1 c2 c3 c4]]
                     [cs
                      (kd/build-tree (vec (map #(with-meta (to-v2 (:c %)) %) cs)))
                      10 ;initial area
                      ]))))


;;;;;;;;;; QUIL ;;;;;;;;;;;;;;
(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  (println 'setup)
  (let [circles (vec (add-back-links (aggregate 1000)))]
    ;(prntcs circles)
;    (println (count circles) (count circles-changed))
;    (println ((vec (concat circles circles-changed)) 1001) )
    {:circles circles; (vec (concat circles circles-changed))
     :lit-layer 0
     :radius radius
     :max-layer (apply max (map :layer circles))
     :frame 0 }
    ))


(defn update-state [state]
  (println (:frame state))
  (assoc state
         :frame (inc (:frame state))))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  ;(q/fill 150 150 150)
  (q/no-fill)
  (q/stroke 150 150 150)
  ; Move origin point to the center of the sketch.
  (q/with-translation [(/ (q/width) 2)
                       ;30
                       (/ (q/height) 2)
                       ]
    (doseq [c (:circles state)]
      (when (zero? (:id c))
        (q/ellipse (:x (:c c)) (:y (:c c)) (* 1.5 (:r c)) (* 1.5 (:r c))))
      (when (:link c)
        ; Draw the circle.
        ;(println (c 0) (c 1))
        ;        (q/ellipse (c 0) (c 1) 2 2)
        ;(when (or true (< (:layer c) (:lit-layer state)))
        (q/line (:x (:c c))
                (:y (:c c))
                (:x (:c ((:circles state) (:link c)))) 
                (:y (:c ((:circles state) (:link c)))))
        ;  )

        )))
  ;(when (= (:frame state) (:lit-layer state))
;  (when (< (:frame state) 60)
;    (q/save-frame "dla2000-####.png"))
  ;)
  )


(q/defsketch dla
  :title "You spin my circle right round"
  :size [1000 1000]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
