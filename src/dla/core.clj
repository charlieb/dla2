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

(defn intersections [c p1 p2]
  ;; http://mathworld.wolfram.com/Circle-LineIntersection.html
  (let [pc1 (c/sub p1 (:c c)) ; move reference frame to center of circle
        pc2 (c/sub p2 (:c c))
        d (c/sub pc2 pc1)
        dr (c/mag d)
        D (- (* (:x pc1) (:y pc2)) (* (:x pc2) (:y pc1)))
        ; some convenience variables
        dx (:x d)
        dy (:y d)
        dr2 (Math/pow dr 2)
        r2 (Math/pow (:r c) 2)
        D2 (Math/pow D 2)
        sgn #(if (< % 0) -1 1)
        disc (- (* r2 dr2) D2) ; discriminant
        sqrt-disc (Math/sqrt disc)
        ]
    (when (< 0 disc) ; there is an intersection
      (if (= 0 disc) ; it's a tangent i.e. a single intersection point
        [(c/add (:c c) {:x (/ (* D dy) dr2) :y (/ (* (- D) dx) dr2)})]
        [(c/add (:c c) {:x (/ (+ (* D dy)     (* (sgn dy) dx sqrt-disc)) dr2)
                        :y (/ (+ (* (- D) dx) (* (Math/abs dy) sqrt-disc)) dr2)})
         (c/add (:c c) {:x (/ (- (* D dy)     (* (sgn dy) dx sqrt-disc)) dr2)
                        :y (/ (- (* (- D) dx) (* (Math/abs dy) sqrt-disc)) dr2)})]))))


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
(defn stick-particle [tree]
  (let [sz 500 -sz -500
        w sz -w -sz 
        h sz -h -sz
        touch-dist-sq (* 2 radius 2 radius)] 
    (loop [p [(q/random -w w) (q/random -h h)]]
      (let [near (kd/nearest-neighbor tree p 2)]
        (if (and (= 2 (count near))
                 (< (:dist-squared (first near)) touch-dist-sq)
                 (< (:dist-squared (second near)) touch-dist-sq))
          (recur [(q/random -w w) (q/random -h h)])
          (if (< (:dist-squared (first near)) touch-dist-sq)
            [p (first near)]
            (recur 
              (if (> (:dist-squared (first near)) (* 10 touch-dist-sq))
                [(q/random -w w) (q/random -h h)]
                [(+ (p 0) (q/random -2 2)) (+ (p 1) (q/random -2 2))]))))))))

(defn aggregate [nparts]
  (first 
    (iterate-times (- nparts 1)
                   (fn [[cs tree]] 
                     (let [[p hit] (stick-particle tree)
                           c (assoc (mk-circle (p 0) (p 1) radius) 
                                    :id (+ 1 (:id (last cs)))
                                    :link (:id (meta hit))
                                    :link-dist link-length
                                    :layer (+ 1 (:layer (meta hit))))]
                       [(conj cs c)
                        (kd/insert tree (with-meta (to-v2 (:c c)) c))]))
                   (let [c0 (center)
                         c1 (assoc c0 :c (c/add (:c c0) {:x    0 :y  150}) :id 1)
                         c2 (assoc c0 :c (c/add (:c c0) {:x  150 :y    0}) :id 2)
                         c3 (assoc c0 :c (c/add (:c c0) {:x    0 :y -150}) :id 3)
                         c4 (assoc c0 :c (c/add (:c c0) {:x -150 :y    0}) :id 4)
                         cs [c0]]; c1 c2 c3 c4]]
                     [cs
                      (kd/build-tree (vec (map #(with-meta (to-v2 (:c %)) %) cs)))]))))


;;;;;;;;;; Constraints ;;;;;;;;;;;;;;

(defn settled? [cs1 cs2 max-delta]
  (not-any? #(> % max-delta)
            (map #(c/mag (c/sub (:c %1) (:c %2))) cs1 cs2)))

(defn settled-av? [cs1 cs2 max-delta]
  (> (* max-delta (count cs1))
     (reduce + (map #(c/mag (c/sub (:c %1) (:c %2))) cs1 cs2))))

(defn settle [cs f]
  (loop [cs cs
         cs-next (f cs)
         i 0]
    ;(print i '-)
    (if (or (> i 1000) (settled? cs cs-next 0.0001))
      cs-next
      (recur cs-next (f cs-next) (inc i)))))

(defn exclude [circles factor]
  (let [tree (kd/build-tree (map #(with-meta (to-v2 (:c %)) %) circles))]
    (cons
      (first circles)
      (map (fn [c] 
              (let [c2 (meta (second (kd/nearest-neighbor tree (to-v2 (:c c)) 2)))]
                (assoc c 
                       :c (c/dist> (:c c) (:c c2)
                                   (+ (:r c) (:r c2)) ; desired-dist
                                   factor))))
           (rest circles)))))

(defn average-pull [p ps dist] "Sums the offsets of ps from p whos magnitude exceeds dist"
  (let [far-ps (->> ps
                    (map #(c/sub % p))
                    (filter #(> (c/mag %) dist)))]
    ;(doseq [fp far-ps] (println 'near (:x fp) (:y fp)))
    (if (> (count far-ps) 0)
      (let [res (c/div (reduce #(c/add %1 %2) {:x 0 :y 0} far-ps)
                       (count far-ps))]
        ;       (println 'av (:x res) (:y res))
        res)
      {:x 0 :y 0})))

(defn stick [circles factor] "Circles must stay a fixed distance from their :link"
  (sort-by :id
           (map 
             (fn [c]
               ;(if (empty? (:back-links c)) c
               (let [cop (average-pull (:c c) (map (comp :c circles) ; center of pull == cop
                                                   (if (:link c)
                                                     (cons (:link c) (:back-links c))
                                                     (:back-links c)))
                                       (:link-dist c))]
                 (if (< (c/mag cop) 1E-6)
                   c
                   ; FIXME it's fairly likely that the average of the pulling positions could
                   ; be within range even if the other points aren't -> no movement
                   (assoc c :c (c/dist< (:c c) (c/add (:c c) cop) (:link-dist c) factor)))))
               ;)
   ;          circles
    (shuffle circles))
   ))

(defn limit-dist1 "Applies distance limit on first link that is too long"
  [c circles factor] 
  (if (nil? (:link c))
    c
  (let [nears (->> (if (:link c) (cons (:link c) (:back-links c)) (:back-links c))
                   (map #(nth circles %))
                   (filter #(> (c/mag (c/sub (:c c) (:c %))) (:link-dist c))))]
    (if (empty? nears) 
      c
      (assoc c :c (c/dist< (:c c) (:c (rand-nth nears)) (:link-dist c) factor)))))) 

(defn limit-dist [circles factor]
  (sort-by :id
  (map (fn [c] (first (settle [c] #(list (limit-dist1 (first %) circles factor)))))
       (shuffle circles))))

;;;;;;;;;; Circle movement  ;;;;;;;;;;;;;;

(defn find-leaf-ids [circles]
  (s/difference (set (map :id circles))
                (set (map :link circles))))

(defn pull-leaves [circles]
  (let [leaves (find-leaf-ids circles)]
    (cons (first circles)
          (map #(if (contains? leaves (:id %))
                  (assoc %
                         :c (c/add (:c %) (c/mul (c/norm (:c %)) 25.0)))
                  %)
               (rest circles)))))

(defn expand [circles]
  (cons (first circles)
    (map #(assoc % :c (c/add (:c %) (c/mul (c/norm (:c %)) 0.5)))
         (rest circles))))

(defn expand-contract [state]
  (let [period 30
        phase (if (< (mod (:frame state) (* period 2)) period) 'expansion 'contraction)
        fr (mod (:frame state) period)

        ; Refers to expansion or contraction phase exclusion change
        exc-min 0.0
        exc-max (* 2. radius) 
        r (+ (:radius state)
               (/ (- (if (= phase 'expansion) exc-max exc-min) (:radius state))
                  (- period fr)))
                     

        ; Refers to expansion or contraction phase link-len change
        lin-min (+ (* 2 exc-min) 3.)
        lin-max (* 2 exc-max) ;link-length

        lin (+ (:link-len state)
               (/ (- (if (= phase 'expansion) lin-max lin-min) (:link-len state))
                  (- period fr)))

        _ (println 'r (:r (second (:circles state))) r)
        _ (println 'li (:link-dist (second (:circles state))) lin)
        
        cs (map #(assoc % :r r :link-dist lin) (:circles state))

        _ (println phase 'r (:r (second cs)) r)
        _ (println phase 'lin (:link-dist (second cs)) lin)
        _ (println (> lin (* 2 r)))
        ]
    (assoc state
           :radius r
           :link-len lin
           :circles (vec (settle cs (if (= phase 'expansion) #(exclude % 0.2) #(exclude (stick (vec %) 0.5) 0.2)))))))


;;;;;;;;;; QUIL ;;;;;;;;;;;;;;
(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  (println 'setup)
  (let [circles (vec (add-back-links (aggregate 100)))]
    ;(prntcs circles)
;    (println (count circles) (count circles-changed))
;    (println ((vec (concat circles circles-changed)) 1001) )
    {:circles circles; (vec (concat circles circles-changed))
     :lit-layer 0
     :radius radius
     :link-len link-length
     :max-layer (apply max (map :layer circles))
     :frame 0 }
    ))


(defn update-state [state]
  (println (:frame state))
  (assoc state ;(expand-contract state)
         :circles (vec (settle (vec (pull-leaves (:circles state))) 
                               #(limit-dist (vec %) 1.0)))
         :frame (inc (:frame state))
         :lit-layer (mod (inc (:lit-layer state)) (:max-layer state))))
             ;
              

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

        ;       (when (> (:id c) 1000) 
        ;         (q/stroke 150 150 50))
        ;       ;(q/stroke 150 150 (* 10 (:layer c)))
        ;       (when (= (:layer c) (:lit-layer state))
        (if (some #(= % (:id c)) (find-leaf-ids (:circles state)))
               ;(q/ellipse (:x (:c c)) (:y (:c c)) (* 2. (:r c)) (* 2. (:r c)))
               (q/ellipse (:x (:c c)) (:y (:c c)) (* 1. (:r c)) (* 1. (:r c)))

               )
        ;)
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
