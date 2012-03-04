;code from http://bestinclass.wordpress.com/2009/09/24/chaos-theory-vs-clojure/
;in a more functional style

(ns ikedafunc
  (:import [java.awt Color Graphics RenderingHints]
        [java.awt.image BufferedImage]
        [java.awt.event MouseListener]
        [javax.swing JFrame JPanel])
  (:gen-class
    :main -main))

;(import (java.awt Color Graphics RenderingHints)
;        (java.awt.image BufferedImage)
;        (java.awt.event MouseListener)
;        (javax.swing JFrame JPanel))

(def c-min             [-20 -25])
(def c-max             [ 25  30])
(def dim-axis          (vec (map #(Math/abs (- %1 %2)) c-min c-max)))
(def dim-screen        [800 700])
(def iterations        100)
(def running           (atom true))

(def axis-seqs [(vec (take (dim-screen 0) (iterate #(+ (/ (dim-axis 0) (dim-screen 0)) %) (c-min 0))))
                (vec (take (dim-screen 1) (iterate #(+ (/ (dim-axis 1)  (dim-screen 1))  %) (c-min 1))))])

(defn ikeda [x y u]
  (iterate (fn [[xn yn]]
             (let [tn (- 0.4 (/ 6 (+ 1 (* xn xn) (* yn yn))))]
               [(inc (* u (- (* xn (Math/cos tn))
                             (* yn (Math/sin tn)))))
                (* u (+ (* xn (Math/sin tn))
                        (* yn (Math/cos tn))))]))
           [x y]))

(defn ikedas [u cnt]    
  (take cnt (repeatedly 
             #(let [x (nth (axis-seqs 0) (rand-int (dim-screen 0)))
                    y (nth (axis-seqs 1) (rand-int (dim-screen 1)))]
                (ikeda x y u)))))

(defn screen-pt [coordinate]
  (map #(* (/ (- %1 %2) (- %3 %2)) %4) coordinate c-min c-max dim-screen))

(defn draw-ikeda-map [#^Graphics canvas n u iks]
  (when (zero? n)
    (doto canvas
      (.setColor Color/WHITE)
      (.fillRect 0 0 (first dim-screen) (last dim-screen))))
  (let [point-color (int (+ 155 (* (/ 100  iterations) (- n iterations))))]
    (.setColor canvas (Color. point-color point-color point-color))
    (doseq [coords iks]
      (let [[x1 y1]  (screen-pt (first coords))
            [x2 y2]  (screen-pt (second coords))]
        (.drawLine canvas x1 y1 x2 y2)))
    (doto canvas
      (.setColor Color/WHITE)
      (.fillRect 0 3 100 30)
      (.setColor Color/BLACK)
      (.drawRect -5 2 101 31))
    (.drawString canvas (format "u: %5f2" u) 3 15)
    (.drawString canvas (format "iterations: %d"   n) 3 28)))

(defn render [g {:keys [n u iks]}]
  (when iks
    (let [img (BufferedImage. (first dim-screen) (last dim-screen) BufferedImage/TYPE_INT_ARGB)
          bg  (.getGraphics img)]
      (.setRenderingHint bg RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
      (draw-ikeda-map bg n u iks)
      (.drawImage g img 0 0 nil)
      (.dispose bg))))

(defn animate-1 [surface amodel u]
  (loop [n 0 iks (ikedas u 200)]
    (when (< n iterations)
      (reset! amodel {:n n :u u :iks iks})
      (.repaint surface)
      (Thread/sleep 20)
      (recur (inc n) (map next iks)))))

(defn animate-loop [surface amodel u]
  (when @running
    (animate-1 surface amodel u)
    (recur surface amodel (+ u (- 0.05 (/ (rand-int 100) 1000))))))

(defn -main []
(let [amodel (atom nil)
      frame    (JFrame. "Ikeda map")
      panel    (doto (proxy [JPanel] [] (paint [g] (render g @amodel))))]
  (doto frame (.add panel) .pack (.setSize (dim-screen 0) (dim-screen 1)) .show
              (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))
  (future (animate-loop panel amodel 0.902)))
  )