(ns calc-angle.core
  (:require [clojure.string :as string]
            [clojure.core.reducers :as r]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.util.response :refer [redirect]]
            [compojure.core :refer [GET POST defroutes]]
            [compojure.handler :refer [site]]
            [compojure.route :refer [resources not-found]]
            [clojure.java.io :as io])
  (:import  [jssc SerialPort SerialPortList])
  (:gen-class))

(declare radian-to-degree)

(def PI Math/PI)

;; 
;; Write field is x (-10, 10) y (22, 32)
;;
(def min-y 18)
(def xfield-size 20)
(def yfield-size 10)

(def base-high 25.4)
(def first-arm 20.9)
(def second-arm 24.0)
(def offset-last 5.2)

(defn to-int [val]
  (int (+ 0.5 val)))

(defn normalize-angle [ang]
  (loop [a ang]
    (if (and (not (neg? a)) (< a (* 2 PI)))
      a
      (recur (+ a (if (neg? a) (* 2 PI) (* -2 PI)))))))

(defn calc-cos-beta [r z]
  (let [z2 (* z z)
        r2 (* r r)
        a (/ 
            (+ (- (* first-arm first-arm) (* second-arm second-arm)) z2 r2) 
            (* 2 first-arm))
        sqrt (Math/sqrt 
               (- 
                 (+ z2 r2) 
                 (* a a)))]
    (/ (- (* a r) (* z sqrt)) (+ z2 r2))))

(defn radian-to-degree [ang]
  (/ (* 180. ang) PI))

(defn degree-to-radian [ang]
  (/ (* PI ang) 180.))

;;
;; beta is the angle of first-arm relative to x positive
;; alpha is the angle of x positive relative to second-arm
;;
(defn calc-alpha-beta [r z]
  (let [cos-beta (calc-cos-beta r z)
        sin-beta (Math/sqrt (- 1 (* cos-beta cos-beta)))
        cos-gamma (/ (- (* first-arm sin-beta) z) second-arm)]
    (list (- (/ PI 2.) (Math/acos cos-gamma)) (Math/acos cos-beta))))

(defn display-alpha-beta [alpha beta]
  (println (str "first-arm*cos(beta)=" (* first-arm (Math/cos beta))))
  (println (str "second-arm*cos(alpha)=" (* second-arm (Math/cos alpha))))
  (println (str "first-arm*sin(beta)=" (* first-arm (Math/sin beta))))
  (println (str "second-arm*sin(alpha)=" (* second-arm (Math/sin alpha)))))

;;
;; r is the distance to origin in the x-y plane
;; theta is the angle of the projection in x-y plane relative to y positive
;;
(defn calc-r-theta [x y]
  (let [norm (Math/sqrt (+ (* x x) (* y y)))
        r (Math/sqrt (- (+ (* x x) (* y y)) (* offset-last offset-last)))]
    (list r (radian-to-degree (- (Math/asin (/ (- x) r)) (Math/asin (/ offset-last norm)))))))

(defn point-dist [pt1 pt2]
  (Math/sqrt (+ (* 
                  (- (first pt2) (first pt1)) 
                  (- (first pt2) (first pt1))) 
                (* 
                  (- (second pt2) (second pt1)) 
                  (- (second pt2) (second pt1))))))

(defn mix-pts [pt1 ratio1 pt2 ratio2]
  (list (+ 
          (* ratio1 (first pt1))
          (* ratio2 (first pt2)))
        (+
          (* ratio1 (second pt1))
          (* ratio2 (second pt2)))))

(defn fill-intra-stroke-gap [stroke]
  (defn append-point [stroke-arr new-tail]
    (let [dist (point-dist (last stroke-arr) new-tail)]
      (if (> dist 15)
        (let [iter-pts (rest (concat (range 0 1 (/ 15 dist)) '(1)))
              last-pt  (last stroke-arr)] 
          (reduce (fn [arr ptv] (conj arr (mix-pts last-pt (- 1 ptv) new-tail ptv))) stroke-arr iter-pts))
        (conj stroke-arr new-tail))))
  (loop [left-stroke (rest stroke)
         last-point  (first stroke)
         reslt-stroke [last-point]]
    (if (zero? (count left-stroke))
      reslt-stroke
      (recur (rest left-stroke) (first left-stroke) (append-point reslt-stroke (first left-stroke))))))

(defn fill-inter-stroke-gap [strokes]
  (defn fill-gap [start-pt end-pt]
    (let [dist (point-dist start-pt end-pt)]
      (if (> dist 5)
        (reduce (fn [arr ptv] (conj arr (mix-pts start-pt (- 1 ptv) end-pt ptv))) [] (concat (range 0 1 (/ 5 dist)) '(1)))
        [start-pt end-pt])))
  (loop [left-strokes (rest strokes)
         last-stroke  (first strokes)
         reslt-strokes [[(first (first strokes))]]]
    (if (zero? (count left-strokes))
      (conj reslt-strokes [(last last-stroke)])
      (recur 
        (rest left-strokes) 
        (first left-strokes) 
        (conj reslt-strokes (fill-gap (last last-stroke) (first (first left-strokes))))))))

(defn process-stroke [stroke xscale yscale default-z]
  (map (fn [[x y]] (list (- (* xfield-size (/ x xscale)) (/ xfield-size 2.)) (+ (* yfield-size (/ (- yscale y) yscale)) min-y) default-z)) stroke))

(defn dump-strokes [strokes title]
  (println (str title " >>>"))
  (doseq [stroke strokes]
    (println (str "   " (doall stroke)))))

(defn calc-xyz [alpha beta theta]
  (let [alpha-rad (degree-to-radian alpha)
        beta-rad  (degree-to-radian beta)
        theta-rad (degree-to-radian theta)
        r (+ (* (Math/cos beta-rad) first-arm) (* (Math/cos alpha-rad) second-arm))
        y (+ (* (Math/sin beta-rad) first-arm) (* (Math/sin (- alpha-rad)) second-arm))]
    (list (* r (Math/cos theta-rad)) y (* r (Math/sin (- theta-rad))))))

(def ^:dynamic *serial-conn* nil)
(def dump-degrees 0)

(defn set-count [cnt]
  (let [count-str (format "s=%d," cnt)]
    (println (str "--> " count-str))
    (.writeString *serial-conn* count-str)
    (println (str "<-- " (char (aget (.readBytes *serial-conn* 1) 0))))))

(defn kick-action []
  (let [draw-str "d"]
    (println (str "--> " draw-str))
    (.writeString *serial-conn* draw-str)
    (println (str "<-- " (char (aget (.readBytes *serial-conn* 1) 0))))))

(defn set-angle [base-angle first-angle second-angle last-angle]
  (let [serial-str (format "a=%.3f,b=%.3f,c=%.3f,d=%.3f," base-angle first-angle second-angle last-angle)]
    (println (str "--> " serial-str))
    (.writeString *serial-conn* serial-str)
    (let [echo (char (aget (.readBytes *serial-conn* 1) 0))]
      (when-not (= echo \0) (println (str echo " angle out of range - " serial-str)))
      (println (str "<-- " echo)))))

(defn xyz2angles-list [xyz-list]
  (let [r-theta-list (map #(apply calc-r-theta %) (map (fn [x] (take 2 x)) xyz-list))
        alpha-beta-radian-list (map #(calc-alpha-beta (first %1) %2) r-theta-list (map (fn [x] (last x)) xyz-list))
        alpha-beta-degree-list (map (fn [[alpha beta]] (list (radian-to-degree alpha) (radian-to-degree beta))) alpha-beta-radian-list)]
    (map (fn [[_ theta] [alpha beta]] (list (- theta) (- 90. beta) (+ alpha beta) alpha)) r-theta-list alpha-beta-degree-list)))

(defn calc-angle-seq [input-str]
  (println input-str)
  (let [input (load-string input-str)
        strokes (:strokes input)
        xscale (:xscale input)
        yscale (:yscale input)
        gaps (fill-inter-stroke-gap strokes)
        dense-strokes (map fill-intra-stroke-gap strokes)
        ;; gaps has one element than dense-strokes
        strokes-plus-gaps (concat (interleave (map (fn [x] (process-stroke x xscale yscale 6.0)) gaps)
                                              (map (fn [x] (process-stroke x xscale yscale 3.2)) dense-strokes))
                                  (list (process-stroke (last gaps) xscale yscale 6.0)))
        strokes-z (reduce concat strokes-plus-gaps)
        ]
    (xyz2angles-list strokes-z)))
;        r-theta-list (map #(apply calc-r-theta %) (map (fn [x] (take 2 x)) strokes-z))
;        alpha-beta-radian-list (map #(calc-alpha-beta (first %1) %2) r-theta-list (map (fn [x] (last x)) strokes-z))
;        alpha-beta-degree-list (map (fn [[alpha beta]] (list (radian-to-degree alpha) (radian-to-degree beta))) alpha-beta-radian-list)]
;    (map (fn [[_ theta] [alpha beta]] (list (- theta) (- 90. beta) (+ alpha beta) alpha)) r-theta-list alpha-beta-degree-list)))

(def prefix-action (list '(15.4, 27.5, 9) '(15.4, 27.5, 4.5) '(15.4, 27.5, 6.0) '(13.5, 27.5, 6.0) '(17.3, 27.5, 6.0) '(15.4, 27.5, 6.0) '(15.4, 26.5, 6.0) '(15.4, 28.5, 6.0) '(15.4, 27.5, 9)))

(defn write-strokes [angles-seq]
  (with-open [fout (io/writer "dump_degrees.h" :append true)]
    (.write fout (str (count angles-seq) "., "))
    (doseq [[a b c d] angles-seq]
      (.write fout (format "%f, %f, %f, %f, " a b c d))))
  "OK")

(defroutes app
           (GET "/" [] (redirect "index.html"))
           (GET "/set-angle" [a b c d] (if (and a b c d)
                                         (write-strokes [(map #(Float. %) (list a b c d))])
                                         "/set-angle?a=%1&b=%2&c=%3&d=%3"))
           (GET "/reset-angles" _ (write-strokes [[0. 0. 0. 0.]]))
           (GET "/set-xyz" [x y z] (if (and x y z)
                                     (write-strokes (xyz2angles-list [(map #(Float. %) (list x y z))]))
                                     "/set-xyz?x=%1&y=%2&z=%3"))
           (GET "/write-prefix" _ (write-strokes (xyz2angles-list prefix-action)))
           (POST "/write-character" [strokes] (write-strokes (calc-angle-seq strokes)))
           (resources "/")
           (not-found "<h1>not found</h1>"))

(def entry (site app))

(defn init
  "one time initialization work"
  []
  (let [ports (. SerialPortList getPortNames)
        ;myport "/dev/tty.usbmodem1421"]
        myport "/dev/tty.usbserial-A402XH6Q"]
    (if (and (pos? (alength ports)) (contains? (set ports) myport))
      (alter-var-root #'*serial-conn* 
                      (fn [_] 
                        (doto (SerialPort. myport)
                          (.openPort)
                          (.setParams 9600 8 1 0)
                          ((fn [_] (Thread/sleep 1000))))))
      (println "There is no serial connection!"))))

(defn destroy 
  "release acquired resources before service stops"
  []
  (.closePort *serial-conn*))

