(ns handwrite_strokes.core
  (:require [goog.dom :as dom]
            [goog.graphics :as graphics]
            [clojure.string :as string]
            [ajax.core :refer [GET POST]]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce mouse-down (atom false))
(defonce strokes (atom []))
(defonce curr-stroke (atom []))

(defn handle-mouseup [evt]
  (swap! strokes (fn [arr] (conj arr @curr-stroke)))
  (swap! curr-stroke (fn [_] []))
  (swap! mouse-down (fn [_] false)))

(defn handle-mousedown [evt]
  (swap! mouse-down (fn [_] true)))

(defn handle-mousemove [context2d]
  (fn [evt]
    (when @mouse-down
      (when (> (count @curr-stroke) 0) 
        (.beginPath context2d)
        (.moveTo context2d (first (last @curr-stroke)) (second (last @curr-stroke)))
        (.lineTo context2d (.-offsetX evt) (.-offsetY evt))
        (.stroke context2d))
      (swap! curr-stroke (fn [arr] (conj arr [(.-offsetX evt) (.-offsetY evt)]))))))

(defn format-strokes []
  (defn format-stroke [arr] (str "(list " 
                                 (string/join " " (map (fn [[x y]] (str "'(" x " " y ")")) arr))
                                 ")"))
  (str "{" 
       :scale " " (.-width (.getElementById js/document "canvas")) " " 
       :strokes " (list " (string/join " " (map format-stroke @strokes)) ")}"))

(defn call-set-strokes []
  (POST "/write-character" {:format :url :params {:strokes (format-strokes)}}))

(defn clear-strokes [context2d image]
  (fn [_]
    (swap! strokes (fn [_] []))
    (swap! curr-stroke (fn [_] []))
    (.drawImage context2d image 0 0)))

(defn reset-angles []
  (GET "/reset-angles"))

(defn start []
  (let [canvas2d (.getElementById js/document "canvas")
        context2d (.getContext canvas2d "2d")
        image (.getElementById js/document "background")
        go-btn (.getElementById js/document "go-btn")
        clear-btn (.getElementById js/document "clear-btn")
        reset-btn (.getElementById js/document "reset-btn")]
    (set! (.-strokeStyle context2d) "red")
    (.drawImage context2d image 0 0)
    (set! (.-onmousemove canvas2d) (handle-mousemove context2d))
    (set! (.-onmousedown canvas2d) handle-mousedown)
    (set! (.-onmouseup canvas2d) handle-mouseup)
    (set! (.-onclick go-btn) call-set-strokes)
    (set! (.-onclick clear-btn) (clear-strokes context2d image))
    (set! (.-onclick reset-btn) reset-angles)))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

(set! (.-onload js/window) start)
