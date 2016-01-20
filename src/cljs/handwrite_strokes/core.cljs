(ns handwrite_strokes.core
  (:require [goog.dom :as dom]
            [goog.graphics :as graphics]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce mouse-down (atom false))
(defonce strokes [])

(defn handle-mouseup [evt]
  (swap! mouse-down (fn [_] false)))

(defn handle-mousedown [evt]
  (swap! mouse-down (fn [_] true)))

(defn handle-mousemove [evt]
  (when @mouse-down 
    (.log js/console (str "(" (.-offsetX evt) "," (.-offsetY evt) ")"))))

(defn start []
  (let [canvas2d (.getElementById js/document "canvas")
        context2d (.getContext canvas2d "2d")
        image (.getElementById js/document "background")]
    (.drawImage context2d image 0 0)
    (set! (.-onmousemove canvas2d) handle-mousemove)
    (set! (.-onmousedown canvas2d) handle-mousedown)
    (set! (.-onmouseup canvas2d) handle-mouseup)))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  (start))

(set! (.-onload js/window) start)
