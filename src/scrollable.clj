(ns scrollable
  (:require [clojure.math :as math]
            [io.github.humbleui.ui :as ui]
            [oop :refer [deftype+]]
            [node :refer [ANode AWrapperNode]]
            [io.github.humbleui.util :as util]
            [io.github.humbleui.canvas :as canvas]
            [io.github.humbleui.protocols :as protocols]
            [io.github.humbleui.signal :as signal]
            [io.github.humbleui.window :as window]))


(deftype+ HScrollable [^:mut clip?
                       ^:mut offset-px
                       ^:mut offset
                       ^:mut child-size]

  :extends AWrapperNode

  (-measure-impl [_ ctx cs]
                 (let [child-cs (assoc cs :height Integer/MAX_VALUE)]
                   (set! child-size (protocols/-measure (.child AWrapperNode) ctx child-cs))
                   (util/ipoint
                    (:width child-size)
                    (min
                     (:height child-size)
                     (:height cs)))))

  (-draw-impl [_ ctx bounds viewport canvas]
              (set! child-size (protocols/-measure (.child AWrapperNode) ctx (util/ipoint (:width bounds) Integer/MAX_VALUE)))
              (set! offset-px (util/clamp
                               (ui/scaled (or @offset 0))
                               0
                               (Math/abs (- (:width child-size) (:width bounds)))))

              (canvas/with-canvas canvas
                (when clip?
                  (canvas/clip-rect canvas bounds))

                (let [child-bounds (-> bounds
                                       (update :x - offset-px)
                                       (update :x math/round)
                                       (assoc :width (:width child-size)))]
                  (ui/draw (.child AWrapperNode) ctx child-bounds (util/irect-intersect viewport bounds) canvas))))

  (-event-impl [_ ctx event]
               (case (:event event)
                 :mouse-scroll
                 (when (util/rect-contains? (.bounds ANode) (util/ipoint (:x event) (:y event)))
                   (or
                    (ui/event (.child AWrapperNode) ctx event)
                    (let [offset-px' (-> offset-px
                                         (- (:delta-x event))
                                         (util/clamp 0 (- (:width child-size) (:width (.bounds ANode)))))]

                      (when (not= offset-px offset-px')
                        (set! offset-px offset-px')
                        (reset! offset (ui/descaled (math/round offset-px')))
                        (window/request-frame (:window ctx))))))

                 :mouse-button
                 (when (util/rect-contains? (.bounds ANode) (util/ipoint (:x event) (:y event)))
                   (ui/event (.child AWrapperNode) ctx event))

                 #_:else
                 (ui/event (.child AWrapperNode) ctx event)))

  (-update-element [_this _ctx new-element]
                   (let [opts (ui/parse-opts (.element ANode))]
                     (set! clip? (:clip? opts true))
                     (when-some [offset' (:offset opts)]
                       (set! offset offset')))))


(defn hscrollable
  ([child]
   (hscrollable {} child))
  ([opts child]
   (map->HScrollable
    {:offset-px 0
     :offset    (or
                 (:offset opts)
                 (signal/signal 0))})))
