(ns node
  (:require [io.github.humbleui.ui :as ui]
            [oop :refer [defparent deftype+]]
            [io.github.humbleui.util :as util]
            [io.github.humbleui.debug :as debug]
            [io.github.humbleui.canvas :as canvas]
            [io.github.humbleui.paint :as paint]
            [io.github.humbleui.protocols :as protocols]))


(def ctor-border
  (paint/stroke 0x80FF00FF 4))

(defn parse-element [el]
  (when el
    (if (map? (nth el 1 nil))
      [(nth el 0) (nth el 1) (subvec el 2)]
      [(nth el 0) {} (subvec el 1)])))

(defparent ANode
  [^:mut element
   ^:mut mounted?
   ^:mut bounds
   ^:mut key
   ^:mut dirty?]

  protocols/IComponent
  (-context [_ ctx]
    ctx)

  (-measure [this ctx cs]
    (let [ctx (protocols/-context this ctx)]
      (ui/maybe-render this ctx)
      (protocols/-measure-impl this ctx cs)))

  (-draw [this ctx bounds' viewport canvas]
    (protocols/-set! this :bounds bounds')
    (when (util/irect-intersect bounds' viewport)
      (let [ctx (protocols/-context this ctx)]
        (ui/maybe-render this ctx)
        (protocols/-draw-impl this ctx bounds' viewport canvas)
        (when (and @debug/*outlines? (not (:mounted? this)))
          (canvas/draw-rect canvas (-> ^io.github.humbleui.types.IRect bounds' .toRect (.inflate 4)) ctor-border)
          (protocols/-set! this :mounted? true)))))

  (-event [this ctx event]
    (let [ctx (protocols/-context this ctx)]
      (protocols/-event-impl this ctx event)))

  (-event-impl [this ctx event]
    nil)

  (-should-reconcile? [_this _ctx _element]
    true)

  (-reconcile [this ctx new-element]
    (when (not (identical? (:element this) new-element))
      (let [ctx (protocols/-context this ctx)]
        (protocols/-reconcile-impl this ctx new-element)
        (protocols/-update-element this ctx new-element)
        (protocols/-set! this :element new-element)))
    this)

  (-child-elements [this ctx new-element]
    (let [[_ _ child-els] (parse-element new-element)]
      child-els))

  (-reconcile-impl [this _ctx element]
    (throw (ex-info "Not implemented" {:element (:element this)})))

  (-update-element [this ctx new-el]
    :nop)

  (-unmount [this]
    (protocols/-unmount-impl this))

  (-unmount-impl [_this]))

(defparent AWrapperNode
  "A component that has exactly one child"
  [^:mut child]
  :extends ANode
  protocols/IComponent
  (-measure-impl [this ctx cs]
                 (ui/measure (:child this) ctx cs))

  (-draw-impl [this ctx bounds viewport canvas]
              (ui/draw (:child this) ctx bounds viewport canvas))

  (-event-impl [this ctx event]
               (ui/event (:child this) ctx event))

  (-iterate [this ctx cb]
            (or
             (cb this)
             (let [ctx (protocols/-context this ctx)]
               (ui/iterate (:child this) ctx cb))))

  (-reconcile-impl [this ctx el']
                   (let [child-els (protocols/-child-elements this ctx el')
                         [child']  (ui/reconcile-many ctx [(:child this)] child-els)]
                     (protocols/-set! this :child child')))

  (-unmount [this]
            (ui/unmount (:child this))
            (protocols/-unmount-impl this)))
