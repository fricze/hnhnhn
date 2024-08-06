(ns things
  (:require [types]
            [scrollable]
            [clojure.walk :refer [postwalk prewalk]]))

(.field1 scrollable/CNode)

(defprotocol Drawer
  (draw [this]))


(macroexpand
 '(deftype Thing []
    Drawer
    (draw [_]
      (set! types/field2 :drawing!))))


(prewalk
 (fn [token]
   (let [sym? (symbol? token)]
     (if (and sym? (= 'x token))
       'new-sym
       token)))
 '(deftype Thing []
  Drawer
  (draw [_]
    (set! types/field2 :drawing!))))
