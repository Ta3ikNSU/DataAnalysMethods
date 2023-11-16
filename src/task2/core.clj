(ns task2.core)
; ----------------------------------------------------------------------------
(defn trapezoid [f a b]
  (* (* (+ (f a) (f b)) (- b a)) 0.5))

; 3x^6+38x^5+27x^4-14x^3-19x^2+39x-5
(defn polynomial [x]
  (+ (* 3 (Math/pow x 6))
     (* 38 (Math/pow x 5))
     (* 27 (Math/pow x 4))
     (* -14 (Math/pow x 3))
     (* -19 (Math/pow x 2))
     (* 39 x)
     -5))

; #2.1
; a,b - границы левая и правая соответственно
; h - длина шага
(defn integral
  ([f a b h]
   (if (< a b)
     (+
       (trapezoid f (- b h) b)
       (integral f a (- b h) h))
     0))

  ([f b h]
   (integral f 0 b h))
  )
(def m-integral
  (memoize
    (fn [f a b h]
      (if (< a b)
        (+
          (trapezoid f (- b h) b)
          (m-integral f a (- b h) h))
        0)))
  )

(defn -main [& args]
  (time (m-integral polynomial -50 50 0.05))
  (time (m-integral polynomial -50 50 0.05))
  (time (m-integral polynomial -50 60 0.05))
  )

; #2.2 Обещаем вычислить потом, когда у нас попросят данные

