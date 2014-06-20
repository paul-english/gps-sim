;; These come from the pseudo code in "Numerical Analysis" by Kincaid
;; & Cheney
;;
;; Many of these algorithms haven't been coded in an idiomatic way
;; and may not perform well in Clojure. For example, many of the
;; books algorithms assume a mutable environment, and make plenty of
;; use of variable reassignment. To do this in clojure I often drop
;; to loop/recur, which may not be that great of a solution.

;; Page 14: Horner's method / nested multiplication
;;
;; p(x) = a_n x^n + a_{n-1} x^{n-1} + ... + a_2 x^2 + a_1 x + a_0
;; 2x^2 + 3x + 4
;;
;; p <- a_n
;; for k = n-1, n-2, ..., 0 do
;;   p <- xp + a_k
;; end
(defn horners [coefficients x]
  (reduce (fn [acc val] (+ (* x acc) val)) 1 coefficients))

;; Page 36: Floating point error, calculating your machine epsilon
;;
;; input s <- 1.0
;; for k = 1, 2, ..., 100 do
;;   s <- 0.5s
;;   t <- s + 1.0
;;   if t <= 1.0 then
;;     s <- 2.0s
;;     output k - 1, s
;;     stop
;;   end if
;; end
(defn machine-epsilon []
  (loop [k 1 s 1.0]
    (if (or (<= (+ s 1.0) 1.0)
            (> k 100))
      [(dec k), (* 2.0 s)]
      (recur (inc k) (* 0.5 s)))))

;; Page 59: Bisection algorithm
;;
;; input a, b, M, delta, epsilon
;; u <- f(a)
;; v <- f(b)
;; e <- b - a
;; output a, b, u, v
;; if sign (u) = sign (v) then stop
;; for k = 1, 2, ..., M do
;;   e <- e/2
;;   c <- a + e
;;   w <- f(c)
;;   output k, c, w, e
;;   if |e| < delta or |w| < epsilon then stop
;;   if sign(w) != sign(u) then
;;     b <- c
;;     v <- w
;;   else
;;     a <- c
;;     u <- w
;;   end if
;; end
(defn sign [a]
  (if (>= a 0) 1 -1))

(defn bisection [f a b M delta epsilon]
  (let [u (f a)
        v (f b)
        e (- b a)]
    (if (= (sign u) (sign v))
      [a, b, u, v]
      (loop [k 1 a a b b
             u u v v e e]
        (println "-" a b u v)
        (let [e (/ e 2)
              c (+ a e)
              w (f c)]
          (println "---" e c w)
          (if (or (< (Math/abs (float e)) delta)
                  (< (Math/abs (float w)) epsilon)
                  (> k M))
            [k, c, w, e]
            (if (not (= (sign w) (sign u)))
              (recur (inc k) a c u w e)
              (recur (inc k) c b w v e))))))))

;; e^x = sin(x)
(defn f [x] (- (Math/exp x) (Math/sin x)))
;; (bisection f -4 -3 20 0.00001 0.00001) => [16 -208605/65536 3.4543204472242683E-6 1/65536]

;; Page 64: Newton's Algorithm
;;
;; input x_0, M, delta, epsilon
;; v <- f(x_0)
;; output 0, x_0, v
;; if |v| < epsilon then stop
;; for k = 1, 2, ..., M do
;;   x_1 <- x_0 - v / f^\prime(x_0)
;;   v <- f(x_1)
;;   output k, x_1, v
;;   if |x_1 - x_0| < delta or |v| < epsilon then stop
;;   x_0 <- x_1
;; end

;; Page 69: Implicit Newton's Algorithm for functions of the form
;; f(x,y)
;;
;; input x <- 0; y <- 1; h <- 0.1; M <- 100; N <- 4
;; output 0, x, y, G(x,y)
;; for i = 1, 2, ..., M do
;;   x <- x + h
;;   for j = 1, 2, ..., N do
;;     y <- y - G(x,y) / \partial G/\partial y (x, y)
;;   end
;;   output i, x, y, G(x,y)
;; end

;; Page 76: Secant Algorithm
;; Doesn't require the calculation of a derivative
;;
;; input a, b, M, delta, epsilon
;; u <- f(a)
;; v <- f(b)
;; output 0, a, u
;; output 1, b, v
;; for k = 2, 3, ..., M do
;;   if |u| < |v| then
;;     a <-> b
;;     u <-> v
;;   end if
;;   s <- (b - a) / (v - u)
;;   a <- b
;;   u <- v
;;   b <- b - vs
;;   v <- f(b)
;;   output k, b, v
;;   if |v| < epsilon or |b - a| < delta then stop
;; end

;; Page 83: Fixed point iteration of F(x) = 4 + 1/3 sin(2x)
;;
;; input x <- 4; M <- 20
;; for k = 1, 2, ..., M do
;;   x <- 4 + 1/3 sin(2x)
;;   output k, x
;; end

;; Page 92: Horner's Algorithm (another one)

;; Page 93: Complete Horner's Algorithm

;; Page 94: Using horner's method as a way to use Newton's method

;; Page 98: Bairstow's method, for calculating potentially imaginary
;; roots of a polynomial
