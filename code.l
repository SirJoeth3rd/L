(dec sqr (int) int)
(def sqr (x)
  (return (* x x)))

(dec trippies (int) int)
(def trippies (x)
	(return (* (sqr x) (sqr x))))

(dec main (int char**) int)
(def main (argc argv)
  (return (sqr argc)))
