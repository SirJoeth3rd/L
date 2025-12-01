(dec sqr (int) int)

(def sqr (x)
  (return (* x x)))

(dec main (int char**) int)
(def main (argc argv)
  (return (sqr argc)))
