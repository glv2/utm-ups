#|
Convert geographic coordinates between Latitude/Longitude and UTM (Universal
Transverse Mercator) or UPS (Universal Polar Stereographic).

Copyright 2013-2021 Guillaume LE VAILLANT

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(defpackage :utm-ups-tests
  (:use :common-lisp :fiveam :utm-ups))

(in-package :utm-ups-tests)


(def-suite utm-ups-tests)

(in-suite utm-ups-tests)

(defconstant +max-error-rate+ 0.0001)

(defparameter *utm-data*
  '(((43.642567 -79.38714) (17 630084 4833438) "T")
    ((57.15 -2.15) (30 551427 6334404) "V")
    ((-34.916666 138.6) (-54 280752 6133569) "H")
    ((13.75 100.5) (47 662176 1520582) "P")
    ((-34.583338 -58.36667) (-21 374656 6172313) "H")
    ((40.433333d0 -3.7d0) (30 440627 4476089) "T")
    ((-77.846389d0 166.668333d0) (-58 539204 1358215) "C")
    ((36.165926d0 -86.723285d0) (16 524887 4002387) "S")
    ((-33.014673d0 116.230695d0) (-50 428145 6346823) "H")
    ((-55.315349d0 -68.794971d0) (-19 513012 3870096) "F")
    ((35.205535d0 136.56579d0) (53 642528 3896959) "S")
    ((-20.166667d0 70.116667d0) (-42 616691 7769683) "K")
    ((52.5d0 104.333333d0) (48 454744 5816860) "U")
    ((-6.266667d0 106.8d0) (-48 699138 9306975) "M")
    ((-26.2d0 28.066667d0) (-35 606571 7101729) "J")
    ((17.983333d0 -76.816667d0) (18 307638 1989283) "Q")
    ((-4.3d0 15.283333d0) (-33 531439 9524706) "M")
    ((3.133333d0 101.7d0) (47 800107 346717) "N")
    ((51.533333d0 -0.083333d0) (30 702286 5713169) "U")
    ((31.2d0 -106.416667d0) (13 365033 3452631) "R")))

(defparameter *ups-data*
  '(((-88 -1) (nil 1996124 2222035) "A")
    ((-87 3) (nil 2017435 2332688) "B")
    ((85 10) (t 2096454 1452981) "Z")
    ((88 -12) (t 1953829 1782783) "Y")
    ((86 50) (t 2340329 1714429) "Z")
    ((86 -50) (t 1659670 1714429) "Y")
    ((-86 50) (nil 2340329 2285570) "B")
    ((-86 -50) (nil 1659670 2285570) "A")
    ((87 130) (t 2255203 2214141) "Z")
    ((87 -130) (t 1744796 2214141) "Y")
    ((-87 130) (nil 2255203 1785858) "B")
    ((-87 -130) (nil 1744796 1785858) "A")))

(defun close-enough (x y)
  (<= (- 1 +max-error-rate+) (abs (/ x y)) (+ 1 +max-error-rate+)))

(test utm-zone
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (let ((zone (abs (first (second data)))))
              (is (= zone (utm-zone latitude longitude))))))
        *utm-data*))

(test utm-band
  (mapc (lambda (data)
          (let ((latitude (first (first data)))
                (band (third data)))
            (is (string= band (utm-band latitude)))))
        *utm-data*))

(test ups-band
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (let ((band (third data)))
              (is (string= band (ups-band latitude longitude))))))
        *ups-data*))

(test band
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (let ((band (third data)))
              (is (string= band (band latitude longitude))))))
        *utm-data*)
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (let ((band (third data)))
              (is (string= band (band latitude longitude))))))
        *ups-data*))

(test lat/lon->utm
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (destructuring-bind (zone easting northing) (second data)
              (let ((band (third data)))
                (multiple-value-bind (utm b) (lat/lon->utm latitude longitude)
                  (destructuring-bind (z e n) utm
                    (is (= zone z))
                    (is (string= band b))
                    (is (close-enough easting e))
                    (is (close-enough northing n))))))))
        *utm-data*))

(test utm->lat/lon
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (destructuring-bind (zone easting northing) (second data)
              (destructuring-bind (lat lon) (utm->lat/lon zone easting northing)
                (is (close-enough latitude lat))
                (is (close-enough longitude lon))))))
        *utm-data*))

(test lat/lon->ups
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (destructuring-bind (zone easting northing) (second data)
              (let ((band (third data)))
                (multiple-value-bind (ups b) (lat/lon->ups latitude longitude)
                  (destructuring-bind (z e n) ups
                    (is (eql zone z))
                    (is (string= band b))
                    (is (close-enough easting e))
                    (is (close-enough northing n))))))))
        *ups-data*))

(test ups->lat/lon
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (destructuring-bind (zone easting northing) (second data)
              (destructuring-bind (lat lon) (ups->lat/lon zone easting northing)
                (is (close-enough latitude lat))
                (is (close-enough longitude lon))))))
        *ups-data*))

(test lat/lon->utm/ups
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (destructuring-bind (zone easting northing) (second data)
              (let ((band (third data)))
                (multiple-value-bind (utm b) (lat/lon->utm latitude longitude)
                  (destructuring-bind (z e n) utm
                    (is (= zone z))
                    (is (string= band b))
                    (is (close-enough easting e))
                    (is (close-enough northing n))))))))
        *utm-data*)
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (destructuring-bind (zone easting northing) (second data)
              (let ((band (third data)))
                (multiple-value-bind (ups b) (lat/lon->ups latitude longitude)
                  (destructuring-bind (z e n) ups
                    (is (eql zone z))
                    (is (string= band b))
                    (is (close-enough easting e))
                    (is (close-enough northing n))))))))
        *ups-data*))

(test utm/ups->lat/lon
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (destructuring-bind (zone easting northing) (second data)
              (destructuring-bind (lat lon) (utm->lat/lon zone easting northing)
                (is (close-enough latitude lat))
                (is (close-enough longitude lon))))))
        *utm-data*)
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (destructuring-bind (zone easting northing) (second data)
              (destructuring-bind (lat lon) (ups->lat/lon zone easting northing)
                (is (close-enough latitude lat))
                (is (close-enough longitude lon))))))
        *ups-data*))

(test format/parse-utm/ups
  (mapc (lambda (data)
          (destructuring-bind (zone easting northing) (second data)
            (destructuring-bind (z e n)
                (parse-utm/ups (format-utm/ups zone easting northing))
              (unless (eql zone z)
                (format t "~a ~a ~a~%" zone easting northing))
              (is (eql zone z))
              (is (close-enough easting e))
              (is (close-enough northing n)))))
        *utm-data*)
  (mapc (lambda (data)
          (destructuring-bind (zone easting northing) (second data)
            (destructuring-bind (z e n)
                (parse-utm/ups (format-utm/ups zone easting northing))
              (is (eql zone z))
              (is (close-enough easting e))
              (is (close-enough northing n)))))
        *ups-data*))

(test format-parse-lat/lon
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (destructuring-bind (lat lon)
                (parse-lat/lon (format-lat/lon latitude longitude t))
              (is (close-enough latitude lat))
              (is (close-enough longitude lon)))))
        *utm-data*)
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (destructuring-bind (lat lon)
                (parse-lat/lon (format-lat/lon latitude longitude t))
              (is (close-enough latitude lat))
              (is (close-enough longitude lon)))))
        *ups-data*))
