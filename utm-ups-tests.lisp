#|
Convert geographic coordinates between Latitude/Longitude and UTM (Universal
Transverse Mercator) or UPS (Universal Polar Stereographic).

Copyright 2013-2020 Guillaume LE VAILLANT

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

(defconstant +max-error-rate+ 0.00001)

(defparameter *utm-data*
  '(((43.642567 -79.38714) (17 630084 4833438) "T")
    ((57.15 -2.15) (30 551427 6334404) "V")
    ((-34.916666 138.6) (-54 280752 6133569) "H")
    ((13.75 100.5) (47 662176 1520582) "P")
    ((-34.583338 -58.36667) (-21 374656 6172313) "H")))

(defparameter *ups-data*
  '(((-88 -1) (nil 1996124.36d0 2222035.45d0) "A")
    ((-87 3) (nil 2017435.44d0 2332687.92d0) "B")
    ((85 10) (t 2096454.16d0 1452981.25d0) "Z")
    ((88 -12) (t 1953829.20d0 1782783.48d0) "Y")))

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
