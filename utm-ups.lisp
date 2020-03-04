#|
Convert geographic coordinates between Latitude/Longitude and UTM (Universal
Transverse Mercator) or UPS (Universal Polar Stereographic).

Copyright 2013-2020 Guillaume Le Vaillant

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

(defpackage :utm-ups
  (:use :common-lisp)
  (:export #:*ellipsoid*
           #:band
           #:ellipsoids
           #:format-lat/lon
           #:format-utm/ups
           #:lat/lon->ups
           #:lat/lon->utm
           #:lat/lon->utm/ups
           #:ups-band
           #:ups->lat/lon
           #:utm-band
           #:utm-zone
           #:utm->lat/lon
           #:utm/ups->lat/lon))

(in-package :utm-ups)


(defun deg->rad (angle)
  "Convert ANGLE from radians to degrees."
  (check-type angle real)
  (* angle (/ pi 180.0d0)))

(defun rad->deg (angle)
  "Convert ANGLE from degrees to radians."
  (check-type angle real)
  (* angle (/ 180.0d0 pi)))

(defparameter *ellipsoids*
  '(("Airy" . (6377563d0 0.00667054d0))
    ("Australian National" . (6378160d0 0.006694542d0))
    ("Bessel 1841" . (6377397d0 0.006674372d0))
    ("Bessel 1841 (Nambia)" . (6377484d0 0.006674372d0))
    ("Clarke 1866" . (6378206d0 0.006768658d0))
    ("Clarke 1880" . (6378249d0 0.006803511d0))
    ("Everest" . (6377276d0 0.006637847d0))
    ("Fischer 1960 (Mercury)" . (6378166d0 0.006693422d0))
    ("Fischer 1968" . (6378150d0 0.006693422d0))
    ("GRS 1967" . (6378160d0 0.006694605d0))
    ("GRS 1980" . (6378137d0 0.00669438d0))
    ("Helmert 1906" . (6378200d0 0.006693422d0))
    ("Hough" . (6378270d0 0.00672267d0))
    ("International" . (6378388d0 0.00672267d0))
    ("Krassovsky" . (6378245d0 0.006693422d0))
    ("Modified Airy" . (6377340d0 0.00667054d0))
    ("Modified Everest" . (6377304d0 0.006637847d0))
    ("Modified Fischer 1960" . (6378155d0 0.006693422d0))
    ("South American 1969" . (6378160d0 0.006694542d0))
    ("WGS 60" . (6378165d0 0.006693422d0))
    ("WGS 66" . (6378145d0 0.006694542d0))
    ("WGS 72" . (6378135d0 0.006694318d0))
    ("WGS 84" . (6378137d0 0.00669438d0)))
  "Ellipsoids used to modelize the earth, defined by an equatorial radius and
a squared eccentricity.")

(defparameter *ellipsoid* "WGS 84"
  "Ellipsoid to use by default.")

(defun ellipsoid-r (ellipsoid)
  "Return the equatorial radius of an ELLIPSOID."
  (check-type ellipsoid string)
  (or (cadr (assoc ellipsoid *ellipsoids* :test #'string-equal))
      (error "Unknown ellipsoid: ~a" ellipsoid)))

(defun ellipsoid-e2 (ellipsoid)
  "Return the squared eccentricity of an ELLIPSOID."
  (check-type ellipsoid string)
  (or (caddr (assoc ellipsoid *ellipsoids* :test #'string-equal))
      (error "Unknown ellipsoid: ~a" ellipsoid)))

(defun ellipsoids ()
  "Return the list of supported ellipsoids."
  (mapcar #'car *ellipsoids*))

(defun utm-zone (latitude longitude)
  "Return the UTM zone number for the given LATITUDE and LONGITUDE."
  (check-type latitude (real -80 84))
  (check-type longitude (real -180 (180)))
  (cond
    ;; Special zone for Norway
    ((and (<= 56 latitude 64) (<= 3 longitude 12)) 32)
    ;; Special zones for Svalbard
    ((and (<= 72 latitude 84) (<= 0 longitude 9)) 31)
    ((and (<= 72 latitude 84) (<= 9 longitude 21)) 33)
    ((and (<= 72 latitude 84) (<= 21 longitude 33)) 35)
    ((and (<= 72 latitude 84) (<= 33 longitude 42)) 37)
    ;; Regular zones
    (t (+ 31 (floor longitude 6)))))

(defun utm-band (latitude)
  "Return the UTM band for the given LATITUDE."
  (check-type latitude (real -80 84))
  (cond
    ((< latitude -72) "C")
    ((< latitude -64) "D")
    ((< latitude -56) "E")
    ((< latitude -48) "F")
    ((< latitude -40) "G")
    ((< latitude -32) "H")
    ((< latitude -24) "J")
    ((< latitude -16) "K")
    ((< latitude -8) "L")
    ((< latitude 0) "M")
    ((< latitude 8) "N")
    ((< latitude 16) "P")
    ((< latitude 24) "Q")
    ((< latitude 32) "R")
    ((< latitude 40) "S")
    ((< latitude 48) "T")
    ((< latitude 56) "U")
    ((< latitude 64) "V")
    ((< latitude 72) "W")
    (t "X")))

(defun ups-band (latitude longitude)
  "Return the UPS band for the given LATITUDE and LONGITUDE."
  (check-type latitude (or (real -90 -60) (real 60 90)))
  (check-type longitude (real -180 (180)))
  (if (minusp latitude)
      (if (minusp longitude) "A" "B")
      (if (minusp longitude) "Y" "Z")))

(defun band (latitude longitude)
  "Return the UTM or UPS band for the given LATITUDE and LONGITUDE."
  (check-type latitude (real -90 90))
  (check-type longitude (real -180 (180)))
  (if (<= -80 latitude 84)
      (utm-band latitude)
      (ups-band latitude longitude)))

(defun lat/lon->utm (latitude longitude &optional zone)
  "Return the UTM zone, easting and northing for the given LATITUDE and
LONGITUDE. The second returned value is the UTM band. The ZONE parameter can
be used to force in which UTM zone the coordinates will be computed."
  (check-type latitude (real -80 84))
  (check-type longitude (real -180 (180)))
  (check-type zone (or null (integer -60 -1) (integer 1 60)))
  (let* ((longitude (if (= longitude 180) -180 longitude))
         (zone (if zone (abs zone) (utm-zone latitude longitude)))
         (band (utm-band latitude))
         (k0 0.9996d0)
         (lat (deg->rad latitude))
         (lon (deg->rad longitude))
         (lon-origin (deg->rad (- (* zone 6) 183)))
         (r (ellipsoid-r *ellipsoid*))
         (e2 (ellipsoid-e2 *ellipsoid*))
         (e2p (/ e2 (- 1 e2)))
         (e4 (* e2 e2))
         (e6 (* e2 e4))
         (sin-lat (sin lat))
         (cos-lat (cos lat))
         (tan-lat (tan lat))
         (n (/ r (sqrt (- 1 (* e2 sin-lat sin-lat)))))
         (ta (* tan-lat tan-lat))
         (c (* e2p cos-lat cos-lat))
         (a (* cos-lat (- lon lon-origin)))
         (m (* r (+ (* lat
                       (+ 1 (* -1/4 e2) (* -3/64 e4) (* -5/256 e6)))
                    (* (sin (* 2 lat))
                       (+ (* -3/8 e2) (* -3/32 e4) (* -45/1024 e6)))
                    (* (sin (* 4 lat))
                       (+ (* 15/256 e4) (* 45/1024 e6)))
                    (* (sin (* 6 lat))
                       (* -35/3072 e6)))))
         (a2 (* a a))
         (a3 (* a a2))
         (a4 (* a2 a2))
         (a5 (* a2 a3))
         (a6 (* a3 a3))
         (ta2 (* ta ta))
         (c2 (* c c))
         (x (* k0 n
               (+ a
                  (* 1/6 a3 (+ 1 (- ta) c))
                  (* 1/120 a5 (+ 5 (* -18 ta) ta2 (* 72 c) (* -58 e2p))))))
         (y (* k0
               (+ m
                  (* n tan-lat
                     (+ (* 1/2 a2)
                        (* 1/24 a4 (+ 5 (- ta) (* 9 c) (* 4 c2)))
                        (* 1/720 a6 (+ 61 (* -58 ta) ta2 (* 600 c)
                                       (* -330 e2p))))))))
         (zone (if (minusp latitude) (- zone) zone))
         (easting (+ x 500000))
         (northing (if (minusp latitude) (+ y 10000000) y)))
    (values (list zone easting northing) band)))

(defun utm->lat/lon (zone easting northing)
  "Return the latitude and longitude for the given UTM ZONE, EASTING and
NORTHING."
  (check-type zone (or (integer -60 -1) (integer 1 60)))
  (check-type easting (real 0 1000000))
  (check-type northing (real 0 10000000))
  (let* ((longitude-origine (- (* (abs zone) 6) 183))
         (x (- easting 500000))
         (y (if (minusp zone) (- northing 10000000) northing))
         (k0 0.9996d0)
         (r (ellipsoid-r *ellipsoid*))
         (e2 (ellipsoid-e2 *ellipsoid*))
         (f1 (/ (- 1 (sqrt (- 1 e2)))
                (+ 1 (sqrt (- 1 e2)))))
         (e2p (/ e2 (- 1 e2)))
         (e4 (* e2 e2))
         (e6 (* e2 e4))
         (f2 (* f1 f1))
         (f3 (* f1 f2))
         (f4 (* f2 f2))
         (m (/ y k0))
         (mu (/ m (* r (+ 1 (* -1/4 e2) (* -3/64 e4) (* -5/256 e6)))))
         (phi1 (+ mu
                  (* (sin (* 2 mu)) (+ (* 3/2 f1) (* -27/32 f3)))
                  (* (sin (* 4 mu)) (+ (* 21/16 f2) (* -55/32 f4)))
                  (* (sin (* 6 mu)) (* 151/96 f3))))
         (sin-phi1 (sin phi1))
         (cos-phi1 (cos phi1))
         (tan-phi1 (tan phi1))
         (e-sin-phi1-2 (* e2 sin-phi1 sin-phi1))
         (n1 (/ r (sqrt (- 1 e-sin-phi1-2))))
         (t1 (* tan-phi1 tan-phi1))
         (c1 (* e2p cos-phi1 cos-phi1))
         (r1 (* r (- 1 e2) (expt (- 1 e-sin-phi1-2) -3/2)))
         (d (/ x n1 k0))
         (d2 (* d d))
         (d3 (* d d2))
         (d4 (* d2 d2))
         (d5 (* d2 d3))
         (d6 (* d3 d3))
         (c1-2 (* c1 c1))
         (t1-2 (* t1 t1))
         (lat (- phi1
                 (* n1 tan-phi1 (/ r1)
                    (+ (* 1/2 d2)
                       (* -1/24 d4 (+ 5 (* 3 t1) (* 10 c1) (* -4 c1-2)
                                      (* -9 e2p)))
                       (* 1/720 d6 (+ 61 (* 90 t1) (* 298 c1) (* 45 t1-2)
                                      (* -252 e2p) (* -3 C1-2)))))))
         (lon (/ (+ d
                    (* -1/6 (+ 1 (* 2 t1) c1) d3)
                    (* 1/120 d5 (+ 5 (* -2 c1) (* 28 t1) (* -3 c1-2)
                                   (* 8 e2p) (* 24 t1-2))))
                 cos-phi1))
         (latitude (rad->deg lat))
         (longitude (+ (rad->deg lon) longitude-origine)))
    (list latitude longitude)))

(defun lat/lon->ups (latitude longitude)
  "Return the UPS hemisphere, easting and northing for the given LATITUDE and
LONGITUDE. The second returned value is the UPS band."
  (check-type latitude (or (real -90 -60) (real 60 90)))
  (check-type longitude (real -180 (180)))
  (let* ((band (ups-band latitude longitude))
         (k0 0.994d0)
         (fn 2000000)
         (fe 2000000)
         (lat (abs (deg->rad latitude)))
         (lon (deg->rad longitude))
         (r (ellipsoid-r *ellipsoid*))
         (e2 (ellipsoid-e2 *ellipsoid*))
         (e (sqrt e2))
         (sin-lat (sin lat))
         (esin-lat (* e sin-lat))
         (sin-lon (sin lon))
         (cos-lon (cos lon))
         (c0 (* 2 r
                (expt (- 1 e2) -1/2)
                (expt (/ (- 1 e) (+ 1 e)) (/ e 2))))
         (ta (* (expt (/ (+ 1 esin-lat) (- 1 esin-lat)) (/ e 2))
                (tan (- (/ pi 4) (/ lat 2)))))
         (ra (* k0 c0 ta))
         (x (* ra sin-lon))
         (y (* ra cos-lon))
         (northp (>= latitude 0))
         (easting (+ fe x))
         (northing (if northp (- fn y) (+ fn y))))
    (values (list northp easting northing) band)))

(defun ups->lat/lon (northp easting northing)
  "Return the LATITUDE and LONGITUDE for the given UPS hemisphere, EASTING and
NORTHING."
  (check-type northp boolean)
  (check-type easting (real 0 4000000))
  (check-type northing (real 0 4000000))
  (let* ((k0 0.994d0)
         (fe 2000000)
         (fn 2000000)
         (de (- easting fe))
         (dn (- northing fn))
         (r (ellipsoid-r *ellipsoid*))
         (e2 (ellipsoid-e2 *ellipsoid*))
         (e (sqrt e2))
         (e4 (* e2 e2))
         (e6 (* e2 e4))
         (e8 (* e4 e4))
         (c0 (* 2 r
                (expt (- 1 e2) -1/2)
                (expt (/ (- 1 e) (+ 1 e)) (/ e 2))))
         (a (+ (* 1/2 e2) (* 5/24 e4) (* 1/12 e6) (* 13/360 e8)))
         (b (+ (* 7/48 e4) (* 29/240 e6) (* 811/11520 e8)))
         (c (+ (* 7/120 e6) (* 81/1120 e8)))
         (d (* 4279/161280 e8))
         (lon (cond
                ((zerop dn) (/ pi 2))
                ((zerop de) 0)
                (northp (let ((l (atan (/ de (- dn)))))
                          (cond
                            ((< dn 0) l)
                            ((> de 0) (+ l pi))
                            (t (- l pi)))))
                (t (let ((l (atan (/ de dn))))
                     (cond
                       ((> dn 0) l)
                       ((> de 0) (+ l pi))
                       (t (- l pi)))))))
         (ra (cond
               ((zerop dn) (abs de))
               ((zerop de) (abs dn))
               (t (abs (/ de (sin lon))))))
         (ta (/ ra k0 c0))
         (ksi (- (/ pi 2) (* 2 (atan ta))))
         (lat (if (and (zerop de) (zerop dn))
                  (/ pi 2)
                  (+ ksi
                     (* a (sin (* 2 ksi)))
                     (* b (sin (* 4 ksi)))
                     (* c (sin (* 6 ksi)))
                     (* d (sin (* 8 ksi))))))
         (longitude (rad->deg lon))
         (latitude (rad->deg (if northp lat (- lat)))))
    (list latitude longitude)))

(defun lat/lon->utm/ups (latitude longitude &optional zone)
  "Return the UTM or UPS zone, easting and northing for the given LATITUDE and
LONGITUDE. The second returned value is the UTM or UPS band. The ZONE parameter
can be used to force in which UTM zone the coordinates will be computed."
  (check-type latitude (real -90 90))
  (check-type longitude (real -180 (180)))
  (if (<= -80 latitude 84)
      (lat/lon->utm latitude longitude zone)
      (lat/lon->ups latitude longitude)))

(defun utm/ups->lat/lon (zone easting northing)
  "Return the latitude and longitude for the given UTM or UPS ZONE, EASTING and
NORTHING."
  (check-type zone (or boolean (integer -60 -1) (integer 1 60)))
  (check-type easting (real 0 4000000))
  (check-type northing (real 0 10000000))
  (if (integerp zone)
      (utm->lat/lon zone easting northing)
      (ups->lat/lon zone easting northing)))

(defun format-utm/ups (zone easting northing &optional band)
  "Return UTM or UPS coordinates as a string. If BAND is not specified, it will
be set to \"+\" for northern zones and to \"-\" for southern zones."
  (check-type zone (or boolean (integer -60 -1) (integer 1 60)))
  (check-type easting (real 0 4000000))
  (check-type northing (real 0 10000000))
  (check-type band (or null string))
  (if (integerp zone)
      (let ((band (or band (if (minusp zone) "-" "+"))))
        (format nil "~2,'0d~a ~7,'0d ~7,'0d"
                (abs zone) band (round easting) (round northing)))
      (let ((band (or band
                      (if zone
                          (if (< easting 2000000) "Y" "Z")
                          (if (< easting 2000000) "A" "B")))))
        (format nil "00~a ~7,'0d ~7,'0d"
                band (round easting) (round northing)))))

(defun format-lat/lon (latitude longitude &optional decimal)
  "Return the coordinates as a string. If DECIMAL is not NIL, decimal notation
will be used for angles, otherwise d°m's\" notation will be used."
  (check-type latitude (real -90 90))
  (check-type longitude (real -180 (180)))
  (check-type decimal boolean)
  (flet ((dms (angle)
           (multiple-value-bind (d r) (floor (abs angle))
             (multiple-value-bind (m r) (floor (* 60 r))
               (let ((s (/ (floor (* 600 r)) 10)))
                 (format nil "~d°~2,'0d'~4,1,,,'0f\"" d m s))))))
    (if decimal
        (format nil "~,6f ~,6f" latitude longitude)
        (format nil "~a~a ~a~a"
                (dms latitude) (if (minusp latitude) "S" "N")
                (dms longitude) (if (minusp longitude) "W" "E")))))
