#|
Convert geographic coordinates between Latitude/Longitude and UTM (Universal
Transverse Mercator) or UPS (Universal Polar Stereographic).

Copyright 2013-2021 Guillaume Le Vaillant

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

(asdf:defsystem "utm-ups"
  :name "utm-ups"
  :description "Convert coordinates between Latitude/Longitude and UTM or UPS."
  :version "1.1"
  :author "Guillaume Le Vaillant"
  :license "GPL-3"
  :in-order-to ((test-op (test-op "utm-ups/tests")))
  :components ((:file "utm-ups")))

(asdf:defsystem "utm-ups/tests"
  :name "utm-ups/tests"
  :description "Unit tests for utm-ups"
  :version "1.1"
  :author "Guillaume Le Vaillant"
  :license "GPL-3"
  :depends-on ("fiveam" "utm-ups")
  :in-order-to ((test-op (load-op "utm-ups/tests")))
  :perform (test-op (o s)
             (let ((tests (uiop:find-symbol* 'utm-ups-tests :utm-ups-tests)))
               (uiop:symbol-call :fiveam 'run! tests)))
  :components ((:file "utm-ups-tests")))
