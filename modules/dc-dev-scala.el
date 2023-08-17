;; -*- lexical-binding: t; -*-
;;
;; Copyright © 2021 David Wilson
;; Copyright © 2014-2022 Henrik Lissner
;; Copyright © 2023 David Conner
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(setup (:pkg scala-mode))

;; use sbt through docker-sbt: https://github.com/sbt/docker-sbt
;; - docker pull sbtscala/scala-sbt:$TAG
;; - where TAG=${JVM_TYPE}-${JVM_VERSION}_${SDT_VERSION}_${SCALA_VERSION}
;; - and where JVM_TYPE={eclipse-temurin,graalvm-ce}
;; e.g. eclipse-temurin-focal-17.0.5_8_1.9.3_2.13.11

;; sbt-mode is for comint buffers, .sbt files should still use scala
(setup (:pkg sbt-mode))

(provide 'dc-dev-scala)
