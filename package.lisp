;; GPT3 shell
;; Packaged on 2022-10-12

(in-package :cl-user)

(defpackage :gpt3-shell
  (:use :cl :py4cl)
  (:export
    init
    generate))