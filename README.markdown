# trivial-signal

Trivial-signal is a Common Lisp library provides unix signal handling feature.

## Usage

```common-lisp
(use-package :trivial-signal)

(defun exit-on-signal (signo)
  (format *error-output* "~&received ~A~%" (signal-name signo))
  (sb-ext:exit :code 1 :abort t))

(signal-handler :term)
;=> NIL
;   NIL

;; Setting a signal handler.
(setf (signal-handler :term) #'exit-on-signal)
;=> #<FUNCTION (LAMBDA (SIGNO)) {1005764E3B}>

(signal-handler :term)
;=> #<FUNCTION (LAMBDA (SIGNO)) {1005764E3B}>
;   T

;; Removing a signal handler.
(remove-signal-handler :term)
;=> T

;; Clearing all signal handlers.
(remove-all-signal-handlers)
```

## Requirements

* [CFFI](http://common-lisp.net/project/cffi/)

## Installation

```
cd ~/common-lisp/
git clone https://github.com/fukamachi/trivial-signal.git
```

```common-lisp
(ql:quickload :trivial-signal)
```

## Functions

### signal-handler (signal)

This returns a signal handler for a signal `SIGNAL`.

`SIGNAL` can be either a keyword or an integer.

```common-lisp
(signal-handler :term)
(signal-handler 15)
(signal-handler +sigterm+)
```

### (setf signal-handler) (fn signal)

This sets a signal handler `FN` for a signal `SIGNAL`.

`FN` must be a function or a symbol of a function name, which takes one argument as a signal number.

```common-lisp
(setf (signal-handler :term)
      #'(lambda (signo)
          (princ (signal-name signo) *error-output*)))
```

### remove-signal-handler (signal)

This removes a signal handler from a signal `SIGNAL`.

### remove-all-signal-handlers ()

This clears all signal handlers.

### signal-name (signo)

This returns the name of `SIGNO` as a keyword.

```common-lisp
(signal-name 15)
;=> :TERM
```

### signal-number (signame)

This returns the number of `SIGNAME` as an integer.

```common-lisp
(signal-number :term)
;=> 15
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## License

Trivial-signal is free and unencumbered software released into the public domain.
