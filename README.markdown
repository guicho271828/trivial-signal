# trivial-signal

Trivial-signal is a Common Lisp UNIX signal handling library.

**News** : now equipped with nested signal handling capability &
 multithreads! See `Threading Policy`` below. (Masataro Asai)

**News** : Maintainer has changed. Thanks Fukamachi! (Masataro Asai)

**TODO** : exit handlers (with `atexit`)

**Requirements** :
* [CFFI](http://common-lisp.net/project/cffi/)
* [Bordeaux-threads](http://trac.common-lisp.net/bordeaux-threads/wiki/ApiDocumentation)

**Installation** :
```
cd ~/common-lisp/
git clone https://github.com/guicho2.71828/trivial-signal.git
```

```common-lisp
(ql:quickload :trivial-signal)
```

## Usage : Toplevel Handlers

```common-lisp
(use-package :trivial-signal)
(defun exit-on-signal (signo)
  (format *error-output* "~&received ~A~%" (signal-name signo))
  (sb-ext:exit :code 1 :abort t))
(setf (signal-handler :term) #'exit-on-signal) ;; :term can also be :sigterm or 15

(loop (sleep 3))

;; now run `kill -15 $PID` on the terminal to run `exit-on-signal`
```

Above example shows the usage of **toplevel handlers**.
Toplevel handlers are system wide and (in most cases) static.

## Usage: SIGNAL-HANDLER-BIND

The next important usage of trivial-signal is
to establish handlers dynamically by `signal-handler-bind`.
The scope of this kind of signal handlers are thread-local.

When the main process receives a signal,
handlers are called in the same way as in `handler-bind` : the
top handler in the innermost `signal-handler-bind` is called first.

```COMMON-LISP
(use-package :trivial-signal)

(tagbody
  (signal-handler-bind ((15 (lambda (c) (print :first)))
                        (15 (lambda (c) (print :escaping) (go :escape)))
                        ;; mixing different handlers is ok
                        (2  (lambda (c) (print :escaping) (go :escape)))
                        ;; once the signal is handled, remaining handlers are not called
                        (15 (lambda (c) (print :this-should-not-be-called))))
    (loop (sleep 3))) ;; now send signal 15 from the terminal
  :escape
  (print :success!))

(tagbody
  ;; nested handlers are called from the most recently established ones.
  ;; If the handler declines, the next innermost one is called.
  (signal-handler-bind ((15 (lambda (c) (print :this-should-not-be-called))))
    (signal-handler-bind ((15 (lambda (c) (print :escaping) (go :escape))))
      (signal-handler-bind ((15 (lambda (c) (print :most-recent))))
        (loop (sleep 3)))))
  :escape
  (print :success!))
```

If all these thread-local handlers decline, then the toplevel handlers are called.
If that declines again, then a common-lisp condition `unix-signal` is
signaled in that context.

# Signal Handling Internal

Signals are handled by C-level posix `signal(8)` API
with which we set a low-level handler through CFFI.

## Some Signals may not Work Right

Note that, depending on the lisp implementation, some signals may not be
captured. This is related to the implementations' internal, which may use
signals internally for their own sake (such as thread manipulation).

At least I checked the following signals work:

+ on SBCL x86_64, trivial-signal can capture 4-8, 10, 11, 16, 18, 21, 22, 30, 31
+ on CCL x86_64, trivial-signal can capture 1,2,3,6,8,10,12-14,16-18,21-24,26-29,31

To see which signals works on your environment, see [TESTING.org](https://github.com/guicho271828/trivial-signal/blob/master/TESTING.org)

## Threading Policy

The C-level signal handlers call a lisp function, which interrupts each
thread who has thread-local signal handlers established by
`signal-handler-bind`.

Signals directly sent to each thread might not be captured by `trivial-signal`.
The behavior is currently undefined.
Our advise is that they should be sent to the main process.

This is again related to the internal behavior of the implementations.
For example, on SBCL, signals sent to the main process are not
distributed to each thread. However, CCL seems to distribute the signals.

# API
## sigspec API

Signals can be either specified by its number or by its name.
In `trivial-signal`, the name can be specified with keywords.
Below examples should be sufficient :

+ 15, :term, :sigterm (additionally, constant `+sigterm+` is bound to 15)
+ 2,  :int,  :sigint  (additionally, constant `+sigint+`  is bound to 2 )
+ 24, :xcpu, :sigxcpu (additionally, constant `+sigxcpu+` is bound to 24)

Note that the signal number actually depends on the OS you are using.
Currently we hard-coded the signal number and its names, but in the future
this would be replaced by the information obtained with `cffi-grovel`.

### [Function] signal-name (signo)

This returns the name of `SIGNO` as a keyword.

```common-lisp
(signal-name 15)
;=> :TERM
```

### [Function] signal-number (signame)

This returns the number of `SIGNAME` as an integer.

```common-lisp
(signal-number :term)
;=> 15
```
## Thread-local handlers API

#### [Macro] signal-handler-bind ([(sigspec handler)]* &body forms)

This executes `FORMS` in an environment where signal handler bindings are in effect.

```common-lisp
(signal-handler-bind ((:term (lambda (signo)
                               (declare (ignore signo))
                               (sb-ext:exit :abort t)))
                      (:int  (lambda (signo)
                               (princ (signal-name signo) *error-output*))))
  ;; do something.
  )
```

#### [Function] call-signal-handler-bind (new-signal-handlers fn)

Run FN in a dynamic environment where the signal handler bindings are
in effect. `new-signal-handlers` is a cons tree of ((signo handler ...) ...).
This is rather an internal function which signal-handler-bind expands into.
Use this function when you want to dynamically alter the signal to be captured.

Note that, trivial-signal only considers the first appearance of (signo handlers...)
with the matching signo in the same layer. For example,

    (call-signal-handler-bind
     `((,*signo* ,(lambda (c) (lprint :first))
                 ,(lambda (c) (lprint :escaping) (go :escape))
                 ,(lambda (c) (lprint :this-should-not-be-called))))
     (lambda () ...))

is okay but

    (call-signal-handler-bind
     `((,*signo* ,(lambda (c) (lprint :first)))
       (,*signo* ,(lambda (c) (lprint :escaping) (go :escape)))
       (,*signo* ,(lambda (c) (lprint :this-should-not-be-called))))
     (lambda () ... ))

is incorrect (the 2nd and 3rd handlers are ignored).
If you want to do it, wrap the main code in the `(lambda () ...)`
with another call-signal-handler-bind.
(Also, the macro `signal-handler-bind` automatically solve this.)

#### [Macro] with-signal-handler (signal fn &body forms)

(deprecated)
This executes `FORMS` in an environment where a signal handler `FN` for a signal `SIGNAL` is in effect.


```common-lisp
(with-signal-handler :term (lambda (signo)
                             (declare (ignore signo))
                             (sb-ext:exit :abort t))
  ;; do something.
  )
```

## Toplevel handlers

Toplevel handlers are system wide, global handlers that capture the signals
sent to the main process.
The functionality of the toplevel signal handlers are analogous to `*debugger-hook*`.
When the lisp process receives a signal,
it is handled by these toplevel handlers 
**unless** some nested signal handlers (described later) handles it.

```common-lisp
(use-package :trivial-signal)
(defun exit-on-signal (signo)
  (format *error-output* "~&received ~A~%" (signal-name signo))
  (sb-ext:exit :code 1 :abort t))

(signal-handler :term) ;=> NIL
(setf (signal-handler :term) #'exit-on-signal)
;=> #<FUNCTION (LAMBDA (SIGNO)) {1005764E3B}>

(signal-handler :term)
;=> #<FUNCTION (LAMBDA (SIGNO)) {1005764E3B}>
;   T

;; Removing a signal handler.
(setf (signal-handler :term) nil) ; or: (remove-signal-handler :term) (deprecated)
;=> T

;; Clearing all signal handlers.
(remove-all-signal-handlers)
```

#### [Function] signal-handler (signal)

This returns a signal handler for a signal `SIGNAL`.

`SIGNAL` can be either a keyword or an integer.

```common-lisp
(signal-handler :term)
(signal-handler 15)
(signal-handler +sigterm+)
```

#### [Function] \(setf signal-handler) (fn signal)

This sets a signal handler `FN` for a signal `SIGNAL`.

`FN` must be a function or a symbol of a function name, which takes one
argument as a signal number.
Otherwise `FN` should be `NIL`, indicating the handler should be removed.

```common-lisp
(setf (signal-handler :term)
      #'(lambda (signo)
          (princ (signal-name signo) *error-output*)))
```

#### [Function] remove-signal-handler (signal)

(deprecated) This removes a signal handler from a signal `SIGNAL`.

#### [Function] remove-all-signal-handlers ()

This clears all signal handlers.



## Author

* Eitaro Fukamachi (e.arrows@gmail.com) (author)
* Masataro Asai (guicho2.71828@gmai.com) (maintainer)

## License

Trivial-signal is free and unencumbered software released into the public domain.
