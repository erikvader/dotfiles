(ql:quickload "adopt")
(ql:quickload "iterate")
(ql:quickload "alexandria")
(use-package :iterate)

(defparameter *option-num*
  (adopt:make-option
   'num
   :long "number"
   :short #\n
   :parameter "N"
   :help "The number of lines to output"
   :initial-value 1
   :key #'parse-integer
   :reduce #'adopt:last))

(defparameter *option-state-file*
  (adopt:make-option
   'state-file
   :long "state-file"
   :short #\f
   :parameter "PATH"
   :help "The file to save state between invocations"
   :reduce #'adopt:last))

(defparameter *ui*
  (adopt:make-interface
   :name "cycle-shuf"
   :summary "shuf which cycles its input randomly"
   :usage "[OPTIONS]"
   :help (format nil "Reads lines from standard input, shuffles them, and prints N of ~
                      them to standard output. The non-printed lines are saved into the ~
                      specified state file and are used the next time. If the state is ~
                      not long enough, then it will be refilled from standard input. ~
                      The number of lines given to standard input should be at least N, ~
                      otherwise exit with non-zero exit status.")
   :contents (list
              *option-num*
              *option-state-file*)))

(defun read-state (state-file)
  (with-open-file (in state-file :if-does-not-exist nil)
    (when in
      (read in))))

(defun write-state (state-file state)
  (with-open-file (out state-file :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
    (prin1 state out)))

(defun read-stdin ()
  (uiop:slurp-stream-lines *standard-input*))

(defun shuf (num-lines state-file)
  (let ((state (read-state state-file)))
    (when (< (length state) num-lines)
      (setq state (append state (alexandria:shuffle (read-stdin)))))

    (when (< (length state) num-lines)
      (error "Not enough lines"))

    (iter (for _ from 1 to num-lines)
          (format t "~a~%" (pop state)))

    (write-state state-file state)))

(defun main (args)
  (setf *random-state* (make-random-state t))
  (handler-case
      (multiple-value-bind (pos kv) (adopt:parse-options *ui* (cdr args))
        (declare (ignore pos))
        (let ((state-file (gethash 'state-file kv))
              (number (gethash 'num kv)))
          (unless (and state-file number)
            (format t "Missing a required argument~%")
            (adopt:print-help-and-exit *ui*))
          (shuf number state-file)))
    (error (c)
      (adopt:print-error-and-exit c))))
