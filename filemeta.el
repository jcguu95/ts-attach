;;; ts-attach.el -mode -*- coding: utf-8; lexical-binding: t; -*-

(require 'f)
;; (require 'filemeta-dired)
;; (require 'filemeta-stats)
;; (require 'filemeta-test)

(defvar ts-attach:root-dir "~/.ts-attach")
(defvar ts-attach:unhealthy-dir (f-join ts-attach:root-dir "unhealthy"))
(defvar ts-attach:db-name "db")
(defvar ts-attach:large-file-threshold 500000000) ;; ~500mb?

(loop for dir in (list ts-attach:root-dir
                       ts-attach:unhealthy-dir)
      do (mkdir dir t))

;;;

(defun ts-attach:dir-for-ts (ts)
  "Expect TS to be a time string in the format like
  \"20200228-105245\" (. Return the directory for it."
  (f-join ts-attach:root-dir ts))

(defun ts-attach:db-for-ts (ts)
  (f-join (ts-attach:dir-for-ts ts)
          ts-attach:db-name))

(defun ts-attach:make-db-for-ts! (ts)
  "Make a database for the timestring TS."
  (if (f-exists-p (ts-attach:dir-for-ts ts))
      (error "Directory for (TS=%s) exists." ts)
    (let ((dir (ts-attach:dir-for-ts ts))
          (db (ts-attach:db-for-ts ts)))
      (mkdir dir t)
      (f-write-text "()" 'utf-8 db))))

(defun ts-attach:read-db-for-ts (ts)
  "Read the database for the timestring TS."
  (with-temp-buffer
    (insert-file-contents (ts-attach:db-for-ts ts))
    (read (buffer-string))))

(defun ts-attach:db-ill-p (ts)
  "Return T if database for TS is unhealthy; orelse, return NIL."
  (condition-case nil
      (progn (ts-attach:read-db-for-ts ts) nil)
    (error t)))

(defun ts-attach:ensure-ts-db (ts)
  "When database for TS is unhealthy, move it to another place,
and create a fresh new database for TS again."
  (when (ts-attach:db-ill-p ts)
    (progn
      (message "Warning: database for %s is unhealthy thus moved
      to patient room. A fresh database will be created to
      replace it." ts)
      (let ((waiting-room (f-join ts-attach:unhealthy-dir
                                  (concat "moved-here-at-"
                                          (ts-format "%Y%m%d-%H%M%S"))))
            (old-data (ts-attach:dir-for-ts ts)))
        (mkdir waiting-room t)
        (ignore-errors                  ;; FIXME bad practice
          (rename-file old-data (f-join waiting-room
                                        (f-filename old-data)))))
      (ts-attach:make-db-for-ts! ts))))

(defun ts-attach:write-attachment! (x ts)
  "Overwrite X to the db of TS. Expect X to be READably printed."
  ;; TODO Reformat the plist to be written by a variant of
  ;; #'lispy-multiline before writing.
  (ts-attach:ensure-ts-db ts)
  (f-write-text (prin1-to-string x)
                'utf-8 (ts-attach:db-for-ts ts)))

(defun ts-attach:+tag! (tag ts)
  "Expect TAG to be a symbol. Add TAG to the database of TS."
  ;; - test -
  ;; (loop for tag in '(math nerd techie emacs)
  ;;       do (ts-attach:+tag! tag "20210303-101010"))
  (unless (symbolp tag)
    (error "TAG must be a symbol."))
  (ts-attach:ensure-ts-db ts)
  (let ((plist (ts-attach:read-db-for-ts ts)))
    (ts-attach:write-attachment!
     (plist-put plist
                :tag (sort (-uniq (cons tag (plist-get plist :tag)))
                           #'string<))
     ts)))

;; (defun filemeta:-tag! (tag file)
;;   "Expect TAG to be a symbol. Remove all tags that equal to TAG
;;   in the filemeta of FILE,and write the updated filemeta to the
;;   hash-file for FILE."
;;   (unless (symbolp tag)
;;     (error "TAG must be a symbol."))
;;   (flet ((sort+uniq (symbols)
;;                     (sort (-uniq symbols) #'string<)))
;;     (let* ((plist (filemeta:attachment<-file file))
;;            (plist_ (plist-put plist ;; TODO fix bad updating method..
;;                               :tag (sort+uniq
;;                                     (-remove (lambda (x) (equal x tag))
;;                                              (plist-get plist :tag))))))
;;       (filemeta:write-attachment! plist_ file))))

;; (defun filemeta:update-file-history! (file)
;;   "Check and update the history of the hash of the FILE. Expect
;; FILE to be a regular file."
;;   (let* ((plist (filemeta:attachment<-file file))
;;          (hist (plist-get plist :history))
;;          (rel-path (filemeta:rel-path<-file file))
;;          (last-rel-path (-last-item (-last-item hist))))
;;     ;; Update history slot accordingly.
;;     (if (equal rel-path last-rel-path)
;;         ;; Then only need to update time.
;;         (setf hist (append (-drop-last 1 hist)
;;                            `(,(list (ts-format) rel-path))))
;;       ;; Otherwise, add a new entry to history.
;;       (setf hist (append hist
;;                          `(,(list (ts-format) rel-path)))))
;;     ;; Update plist and write to database.
;;     (plist-put! plist :history hist)
;;     (filemeta:write-attachment! plist file)))
