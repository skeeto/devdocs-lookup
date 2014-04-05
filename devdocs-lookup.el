;;; devdocs-lookup.el --- jump to documentation on devdocs.io -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/devdocs-lookup

;;; Commentary:

;; This package provides an interactive function `devdocs-lookup' to
;; quickly jump to documentation on a particular API at devdocs.io
;; with your browser.

;; http://devdocs.io/

;; Currently supported subjects:

;; Angular.js, Backbone.js, CoffeeScript, C++, C, D3, Ember.jg, Git,
;; HTTP, jQuery, Knockout.js, Less, Lo-Dash, Moment.js, Node.js, PHP,
;; PostgreSQL, Python, Ruby, Redis, SASS, Underscore.js, YII

;; To bypass indicating the subject on each lookup, devdocs-lookup can
;; generate interactive commands for each of the individual subjects
;; by calling `devdocs-setup'.

;; (devdocs-setup)
;; (global-set-key (kbd "C-h C-c") #'devdocs-lookup-c)
;; (global-set-key (kbd "C-h C-p") #'devdocs-lookup-python)

;;; Code:

(require 'url)
(require 'ido)
(require 'json)
(require 'cl-lib)

(defvar devdocs-base-url "http://devdocs.io"
  "Base url for devdocs.io.")

(defvar devdocs-base-index-url "http://docs.devdocs.io"
  "Base url for devdocs.io.")

(defvar devdocs-subjects
  '(("Angular.js" "angular")
    ("Backbone.js" "backbone")
    ("CoffeeScript" "coffeescript")
    ("C++" "cpp")
    ("C" "c")
    ("D3" "d3")
    ("Ember.jg" "ember")
    ("Git" "git")
    ("HTTP" "http")
    ("jQuery" "jquery")
    ("Knockout.js" "knockout")
    ("Less" "less")
    ("Lo-Dash" "lodash")
    ("Moment.js" "moment")
    ("Node.js" "node")
    ("PHP" "php")
    ("PostgreSQL" "postgresql")
    ("Python" "python")
    ("Ruby" "ruby")
    ("Redis" "redis")
    ("SASS" "sass")
    ("Underscore.js" "underscore")
    ("YII" "yii"))
  "List of subjects supported by devdocs.io.")


(defvar devdocs-index (make-hash-table :test 'equal)
  "Hash table for indexes for various subjects.")

(defun devdocs-index (subject &optional callback)
  "Return the devdocs.io index for SUBJECT, optionally async via CALLBACK."
  (cl-declare (special url-http-end-of-headers))
  (let ((index (gethash subject devdocs-index))
        (url (format "%s/%s/index.json" devdocs-base-index-url subject)))
    (cond ((and index callback)
           (funcall callback index))
          ((and index (not callback))
           index)
          ((and (not index) (not callback))
           (with-current-buffer (url-retrieve-synchronously url nil t)
             (goto-char url-http-end-of-headers)
             (setf (gethash subject devdocs-index) (json-read))))
          ((and (not index) callback)
           (url-retrieve
            url
            (lambda (_)
              (goto-char url-http-end-of-headers)
              (setf (gethash subject devdocs-index) (json-read))
              (funcall callback (gethash subject devdocs-index))))))))

(defun devdocs-entries (subject)
  "Return an association list of the entries in SUBJECT."
  (cl-loop for entry across (cdr (assoc 'entries (devdocs-index subject)))
           collect (cons (cdr (assoc 'name entry))
                         (cdr (assoc 'path entry)))))

(defvar devdoc--hist-subjects nil)

(defun devdocs-read-subject ()
  "Interactively ask the user for a subject."
  (let* ((subjects (mapcar #'car devdocs-subjects))
         (hist 'devdoc--hist-subjects)
         (subject (ido-completing-read "Subject: " subjects nil t nil hist)))
    (cadr (assoc subject devdocs-subjects))))

(defun devdocs-read-entry (subject)
  "Interactively ask the user for an entry in SUBJECT."
  (let ((names (mapcar #'car (devdocs-entries subject)))
        (hist (intern (format "devdocs--hist-%s" subject))))
    (unless (boundp hist)
      (set hist nil))
    (ido-completing-read "Entry: " names nil :match nil hist)))

;;;###autoload
(defun devdocs-lookup (subject entry)
  "Visit the documentation for ENTRY from SUBJECT in a browser."
  (interactive
   (let* ((subject (devdocs-read-subject))
          (entry (devdocs-read-entry subject)))
     (list subject entry)))
  (let ((path (cdr (assoc entry (devdocs-entries subject)))))
    (when path
      (browse-url (format "%s/%s/%s" devdocs-base-url subject path))
      :found)))

;;;###autoload
(defun devdocs-setup ()
  "Generate an interactive command for each subject (`devdocs-subjects')."
  (dolist (pair devdocs-subjects)
    (cl-destructuring-bind (name subject) pair
      (let ((symbol (intern (format "devdocs-lookup-%s" subject))))
        (defalias symbol
          (lambda ()
            (interactive)
            (devdocs-lookup subject (devdocs-read-entry subject)))
          (format "Look up documentation for %s on devdocs.io." name))))))

(provide 'devdocs-lookup)

;;; devdocs-lookup.el ends here
