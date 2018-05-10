;;; devdocs-lookup.el --- jump to documentation on devdocs.io -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/devdocs-lookup

;;; Commentary:

;; This package provides an interactive function `devdocs-lookup' to
;; quickly jump to documentation on a particular API at devdocs.io
;; with your browser.

;; https://devdocs.io/

;; Currently supported subjects:

;; Angular.js, Apache HTTP Server, Backbone.js, Bower, C, Chai,
;; Clojure, CoffeeScript, Cordova, C++, CSS, D3.js, Django, Dojo, DOM,
;; DOM Events, Drupal, Elixir, Ember.js, Express, Flow, Git, Go,
;; Grunt, Haskell, HTML, HTTP, io.js, JavaScript, jQuery, jQuery
;; Mobile, jQuery UI, Knockout.js, Laravel, Less, lodash, Lua,
;; Marionette.js, Markdown, Meteor, Ruby / Minitest, mocha, Modernizr,
;; Moment.js, Mongoose, nginx, Node.js, Node.js (LTS), Nokogiri, npm,
;; OpenTSDB, Phalcon, Phaser, Phoenix, PHP, PHPUnit, PostgreSQL,
;; Python, Q, Ruby on Rails, React, React Native, Redis, Relay,
;; RequireJS, RethinkDB, Ruby, Rust, Sass, Sinon, Socket.IO, SVG,
;; Symfony, Underscore.js, Vagrant, Vue.js, webpack, XPath, Yii.

;; To bypass indicating the subject on each lookup, devdocs-lookup can
;; generate interactive commands for each of the individual subjects
;; by calling `devdocs-setup'.

;; (devdocs-setup)
;; (global-set-key (kbd "C-h C-c") #'devdocs-lookup-c)
;; (global-set-key (kbd "C-h C-p") #'devdocs-lookup-python)

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)

(defvar devdocs-base-url "https://devdocs.io"
  "Base url for devdocs.io.")

(defvar devdocs-base-index-url "https://docs.devdocs.io"
  "Base url for devdocs.io.")

(defvar devdocs-subjects
  '(("Angular.js" "angular")
    ("Apache HTTP Server" "apache_http_server")
    ("Backbone.js" "backbone")
    ("Bower" "bower")
    ("C" "c")
    ("Chai" "chai")
    ("Clojure" "clojure")
    ("CoffeeScript" "coffeescript")
    ("Cordova" "cordova")
    ("C++" "cpp")
    ("CSS" "css")
    ("D3.js" "d3")
    ("Django" "django")
    ("Dojo" "dojo")
    ("DOM" "dom")
    ("DOM Events" "dom_events")
    ("Drupal" "drupal")
    ("Elixir" "elixir")
    ("Ember.js" "ember")
    ("Express" "express")
    ("Flow" "flow")
    ("Git" "git")
    ("Go" "go")
    ("Grunt" "grunt")
    ("Haskell" "haskell")
    ("HTML" "html")
    ("HTTP" "http")
    ("io.js" "iojs")
    ("JavaScript" "javascript")
    ("jQuery" "jquery")
    ("jQuery Mobile" "jquerymobile")
    ("jQuery UI" "jqueryui")
    ("Knockout.js" "knockout")
    ("Laravel" "laravel")
    ("Less" "less")
    ("lodash" "lodash")
    ("Lua" "lua")
    ("Marionette.js" "marionette")
    ("Markdown" "markdown")
    ("Meteor" "meteor")
    ("Ruby / Minitest" "minitest")
    ("mocha" "mocha")
    ("Modernizr" "modernizr")
    ("Moment.js" "moment")
    ("Mongoose" "mongoose")
    ("nginx" "nginx")
    ("Nim" "nim")
    ("Node.js" "node")
    ("Node.js (LTS)" "node_lts")
    ("Nokogiri" "nokogiri")
    ("npm" "npm")
    ("OpenTSDB" "opentsdb")
    ("Phalcon" "phalcon")
    ("Phaser" "phaser")
    ("Phoenix" "phoenix")
    ("PHP" "php")
    ("PHPUnit" "phpunit")
    ("PostgreSQL" "postgresql")
    ("Python" "python")
    ("Python 2" "python2")
    ("Q" "q")
    ("Ruby on Rails" "rails")
    ("React" "react")
    ("React Native" "react_native")
    ("Redis" "redis")
    ("Relay" "relay")
    ("RequireJS" "requirejs")
    ("RethinkDB" "rethinkdb")
    ("Ruby" "ruby")
    ("Rust" "rust")
    ("Sass" "sass")
    ("Sinon" "sinon")
    ("Socket.IO" "socketio")
    ("SVG" "svg")
    ("Symfony" "symfony")
    ("Underscore.js" "underscore")
    ("Vagrant" "vagrant")
    ("Vue.js" "vue")
    ("webpack" "webpack")
    ("XPath" "xpath")
    ("Yii" "yii")
    ("Yii 1" "yii1"))
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
         (subject (completing-read "Subject: " subjects nil t nil hist)))
    (cadr (assoc subject devdocs-subjects))))

(defun devdocs-read-entry (subject)
  "Interactively ask the user for an entry in SUBJECT."
  (let ((names (mapcar #'car (devdocs-entries subject)))
        (hist (intern (format "devdocs--hist-%s" subject))))
    (unless (boundp hist)
      (set hist nil))
    (completing-read "Entry: " names nil :match nil hist)))

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
