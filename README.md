# Emacs devdocs-lookup

This package provides an interactive function `devdocs-lookup` to
quickly jump to documentation on a particular API at
[devdocs.io](http://devdocs.io/) with your browser. It works similarly
to [javadoc-lookup](https://github.com/skeeto/javadoc-lookup), using
your locally-configured `completing-read` to select an entry.

The currently supported "subjects" are the same as devdocs.io:
Angular.js, Apache HTTP Server, Backbone.js, Bower, C, Chai, Clojure,
CoffeeScript, Cordova, C++, CSS, D3.js, Django, Dojo, DOM, DOM Events,
Drupal, Elixir, Ember.js, Express, Flow, Git, Go, Grunt, Haskell,
HTML, HTTP, io.js, JavaScript, jQuery, jQuery Mobile, jQuery UI,
Knockout.js, Laravel, Less, lodash, Lua, Marionette.js, Markdown,
Meteor, Ruby / Minitest, mocha, Modernizr, Moment.js, Mongoose, nginx,
Node.js, Node.js (LTS), Nokogiri, npm, OpenTSDB, Phalcon, Phaser,
Phoenix, PHP, PHPUnit, PostgreSQL, Python, Q, Ruby on Rails, React,
React Native, Redis, Relay, RequireJS, RethinkDB, Ruby, Rust, Sass,
Sinon, Socket.IO, SVG, Symfony, Underscore.js, Vagrant, Vue.js,
webpack, XPath, Yii.

## Optional Configuration

To bypass indicating the subject on each lookup, devdocs-lookup can
generate interactive commands for each of the individual subjects by
calling `devdocs-setup`. Your Emacs initialization file might contain
the following snippet.

~~~el
(devdocs-setup)
(global-set-key (kbd "C-h C-c") #'devdocs-lookup-c)
(global-set-key (kbd "C-h C-p") #'devdocs-lookup-python)
(global-set-key (kbd "C-h C-=") #'devdocs-lookup-cpp)
~~~
