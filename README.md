# Emacs devdocs-lookup

This package provides an interactive function `devdocs-lookup` to
quickly jump to documentation on a particular API at
[devdocs.io](http://devdocs.io/) with your browser. It works similarly
to [javadoc-lookup](https://github.com/skeeto/javadoc-lookup), using
Ido to make a fuzzy match with a particular entry.

The currently supported "subjects" are the same as devdocs.io:
Angular.js, Backbone.js, CoffeeScript, C++, C, D3, Ember.jg, Git,
HTTP, jQuery, Knockout.js, Less, Lo-Dash, Moment.js, Node.js, PHP,
PostgreSQL, Python, Ruby, Redis, SASS, Underscore.js, and YII.

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
