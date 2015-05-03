exports.config = {
  // See http://brunch.io/#documentation for docs.
  files: {
    javascripts: {
      joinTo: 'js/app.js',
      order: {
        before: [
          'web/static/vendor/js/jquery-1.10.2.min.js'
        ]
      }

    },
    stylesheets: {
      joinTo: 'css/app.css',
      order: {
        before: [
          'web/static/vendor/css/normalize.css',
          'web/static/vendor/css/skeleton.css'
        ]
      }
    },
    templates: {
      joinTo: 'js/app.js'
    }
  },

  // Phoenix paths configuration
  paths: {
    // Which directories to watch
    watched: ["web/static", "test/static"],

    // Where to compile files to
    public: "priv/static"
  },

  // Configure your plugins
  plugins: {
    ES6to5: {
      // Do not use ES6 compiler in vendor code
      ignore: [/^(vendor)/]
    }
  }
};