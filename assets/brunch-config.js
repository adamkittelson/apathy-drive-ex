exports.config = {
  // See http://brunch.io/#documentation for docs.
  files: {
    javascripts: {
      joinTo: {
        'js/admin.js': /admin\/*/,
        'js/app.js': /(app|deps\/phoenix)\/*/
      },
      order: {
        before: [
          'web/static/js/app/jquery-1.10.2.min.js'
        ]
      }
    },
    stylesheets: {
      joinTo: {
        'css/admin.css': /admin\/*/,
        'css/app.css': /app\/*/
      },
      order: {
        before: [
          'web/static/css/app/normalize.css',
          'web/static/css/app/skeleton.css'
        ]
      }
    },
    templates: {
      joinTo: 'js/app.js'
    }
  },

  conventions: {
    // This option sets where we should place non-css and non-js assets in.
    // By default, we set this to "/assets/static". Files in this directory
    // will be copied to `paths.public`, which is "priv/static" by default.
    assets: /^(static)/
  },

  // Phoenix paths configuration
  paths: {
    // Which directories to watch
    watched: ["static", "css", "js", "vendor"],

    // Where to compile files to
    public: "../priv/static"
  },

  // Configure your plugins
  plugins: {
    babel: {
      // Do not use ES6 compiler in vendor code
      ignore: [/vendor/]
    }
  },

  modules: {
    autoRequire: {
      "js/app.js": ["js/app"]
    }
  },

  npm: {
    enabled: true
  }
};
