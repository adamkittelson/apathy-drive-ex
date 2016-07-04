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

  // Phoenix paths configuration
  paths: {
    // Which directories to watch
    watched: ["deps/phoenix/web/static",
              "deps/phoenix_html/web/static",
              //"deps/phoenix_live_reload/priv/static",
              "web/static", "test/static"],

    // Where to compile files to
    public: "priv/static"
  },

  // modules: {
  //   autoRequire: {
  //     'js/app.js': ['web/static/js/app']
  //   }
  // },

  // Configure your plugins
  plugins: {
    ES6to5: {
      // Do not use ES6 compiler in vendor code
      ignore: [/^(vendor)/]
    }
  }
};
