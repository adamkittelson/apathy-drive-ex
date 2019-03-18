exports.config = {
  // See http://brunch.io/#documentation for docs.
  files: {
    javascripts: {
      joinTo: {
        'js/admin.js': /^admin/,
        'js/app.js': /^(?!admin)/
      }

      // To use a separate vendor.js bundle, specify two files path
      // http://brunch.io/docs/config#-files-
      // joinTo: {
      //   "js/app.js": /^js/,
      //   "js/vendor.js": /^(?!js)/
      // }
      //
      // To change the order of concatenation of files, explicitly mention here
      // order: {
      //   before: [
      //     "vendor/js/jquery-2.1.1.js",
      //     "vendor/js/bootstrap.min.js"
      //   ]
      // }
    },
    stylesheets: {
      joinTo: {
        "css/admin.css": /admin\.scss$/,
        "css/app.css": /(app\.scss)$/,
        "css/game.css": /(game\.scss)$/,
        "css/normalize.css": /(normalize\.css)$/,
        "css/skeleton.css": /(skeleton\.css)$/,
        "css/nav.css": /(nav\.scss)$/
      }
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
    // Dependencies and current project directories to watch
    watched: ["css", "js", "js/admin", "vendor", "vue"],
    // Where to compile files to
    public: "../priv/static"
  },

  // Configure your plugins
  plugins: {
    babel: {
      // Do not use ES6 compiler in vendor code
      ignore: [/vendor/]
    },
    vue: {
      extractCSS: true,
      out: '../priv/static/css/components.css'
    }
  },

  // modules: {
  //   autoRequire: {
  //     "js/app.js": ["js/app"]
  //   }
  // },

  npm: {
    enabled: true
  }
};
