module.exports = function(grunt) {
  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),

    coffee: {
      complie: {
        files: {
          'public/js/app.js': 'assets/app.coffee'
        }
      }
    },

    sass: {
      dist: {
        files: {
          'public/css/main.css': 'assets/main.scss'
        }
      }
    }

  });

  grunt.loadNpmTasks('grunt-contrib-coffee');
  grunt.loadNpmTasks('grunt-contrib-sass');
};
