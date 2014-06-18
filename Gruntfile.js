module.exports = function(grunt) {
  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),

    coffee: {
      complie: {
        files: {
          'public/js/game.js': 'assets/game.coffee'
        }
      }
    },

    sass: {
      dist: {
        files: {
          'public/css/game.css': 'assets/game.scss'
        }
      }
    }

  });

  grunt.loadNpmTasks('grunt-contrib-coffee');
  grunt.loadNpmTasks('grunt-contrib-sass');
};
