module.exports = function(grunt) {
  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),

    coffee: {
      complie: {
        files: {
          'public/js/app.js': [ 'public/coffee/*.coffee']
        }
      }
    }
    
  });
  
  grunt.loadNpmTasks('grunt-contrib-coffee');
};
