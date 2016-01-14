# config valid only for current version of Capistrano
lock '3.4.0'

set :application, 'apathy_drive'
set :repo_url, 'git@example.com:me/my_repo.git'
set :deploy_to, '/app'
set :default_env, { 
  'RELEASE_CONFIG_FILE' => '/home/deploy/apathy_drive.conf',
  'VMARGS_PATH' => '/home/deploy/vm.args'
}

# Default branch is :master
# ask :branch, `git rev-parse --abbrev-ref HEAD`.chomp

# Default deploy_to directory is /var/www/my_app_name
# set :deploy_to, '/var/www/my_app_name'

# Default value for :scm is :git
# set :scm, :git

# Default value for :format is :pretty
# set :format, :pretty

# Default value for :log_level is :debug
# set :log_level, :debug

# Default value for :pty is false
# set :pty, true

# Default value for :linked_files is []
# set :linked_files, fetch(:linked_files, []).push('config/database.yml', 'config/secrets.yml')

# Default value for linked_dirs is []
# set :linked_dirs, fetch(:linked_dirs, []).push('log', 'tmp/pids', 'tmp/cache', 'tmp/sockets', 'vendor/bundle', 'public/system')

# Default value for default_env is {}
# set :default_env, { path: "/opt/ruby/bin:$PATH" }

# Default value for keep_releases is 5
# set :keep_releases, 5

role :apathy_drive, ["apotheos.is"]
Rake::Task["deploy"].clear_actions
task :deploy do
  invoke "deploy:build"
  run_locally do
    last_release = capture("ls rel/apathy_drive/releases").split("\n").select {|f| f =~ /\d+\.\d+\.\d+/}.last
    execute :scp, "rel/apathy_drive/releases/#{last_release}/apathy_drive.tar.gz", "apotheos.is:/home/deploy"
  end
  on roles(:apathy_drive) do |host|
    execute :sudo, "stop", "apathy_drive"
    execute :sudo, "rm", "-rf", "/app"
    execute :sudo, "mkdir", "-p", "/app"
    execute :sudo, "chown", "deploy:deploy", "/app"
    execute "tar", "xfz", "/home/deploy/apathy_drive.tar.gz", "-C", "/app"
    execute :sudo, "start", "apathy_drive"
  end
end

namespace :deploy do

  desc "Perform a hot code upgrade"
  task :hot do
    invoke "deploy:build"
    last_release = nil
    run_locally do
      last_release = capture("ls rel/apathy_drive/releases").split("\n").select {|f| f =~ /\d+\.\d+\.\d+/}.last
    end
    on roles(:apathy_drive) do |host|
      execute :mkdir, "/app/releases/#{last_release}"
    end
    run_locally do
      execute :scp, "rel/apathy_drive/releases/#{last_release}/apathy_drive.tar.gz", "apotheos.is:/app/releases/#{last_release}"
    end
    on roles(:apathy_drive) do |host|
      execute "/app/bin/apathy_drive", "upgrade", last_release
    end
  end

  desc "Build release tarball"
  task :build do
    run_locally do
      `eval "$(docker-machine env apathy-drive-build)"`
      build_output = capture("docker build .")
      version  = /The release for apathy_drive-(\d+\.\d+\.\d+-\d+) is ready!/.match(build_output)[1]
      image_id = /Successfully built (\w+)/.match(build_output)[1]
      raise "build error" unless version && image_id
      container_id = capture("docker create #{image_id}")
      `docker cp #{container_id}:/usr/src/app/rel/ .`
      `docker rm -v #{container_id}`
    end
  end

end
