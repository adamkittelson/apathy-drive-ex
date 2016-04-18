# config valid only for current version of Capistrano
lock '3.4.0'

set :application, 'apathy_drive'
set :deploy_to, '/app'
set :default_env, {
  'RELEASE_CONFIG_FILE' => '/home/deploy/apathy_drive.conf',
  'VMARGS_PATH' => '/home/deploy/vm.args'
}

Rake::Task["deploy"].clear_actions
task :deploy do
  invoke "deploy:build"
  run_locally do
    last_release = capture("ls rel/apathy_drive/releases").split("\n").select {|f| f =~ /\d+\.\d+\.\d+/}.last
    execute :scp, "rel/apathy_drive/releases/#{last_release}/apathy_drive.tar.gz", "apotheos.is:/tmp"
  end
  on roles(:app) do |host|
    execute :sudo, "stop", "apathy_drive" rescue nil
    execute :sudo, "rm", "-rf", fetch(:deploy_to)
    execute :sudo, "mkdir", "-p", fetch(:deploy_to)
    execute :sudo, "chown", "deploy:deploy", fetch(:deploy_to)
    execute "tar", "xfz", "/tmp/apathy_drive.tar.gz", "-C", fetch(:deploy_to)
    execute :sudo, "start", "apathy_drive"
  end
end

namespace :db do

  desc "Create DB"
  task :create do
    on roles(:app) do |host|
      execute "#{fetch(:deploy_to)}/bin/apathy_drive", "rpc", "Elixir.Ecto.Storage", "up", "\"['Elixir.ApathyDrive.Repo'].\""
    end
  end

  desc "Drop / Load Game World State"
  task :reload do
    on roles(:app) do |host|
      last_release = capture("ls #{fetch(:deploy_to)}/releases").split("\n").select {|f| f =~ /\d+\.\d+\.\d+/}.last
      execute "#{fetch(:deploy_to)}/bin/apathy_drive", "rpc", "Elixir.ApathyDrive.Repo", "drop_world!", "\"[].\""
      execute :pg_restore, "--dbname=apathy_drive", "-U apathy_drive", "-w", "-h localhost", "#{fetch(:deploy_to)}/lib/apathy_drive-#{last_release}/priv/data.dump"
    end
    invoke "deploy:restart"
  end
end

namespace :deploy do

  desc "Reset game state"
  task :reset do
    on roles(:app) do |host|
      execute :sudo, "stop", "apathy_drive"
      execute :sudo, "start", "apathy_drive", "RESET_GAME=true"
    end
  end

  desc "Restart application"
  task :restart do
    on roles(:app) do |host|
      execute :sudo, "restart", "apathy_drive"
    end
  end

  desc "Perform a hot code upgrade"
  task :hot do
    invoke "deploy:build"
    last_release = nil
    run_locally do
      last_release = capture("ls rel/apathy_drive/releases").split("\n").select {|f| f =~ /\d+\.\d+\.\d+/}.last
    end
    on roles(:app) do |host|
      execute :mkdir, "#{fetch(:deploy_to)}/releases/#{last_release}"
    end
    run_locally do
      execute :scp, "rel/apathy_drive/releases/#{last_release}/apathy_drive.tar.gz", "apotheos.is:#{fetch(:deploy_to)}/releases/#{last_release}"
    end
    on roles(:app) do |host|
      execute "#{fetch(:deploy_to)}/bin/apathy_drive", "upgrade", last_release
    end
  end

  desc "Build release tarball"
  task :build do
    run_locally do
      build_output = capture("docker build --build-arg MIX_ENV=#{fetch(:mix_env)} .")
      version  = /The release for apathy_drive-(\d+\.\d+\.\d+\+\d+) is ready!/.match(build_output)[1]
      image_id = /Successfully built (\w+)/.match(build_output)[1]
      raise "build error" unless version && image_id
      container_id = capture("docker create #{image_id}")
      `docker cp #{container_id}:/usr/src/app/rel/ .`
      `docker rm -v #{container_id}`
    end
  end

end
