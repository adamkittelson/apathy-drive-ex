# config valid only for current version of Capistrano
lock '3.4.0'

set :application, 'apathy_drive'
set :deploy_to, '/data/apathy_drive'
set :releases_to_keep, 5
set :default_env, {
  'RELEASE_CONFIG_FILE' => '/home/deploy/apathy_drive.conf',
  'VMARGS_PATH' => '/home/deploy/vm.args'
}

Rake::Task["deploy"].clear_actions
task :deploy do
  invoke "deploy:build"

  release = Time.now.strftime("%Y%m%d%H%M%S")

  on roles(:app) do |host|
    execute :mkdir, "-p", "#{fetch(:deploy_to)}/releases/#{release}"
  end
  run_locally do
    roles(:app).each do |host|
      execute :scp, "apathy_drive.tar.gz", "#{host.hostname}:#{fetch(:deploy_to)}/releases/#{release}/"
    end
  end
  on roles(:app) do |host|
    invoke "deploy:stop" rescue nil # raises if it isn't running, and we don't care
    execute :sudo, "rm", "-rf", "#{fetch(:deploy_to)}/app"
    execute :mkdir, "#{fetch(:deploy_to)}/app"
    execute "tar", "xfz", "#{fetch(:deploy_to)}/releases/#{release}/apathy_drive.tar.gz", "-C", "#{fetch(:deploy_to)}/app"
    execute :sudo, "rm", "-rf", "#{fetch(:deploy_to)}/app/log"
    execute :ln, "-s", "#{fetch(:deploy_to)}/shared/log", "#{fetch(:deploy_to)}/app/log"
    invoke "deploy:start"
  end

  invoke "deploy:cleanup"
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
      last_release = capture("ls #{fetch(:deploy_to)}/app/releases").split("\n").select {|f| f =~ /\d+\.\d+\.\d+/}.last
      execute :pg_restore, "--dbname=apathy_drive", "-U apathy_drive", "-w", "-h localhost", "-Ft #{fetch(:deploy_to)}/app/lib/apathy_drive-#{last_release}/priv/database.tar"
    end
    invoke "deploy:restart"
  end

  desc "Dump / download World Data"
  task :download do
    on roles(:app) do |host|
      execute :pg_dump, "--dbname=apathy_drive", "-U apathy_drive", "-w", "-h localhost", "-Ft apathy_drive > /home/deploy/database.tar"
    end
    run_locally do
      execute :scp, "apotheos.is:/home/deploy/database.tar", "priv/database.tar"
      execute :pg_restore, "--dbname=apathy_drive", "-w", "-h localhost", "-Ft priv/database.tar"
      execute :git,  "add priv/database.tar"
      execute :git, "commit",  "-m 'update data from production'"
      execute :git, "push"
    end
  end
end

namespace :deploy do

  desc "Stop the application"
  task :stop do
    on roles(:app) do |host|
      execute :sudo, "stop", "apathy_drive"
    end
  end

  desc "Start the application"
  task :start do
    on roles(:app) do |host|
      execute :sudo, "start", "apathy_drive"
    end
  end

  desc "Reset game state"
  task :reset do
    on roles(:app) do |host|
      execute :sudo, "stop", "apathy_drive" rescue nil
      execute :sudo, "start", "apathy_drive", "RESET_GAME=true"
    end
  end

  desc "Restart application"
  task :restart do
    on roles(:app) do |host|
      execute :sudo, "restart", "apathy_drive"
    end
  end

  desc "Build release tarball"
  task :build do
    run_locally do
      build_output = capture("docker build --build-arg MIX_ENV=#{fetch(:mix_env)} .")
      version  = /The release for apathy_drive-(\d+\.\d+\.\d+) is ready!/.match(build_output)[1]
      image_id = /Successfully built (\w+)/.match(build_output)[1]
      raise "build error" unless version && image_id
      container_id = capture("docker create #{image_id}")
      `docker cp #{container_id}:/usr/src/app/rel/apathy_drive/releases/#{version}/apathy_drive.tar.gz .`
      `docker rm -v #{container_id}`
    end
  end

  desc "Cleanup deployment artifacts / old releases"
  task :cleanup do
    on roles(:app) do |host|
      releases = capture("ls #{fetch(:deploy_to)}/releases/").split("\n")
      if releases.length > fetch(:releases_to_keep)
        releases_to_remove = releases.take(releases.length - fetch(:releases_to_keep))
        puts "Removing old releases: #{releases_to_remove.inspect}"
        releases_to_remove.each do |old_release|
          execute :sudo, "rm", "-rf", "#{fetch(:deploy_to)}/releases/#{old_release}"
        end
      end
    end
    run_locally do
      execute :rm, "apathy_drive.tar.gz"
    end
  end

end
