# ApathyDrive

## Notes

## local world data backup / restore
* pg_dump --table=abilities --table=class_abilities --table=classes --table=item_drops --table=items --table=lair_monsters --table=monster_abilities --table=monster_templates --table=rooms --table=scripts --data-only --dbname=apathy_drive -Fc > priv/data.dump
* pg_restore --dbname=apathy_drive -U apathy_drive -W -h localhost priv/data.dump

## Setting up the build server
* docker-machine create --driver virtualbox apathy-drive-build
* eval "$(docker-machine env apathy-drive-build)"

## deploy
* cap production deploy

## misc db commands
* create db: `./bin/apathy_drive rpc Elixir.Ecto.Storage up "['Elixir.ApathyDrive.Repo']."`
* run migrations: `./bin/apathy_drive rpc Elixir.Ecto.Migrator run "['Elixir.ApathyDrive.Repo', <<\"//app/lib/apathy_drive-0.0.1/priv/repo/migrations\">>, up, [{all, true}]]."`
* drop world data: `./bin/apathy_drive rpc Elixir.ApathyDrive.Repo drop_world! "[]."`
* restore world data: `pg_restore --dbname=apathy_drive -U apathy_drive -W -h localhost /app/lib/apathy_drive-x.x.x/priv/data.dump`
