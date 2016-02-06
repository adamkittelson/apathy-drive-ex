# ApathyDrive

## Notes

## local world data backup / restore
* pg_dump --table=abilities --table=class_abilities --table=classes --table=item_drops --table=items --table=lair_monsters --table=monster_abilities --table=monster_templates --table=rooms --table=scripts --data-only --dbname=apathy_drive -Fc > priv/data.dump
* pg_restore --dbname=apathy_drive -U apathy_drive -W -h localhost priv/data.dump

## Setting up the build server
* docker-machine create --driver virtualbox apathy-drive-build
* eval "$(docker-machine env apathy-drive-build)"
* word on the street is `docker rmi $(docker images --filter "dangling=true" -q --no-trunc)` might help if docker is being lame and running out of space

## deploy
* cap production deploy

## misc db commands
* create db: `cap production db:create`
* run migrations: `cap production db:migrate`
* drop / restore world data: `cap production db:reload`
