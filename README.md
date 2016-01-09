# ApathyDrive

To start your new Phoenix application you have to:

1. Install dependencies with `mix deps.get`
2. Start Phoenix router with `mix phoenix.start`

Now you can visit `localhost:4000` from your browser.


## Notes

* If you choose to change the application's structure, you could manually start the router from your code like this `ApathyDrive.Router.start`

## world data backup / restore
* pg_dump --table=abilities --table=class_abilities --table=classes --table=item_drops --table=items --table=lair_monsters --table=monster_abilities --table=monster_templates --table=rooms --table=scripts --data-only --dbname=apathy_drive -Fc > priv/data.dump
* pg_restore --dbname=apathy_drive -U apathy_drive -W -h localhost /app/lib/apathy_drive-0.0.1/priv/data.dump


## docker setup
* docker-compose build
* docker-compose up -d
* docker-compose stop web
* docker-compose run web mix ecto.create
* docker-compose run web mix ecto.migrate
* docker-compose run web pg_restore --dbname=apathy_drive_production --host=db --user=postgres test/data/data.dump
* docker-compose up -d

## grant a user admin privileges
* docker-compose run web mix add_admin <username (case-sensitive)>

## remove a user's admin privileges
* docker-compose run web mix remove_admin <username (case-sensitive)>

##########
Setting up the build server
docker-machine create --driver virtualbox apathy-drive-build
eval "$(docker-machine env apathy-drive-build)"

docker build .

Successfully built e912a7f84cc2

id=$(docker create 7573770908e4)
docker cp $id:/usr/src/app/rel/apathy_drive/releases/0.0.1/apathy_drive.tar.gz apathy_drive.tar.gz
docker rm -v $id

scp apathy_drive.tar.gz apotheos.is:/home/deploy

ssh apotheos.is
sudo stop apathy_drive
sudo rm -rf /app
sudo mkdir -p /app
sudo chown deploy:deploy /app
cd /app
tar xfz /home/deploy/apathy_drive.tar.gz
sudo start apathy_drive
./bin/apathy_drive rpc Elixir.Ecto.Migrator run "['Elixir.ApathyDrive.Repo', <<\"//app/lib/apathy_drive-0.0.1/priv/repo/migrations\">>, up, [{all, true}]]."
./bin/apathy_drive rpc Elixir.ApathyDrive.Repo drop_world! "[]."

pg_restore from above

./bin/apathy_drive start

./bin/apathy_drive rpc Elixir.Ecto.Storage up "['Elixir.ApathyDrive.Repo']."



