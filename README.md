# ApathyDrive

To start your new Phoenix application you have to:

1. Install dependencies with `mix deps.get`
2. Start Phoenix router with `mix phoenix.start`

Now you can visit `localhost:4000` from your browser.


## Notes

* If you choose to change the application's structure, you could manually start the router from your code like this `ApathyDrive.Router.start`

## world data backup / restore
* pg_dump --table=abilities --table=hints --table=monster_templates --table=rooms --data-only --dbname=apathy_drive -Fc > test/data/data.dump
* pg_restore --dbname=apathy_drive data.dump


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
