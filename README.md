# ApathyDrive

To start your new Phoenix application you have to:

1. Install dependencies with `mix deps.get`
2. Start Phoenix router with `mix phoenix.start`

Now you can visit `localhost:4000` from your browser.


## Notes

* If you choose to change the application's structure, you could manually start the router from your code like this `ApathyDrive.Router.start`

# skill export for tests
# pg_dump --table=skills --data-only --dbname=apathy_drive -Fc > test/data/skills.dump
# pg_restore --dbname=apathy_drive_test test/data/skills.dump

# world data backup / restore
# pg_dump --table=abilities --table=hints --table=monster_templates --table=rooms --table=skills --data-only --dbname=apathy_drive -Fc > data.dump
# pg_restore --dbname=apathy_drive data.dump
