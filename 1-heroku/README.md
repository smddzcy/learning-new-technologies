# Heroku

Heroku is a cloud platform as a service (PaaS) that lets you build, deliver, monitor, and scale apps.

## Steps

### Initial

- Create an account (free 550 dyno hours/month), enter a credit card to get 450 more dyno hours/month.

- Install Heroku CLI: `brew install heroku` and test it with `heroku --version`

- Login from CLI: `heroku login`

- Clone the getting started repo for Ruby: `git clone https://github.com/heroku/ruby-getting-started.git && d ruby-getting-started`


### Deployment

- Create an Heroku app: `heroku create` - Heroku automatically creates a new remote on the git repository called `heroku`.

- Push the code: `git push heroku master`

- Visit the deployed app: `heroku open`

### More

- See a stream of logs from the dyno: `heroku logs --tail`

- `Procfile` contains what command should be executed to start the app. Example:
```
web: bundle exec puma -C config/puma.rb
worker: bundle exec sidekiq
clock: bundle exec clockwork lib/clock.rb
```

- Scale the dynos (for web, worker or clock separately): `heroku ps:scale web=1`

- Run the app locally: `heroku local`

- Add a provisioning addon: `heroku addons:create papertrail` - See its output: `heroku addons:open papertrail`

- Run commands on the dyno: `heroku run bash` `heroku run rails console`

- Set environment varibles: `heroku config:set TIMES=10`

---

Reference: [Getting Started on Heroku](https://devcenter.heroku.com/start)
