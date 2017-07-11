# config valid only for current version of Capistrano
lock "3.8.2"

set :application, "subtitre"
set :repo_url, "https://github.com/matsumonkie/subtitre.git"
set :chruby_ruby, 'ruby-2.4.1'

set :deploy_to, "~deploy/#{fetch(:application)}"
set :repo_tree, "hal"

# Default branch is :master
# ask :branch, `git rev-parse --abbrev-ref HEAD`.chomp

# Default value for :format is :airbrussh.
# set :format, :airbrussh

# You can configure the Airbrussh format using :format_options.
# These are the defaults.
# set :format_options, command_output: true, log_file: "log/capistrano.log", color: :auto, truncate: :auto

# Default value for :pty is false
# set :pty, true

# Default value for :linked_files is []
append :linked_files, "config/database.yml", "config/secrets.yml"

# Default value for linked_dirs is []
append :linked_dirs, "log", "tmp/pids", "tmp/cache", "tmp/sockets", "public/system"

#set :stage,           :production
#set :puma_bind,       "unix://#{shared_path}/tmp/sockets/#{fetch(:application)}-puma.sock"
#set :puma_state,      "#{shared_path}/tmp/pids/puma.state"
#set :puma_pid,        "#{shared_path}/tmp/pids/puma.pid"
#set :puma_access_log, "#{release_path}/log/puma.error.log"
#set :puma_error_log,  "#{release_path}/log/puma.access.log"

# path needs to be absolute, don't use shared_path var
set :puma_rackup, -> { File.join(current_path, 'config.ru') }
set :puma_state, "/home/deploy/subtitre/shared/tmp/pids/puma.state"
set :puma_pid, "/home/deploy/subtitre/shared/tmp/pids/puma.pid"
set :puma_bind, "unix:///home/deploy/subtitre/shared/tmp/sockets/#{fetch(:application)}-puma.sock"
set :puma_conf, "/home/deploy/subtitre/shared/puma.rb"
set :puma_access_log, "/home/deploy/subtitre/shared/log/puma_access.log"
set :puma_error_log, "/home/deploy/subtitre/shared/log/puma_error.log"
set :puma_role, :app
set :puma_env, fetch(:rack_env, fetch(:rails_env, 'production'))
set :puma_threads, [0, 16]
set :puma_workers, 0
set :puma_worker_timeout, nil
set :puma_init_active_record, false
set :puma_preload_app, false
set :puma_daemonize, true
set :puma_tag, fetch(:application)

# Default value for default_env is {}
# set :default_env, { path: "/opt/ruby/bin:$PATH" }

# Default value for local_user is ENV['USER']
# set :local_user, -> { `git config user.name`.chomp }

# Default value for keep_releases is 5
# set :keep_releases, 5
