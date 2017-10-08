namespace :walle do

  WDIR = "wall-e"

  desc 'restart'
  task :restart do
    Rake::Task["walle:build"].invoke
    Rake::Task["walle:kill" ].invoke
    Rake::Task["walle:start"].invoke
  end

  desc 'update & build wall-e'
  task :build do
    on roles(:app) do
      within "#{deploy_to}" do
        within "./repo" do
          execute :git, :archive, :master, WDIR, "|", "/usr/bin/env", "tar", "-x", "--strip-components", "1", "-f", "-", "-C", "~deploy/subtitre/wall-e/"
        end
        within WDIR do
          execute :stack, :build
        end
      end
    end
  end

  desc 'kill former wall-e instance if it exists'
  task :kill do
    on roles(:app) do
      execute :pkill, "subtitre-exe"
    end
  end

  desc 'start wall-e instance'
  task :start do
    on roles(:app) do
      within "#{deploy_to}/#{WDIR}" do
        within WDIR do
          execute :stack, :exec, 'subtitre-exe', '&'
        end
      end
    end
  end
end
