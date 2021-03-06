namespace :walle do

  WDIR = "wall-e"

  desc 'restart'
  task :restart_stack do
    Rake::Task["walle:kill" ].invoke
    Rake::Task["walle:build"].invoke
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
      execute :pkill, "subtitre-exe", ";true" #always exit successfully
    end
  end

  desc 'start wall-e instance'
  task :start do
    on roles(:app) do
      execute "cd #{deploy_to}/#{WDIR}; nohup stack exec ./.stack-work/install/x86_64-linux-nopie/lts-8.5/8.0.2/bin/subtitre-exe > /dev/null & sleep 5"
    end
  end
end
