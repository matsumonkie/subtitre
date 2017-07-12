namespace :walle do

  WDIR = "wall-e"

  desc 'update & build wall-e'
  task :ubuild do
    on roles(:app) do
      within "#{deploy_to}" do
        #execute :rm, "-fr", WDIR
        #execute :mkdir, "-p", WDIR
        within "./repo" do
          execute :git, :archive, :master, WDIR, "|", "/usr/bin/env", "tar", "-x", "--strip-components", "1", "-f", "-", "-C", "~deploy/subtitre/wall-e/"
        end
        within WDIR do
          execute :stack, :build
        end
      end
    end
  end
end
