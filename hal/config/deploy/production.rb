set :user, "deploy"
server 'subtitre.com',
       user: "deploy",
       roles: %w{app db web},
       port: 8000
