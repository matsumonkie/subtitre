# For details on the DSL available within this file, see http://guides.rubyonrails.org/routing.html
Rails.application.routes.draw do

  root to: "subtitre#index"
  get '/about', to: 'home#about'
  resources :subtitre, only: [:index, :create, :render]
end
