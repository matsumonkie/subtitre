class CreateTranslations < ActiveRecord::Migration[5.1]
  def change
    create_table :translations do |t|
      t.string :from_lang
      t.string :to_lang
      t.string :site
      t.string :word
      t.json :response

      t.timestamps
    end
  end
end
