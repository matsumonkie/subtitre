class CreateWordreferences < ActiveRecord::Migration[5.1]
  def change
    create_table :wordreferences do |t|
      t.string :from
      t.string :to
      t.string :word
      t.json :response

      t.timestamps
    end
  end
end
