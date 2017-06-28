class Subtitre
  include ActiveModel::Model

  attr_accessor :file

  def content
    file.read
  end
end
