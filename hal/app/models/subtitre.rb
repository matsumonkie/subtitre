class Subtitre
  include ActiveModel::Model

  attr_accessor :file, :original_filename

  def content
    file.read
  end
end
