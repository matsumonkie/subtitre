class Subtitle

  extend Enumerize
  include ActiveModel::Model

  attr_accessor :file
  enumerize :mode, in: [:easy, :normal, :hard], default: :easy

  def with_temp_file
    begin
      yield file.tempfile
    ensure
      file.tempfile.close(true)
    end
  end
end
