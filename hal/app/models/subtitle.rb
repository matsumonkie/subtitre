class Subtitle
  include ActiveModel::Model

  attr_accessor :file

  def with_temp_file
    begin
      yield file.tempfile
    ensure
      file.tempfile.close(true)
    end
  end
end
