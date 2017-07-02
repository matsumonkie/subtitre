class Subtitle

  extend Enumerize
  extend ActiveModel::Naming
  include ActiveModel::Model

  attr_accessor :file
  enumerize :mode, in: [:easy, :normal, :hard],
            default: :easy,
            i18n_scope: "mode"
  enumerize :translate_to, in: [:french, :spanish],
            default: :french,
            i18n_scope: "translate_to"

  def with_temp_file
    begin
      yield file.tempfile
    ensure
      file.tempfile.close(true)
    end
  end
end
