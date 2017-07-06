class Subtitle

  extend Enumerize
  extend ActiveModel::Naming
  include ActiveModel::Model

  attr_accessor :file

  enumerize :mode, in: [:easy, :normal, :hard],
            default: :hard,
            i18n_scope: "mode"
  enumerize :translate_to, in: Language.keys,
            default: :fr,
            i18n_scope: "translate_to"

  def with_temp_file
    begin
      yield file.tempfile
    ensure
      file.tempfile.close(true)
    end
  end
end
