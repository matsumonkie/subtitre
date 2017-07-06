class Subtitle

  extend Enumerize
  extend ActiveModel::Naming
  include ActiveModel::Model

  attr_accessor :file

  enumerize :level, in: [:easy, :normal, :hard],
            default: :hard,
            i18n_scope: "level"
  enumerize :translate_to, in: Language.keys,
            default: :fr,
            i18n_scope: "translate_to"

  def initialize *args
    super
    self.translate_to = Subtitle.translate_to.default_value if ! self.translate_to
    self.level = Subtitle.level.default_value if ! self.level
  end

  def with_temp_file
    begin
      yield file.tempfile
    ensure
      file.tempfile.close(true)
    end
  end
end
