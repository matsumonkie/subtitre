class Subtitle

  extend Enumerize
  extend ActiveModel::Naming
  include ActiveModel::Model

  attr_accessor :file, :id

  enumerize :level, in: [:easy, :normal, :hard],
            default: :hard,
            i18n_scope: 'level'
  enumerize :translate_to, in: Language.keys,
            default: :fr,
            i18n_scope: 'lang'
  enumerize :translate_from, in: [ :fr,
                                   :en
                                 ],
            default: :en,
            i18n_scope: 'lang'

  def initialize *args
    super
    self.id = rand(10**20)
    self.translate_to = Subtitle.translate_to.default_value if ! self.translate_to
    self.level = Subtitle.level.default_value if ! self.level
  end

  def filename
    file.original_filename
  end

  def orig_filepath
    file.tempfile.path
  end

  def subtitle_channel
    [ "subtitle",
      translate_from,
      translate_to,
      id
    ].join(":")
  end

  def subtitled_channel
    [ "subtitled",
      id
    ].join(":")
  end
end
