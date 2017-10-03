class Subtitre

  attr_accessor :filename, :content

  def initialize subtitle
    @filename = "subtitre_#{subtitle.filename}"
  end
end
