class Language

  ALL = {
    ar: :arabic,
    cz: :czech,
    en: :english,
    es: :spanish,
    fr: :french,
    gr: :greek,
    it: :italian,
    ja: :japanese,
    ko: :korean,
    po: :polish,
    pt: :portuguese,
    ro: :romanian,
    tr: :turkish,
    zh: :chinese,
  }

  class << self
    def all
      ALL
    end

    def keys
      all.keys
    end

    def values
      all.values
    end

    def valid? key
      keys.include? key
    end
  end
end
