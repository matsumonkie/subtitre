class HomeController < ApplicationController

  def index
    set_meta_tags title: 'Translate your subtitles',
                  description: 'Translate your movie\'s subtitles. Translations are available from english and french to many languages.'
    @subtitle = Subtitle.new(translate_from: cookies[:prefered_orig_language],
                             translate_to:   cookies[:prefered_dest_language],
                             level:          cookies[:prefered_level])
  end
end
