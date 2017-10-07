require 'tempfile'

class SubtitreController < ApplicationController

  SUBTITRE_TIMEOUT = 20

  def create
    subtitle = Subtitle.new(subtitle_params)
    subtitre = Subtitre.new(subtitle)
    redis = Redis.new
    redis.publish(subtitle.subtitle_channel,
                  subtitle.orig_filepath)

    begin
      redis.subscribe_with_timeout(SUBTITRE_TIMEOUT,
                                   subtitle.subtitled_channel) do |on|
        on.message do |channel, message|
          set_response(subtitle, subtitre, message)
          redis.unsubscribe(channel)
        end
      end
    rescue Redis::TimeoutError
      set_response(subtitle, subtitre, "")
    ensure
      send_data(subtitre.content,
                filename: subtitre.filename)
    end
  end

  private

  def set_response subtitle, subtitre, content
    cookies[:prefered_orig_language] = subtitle.translate_from
    cookies[:prefered_dest_language] = subtitle.translate_to
    cookies[:subtitre_downloaded]    = true
    subtitre.content = content
  end

  def subtitle_params
    keys = [:file, :level, :translate_from, :translate_to]
    params.require(:subtitle).permit(keys)
  end
end
